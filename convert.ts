import { parse as parseVue } from '@vue/compiler-sfc'
import fs from 'node:fs/promises'
import { parse, print, visit, types } from 'recast'
import MagicString from 'magic-string'
import type { TSTypeKind, ExpressionKind, PatternKind } from 'ast-types/gen/kinds'

const b = types.builders

const assertStringLiteralOrIdentifier = (
  t: any
): t is types.namedTypes.Identifier | types.namedTypes.StringLiteral => {
  const v = types.namedTypes.Identifier.check(t) || types.namedTypes.StringLiteral.check(t)
  if (!v) throw new Error(`${v} is not identifier or string literal`)
  return v
}

const getNameFromStringLiteralOrIdentifier = (
  t: types.namedTypes.Identifier | types.namedTypes.StringLiteral
) => {
  if (types.namedTypes.Identifier.check(t)) {
    return t.name
  }
  return t.value
}

const propValueToType = (key: string, propValue: any): TSTypeKind => {
  if (types.namedTypes.TSAsExpression.check(propValue)) {
    if (
      types.namedTypes.TSTypeReference.assert(propValue.typeAnnotation) &&
      types.namedTypes.Identifier.assert(propValue.typeAnnotation.typeName) &&
      propValue.typeAnnotation.typeName.name === 'PropType' &&
      propValue.typeAnnotation.typeParameters?.params.length === 1
    ) {
      return propValue.typeAnnotation.typeParameters.params[0]!
    }
    throw new Error('Unexpected type cast')
  }
  if (types.namedTypes.ArrayExpression.check(propValue)) {
    const hasRestOrSpread = propValue.elements.some(element =>
      types.namedTypes.RestElement.check(element) || types.namedTypes.SpreadElement.check(element)
    )
    if (hasRestOrSpread) {
      throw new Error('prop type has rest or spread element')
    }
    const typeList = propValue.elements.map(element => propValueToType(key, element))
    return b.tsUnionType(typeList)
  }
  if (types.namedTypes.Identifier.check(propValue)) {
    switch(propValue.name) {
    case 'String': return b.tsStringKeyword()
    case 'Number': return b.tsNumberKeyword()
    case 'Boolean': return b.tsBooleanKeyword()
    case 'Array': return b.tsArrayType(b.tsAnyKeyword())
    case 'Object': return b.tsAnyKeyword()
    case 'Date': return b.tsTypeReference(b.identifier('Date'))
    case 'Function': return b.tsTypeReference(b.identifier('Function'))
    case 'Symbol': return b.tsSymbolKeyword()
    }
    throw new Error(`Cannot convert prop(${key}) constructor to type: ${propValue.name}`)
  }
  if (types.namedTypes.NullLiteral.check(propValue)) {
    return b.tsAnyKeyword()
  }
  throw new Error(`Cannot convert prop(${key}) to type: ${propValue}`)
}

const extractPropInfo = (props: types.namedTypes.ObjectExpression) => {
  const propTypes: types.namedTypes.TSPropertySignature[] = []
  const defaults: types.namedTypes.ObjectProperty[] = []

  for (const prop of props.properties) {
    if (
      types.namedTypes.ObjectProperty.assert(prop) &&
      assertStringLiteralOrIdentifier(prop.key)
    ) {
      const propName = getNameFromStringLiteralOrIdentifier(prop.key)
      const type = prop.value

      if (types.namedTypes.ObjectExpression.check(type)) {
        let typeAnnotation: types.namedTypes.TSTypeAnnotation | undefined
        let isRequired = false
        for (const p of type.properties) {
          if (!types.namedTypes.ObjectProperty.check(p)) continue
          if (!types.namedTypes.Identifier.check(p.key)) continue

          if (p.key.name === 'type') {
            typeAnnotation = b.tsTypeAnnotation(propValueToType(propName, p.value))
          } else if (p.key.name === 'required') {
            if (types.namedTypes.BooleanLiteral.assert(p.value) && p.value.value) {
              isRequired = true
            }
          } else if (p.key.name === 'default') {
            if (!types.namedTypes.Identifier.check(p.value) || p.value.name !== 'undefined') {
              defaults.push(b.objectProperty(prop.key, p.value))
            }
          }
        }
        if (!typeAnnotation) {
          throw new Error(`type was not found for prop(${propName})`)
        }

        const t = b.tsPropertySignature(
          prop.key,
          typeAnnotation,
          !isRequired
        )
        propTypes.push(t)
      } else {
        const t = b.tsPropertySignature(
          prop.key,
          b.tsTypeAnnotation(propValueToType(propName, type)),
          true
        )
        propTypes.push(t)
      }
    }
  }

  const propType = propTypes.length !== 0 ? b.tsTypeLiteral(propTypes) : undefined
  const propDefault = defaults.length !== 0 ? b.objectExpression(defaults) : undefined
  return [propType, propDefault] as const
}

const extractEmitInfo = (emits: types.namedTypes.ObjectExpression) => {
  const emitTypes: types.namedTypes.TSCallSignatureDeclaration[] = []

  for (const emit of emits.properties) {
    if (
      types.namedTypes.ObjectProperty.check(emit) &&
      assertStringLiteralOrIdentifier(emit.key) &&
      types.namedTypes.ArrowFunctionExpression.check(emit.value)
    ) {
      const eventNameLiteralType = b.tsLiteralType(b.stringLiteral(
        getNameFromStringLiteralOrIdentifier(emit.key)
      ))
      const eventNameParam = b.identifier.from({
        name: 'e',
        typeAnnotation: b.tsTypeAnnotation(eventNameLiteralType)
      })
      const params = [eventNameParam, ...emit.value.params]
      const returnType = b.tsTypeAnnotation(b.tsVoidKeyword())
      // @ts-expect-error should be ok
      const t = b.tsCallSignatureDeclaration(params, returnType)
      emitTypes.push(t)
    } else if (
      types.namedTypes.ObjectMethod.check(emit) &&
      assertStringLiteralOrIdentifier(emit.key)
    ) {
      const eventNameLiteralType = b.tsLiteralType(b.stringLiteral(
        getNameFromStringLiteralOrIdentifier(emit.key)
      ))
      const params = [b.tsTypeParameter('e', eventNameLiteralType), ...emit.params]
      // @ts-expect-error should be ok
      const t = b.tsCallSignatureDeclaration(params)
      emitTypes.push(t)
    } else {
      throw new Error(`Unexpected emits: ${emit}`)
    }
  }
  const emitType = emitTypes.length !== 0 ? b.tsTypeLiteral(emitTypes) : undefined
  return emitType
}

/**
 * ['foo'] => (e: 'foo', ...values: any): void
 */
const extractEmitArrayInfo = (emits: types.namedTypes.ArrayExpression) => {
  const emitTypes: types.namedTypes.TSCallSignatureDeclaration[] = []

  for (const emit of emits.elements) {
    if (types.namedTypes.StringLiteral.check(emit)) {
      const eventNameLiteralType = b.tsLiteralType(b.stringLiteral(emit.value))
      const eventNameParam = b.identifier.from({
        name: 'e',
        typeAnnotation: b.tsTypeAnnotation(eventNameLiteralType)
      })
      const eventValuesParam = b.identifier.from({
        name: '...values',
        typeAnnotation: b.tsTypeAnnotation(b.tsAnyKeyword())
      })
      const params = [eventNameParam, eventValuesParam]
      const returnType = b.tsTypeAnnotation(b.tsVoidKeyword())
      const t = b.tsCallSignatureDeclaration(params, returnType)
      emitTypes.push(t)
    } else {
      throw new Error(`Unexpected emits: ${emit}`)
    }
  }
  const emitType = emitTypes.length !== 0 ? b.tsTypeLiteral(emitTypes) : undefined
  return emitType
}

const extractInfoFromDefineComponent = (options: types.namedTypes.ObjectExpression) => {
  const components: string[] = []
  let setupBody: types.namedTypes.BlockStatement | undefined
  let propType: types.namedTypes.TSTypeLiteral | undefined
  let propDefaults: types.namedTypes.ObjectExpression | undefined
  let emitType: types.namedTypes.TSTypeLiteral | undefined

  for (const prop of options.properties) {
    if (
      types.namedTypes.ObjectProperty.check(prop) &&
      assertStringLiteralOrIdentifier(prop.key)
    ) {
      const keyName = getNameFromStringLiteralOrIdentifier(prop.key)
      if (keyName === 'name') {
        // no-op
      } else if (keyName === 'components') {
        if (types.namedTypes.ObjectExpression.assert(prop.value)) {
          for (const p of prop.value.properties) {
            if (
              types.namedTypes.ObjectProperty.assert(p) &&
              assertStringLiteralOrIdentifier(p.key)
            ) {
              components.push(getNameFromStringLiteralOrIdentifier(p.key))
            }
          }
        }
      } else if (keyName === 'props') {
        if (types.namedTypes.ObjectExpression.assert(prop.value)) {
          [propType, propDefaults] = extractPropInfo(prop.value)
        }
      } else if (keyName === 'emits') {
        if (types.namedTypes.ObjectExpression.check(prop.value)) {
          emitType = extractEmitInfo(prop.value)
        } else if (types.namedTypes.ArrayExpression.assert(prop.value)) {
          emitType = extractEmitArrayInfo(prop.value)
        }
      } else {
        throw new Error(`Unexpected prop name in object: ${keyName}`)
      }
    } else if (
      types.namedTypes.ObjectMethod.check(prop) &&
      assertStringLiteralOrIdentifier(prop.key)
    ) {
      const keyName = getNameFromStringLiteralOrIdentifier(prop.key)
      if (keyName === 'setup') {
        setupBody = prop.body
      } else {
        throw new Error(`Unexpected prop name in object: ${keyName}`)
      }
    } else {
      throw new Error(`Unexpected prop type: ${prop.type}`)
    }
  }

  return {
    components,
    setupBody,
    propType,
    propDefaults,
    emitType
  }
}

const collectAndDeleteComponentImports = (ast: any, components: string[]) => {
  const program = ast.program
  if (!types.namedTypes.Program.assert(program)) return []

  const imports: types.namedTypes.ImportDeclaration[] = []

  visit(ast, {
    visitImportDeclaration(path) {
      if (!path.node.specifiers) {
        // side effect. so do not move to script setup
        return false
      }

      const deleteIndices: number[] = []
      for (const [i, sp] of path.node.specifiers.entries()) {
        if (types.namedTypes.ImportDefaultSpecifier.check(sp)) {
          const name = sp.local?.name ?? ''
          // assumes vue component only uses 'default export'
          if (components.includes(name)) {
            const declForSetup = b.importDeclaration(
              [
                b.importDefaultSpecifier(sp.local)
              ],
              path.node.source,
              path.node.importKind
            )
            imports.push(declForSetup)
            deleteIndices.push(i)
          }
        } else if (types.namedTypes.ImportSpecifier.check(sp)) {
          // delete `defineComponent` and `PropType`
          const module = path.node.source.value
          if (module !== 'vue') continue

          const name = sp.imported.name
          if (name === 'defineComponent' || name === 'PropType') {
            deleteIndices.push(i)
          }
        }
      }
      if (deleteIndices.length > 0) {
        if (deleteIndices.length === path.node.specifiers.length) {
          path.prune()
        } else {
          for (const [i, v] of deleteIndices.entries()) {
            path.node.specifiers.splice(v - i, 1)
          }
          path.replace(path.node)
        }
      }
      return false
    }
  })

  const isAllImportDecls = program.body.every(
    statement => types.namedTypes.ImportDeclaration.check(statement)
  )
  if (isAllImportDecls) {
    // move all if there is only import decls
    const all = [...imports, ...program.body] as types.namedTypes.ImportDeclaration[]
    program.body = []
    return all
  }

  return imports
}

type Alias = [string, ExpressionKind | PatternKind]

const collectReturnFromSetup = (
  outputWarning: (msg: string) => void,
  body: types.namedTypes.BlockStatement
) => {
  // there should not have conditions inside setup function
  const returnIndex = body.body.findIndex(
    statement => types.namedTypes.ReturnStatement.check(statement)
  )

  const returnStatement = body.body[returnIndex]! as types.namedTypes.ReturnStatement

  body.body.splice(returnIndex, 1)

  if (!returnStatement.argument) return []

  const aliases: Alias[] = []
  if (types.namedTypes.ObjectExpression.assert(returnStatement.argument)) {
    for (const prop of returnStatement.argument.properties) {
      if (types.namedTypes.SpreadElement.check(prop) || types.namedTypes.SpreadProperty.check(prop)) {
        outputWarning('  Spread operator is used in return statement of setup function. Should manually edit.')
        continue
      }
      if (types.namedTypes.ObjectMethod.check(prop)) {
        outputWarning('  Object method is used in return statement of setup function. Should manually edit.')
        continue
      }
      if (types.namedTypes.ObjectProperty.check(prop) || types.namedTypes.Property.check(prop)) {
        if (!assertStringLiteralOrIdentifier(prop.key)) {
          outputWarning('  Dynamic key is used in return statement of setup function. Should manually edit.')
          continue
        }
        if (prop.shorthand) continue

        const keyName = getNameFromStringLiteralOrIdentifier(prop.key)
        if (types.namedTypes.StringLiteral.check(prop.value) || types.namedTypes.Identifier.check(prop.value)) {
          const valueName = getNameFromStringLiteralOrIdentifier(prop.value)
          if (keyName === valueName) continue
        }

        aliases.push([keyName, prop.value])
        continue
      }
      outputWarning('  Not supported syntax is used in return statement of setup function. Should manually edit.')
    }
  }

  return aliases
}

const createConstDeclarationIfNeeded = (doDecl: boolean, ident: string, init: any) => {
  if (!doDecl) return init
  return createConstDeclaration(ident, init)
}

const createConstDeclaration = (ident: string, init: any) => {
  return b.variableDeclaration('const', [
    b.variableDeclarator(b.identifier(ident), init)
  ])
}

const createDefineProps = (
  declareProps: boolean,
  propType: types.namedTypes.TSTypeLiteral,
  propDefaults: types.namedTypes.ObjectExpression | undefined
) => {
  const typeParam = b.tsTypeParameterInstantiation([propType])
  const defineProps = b.callExpression.from({
    callee: b.identifier('defineProps'),
    arguments: []
  })
  // @ts-expect-error https://github.com/benjamn/ast-types/pull/358
  defineProps.typeParameters = typeParam

  if (!propDefaults) {
    return createConstDeclarationIfNeeded(
      declareProps,
      'props',
      defineProps
    )
  }

  return createConstDeclarationIfNeeded(
    declareProps,
    'props',
    b.callExpression(
      b.identifier('withDefaults'),
      [defineProps, propDefaults]
    )
  )
}

const createDefineEmits = (emitType: types.namedTypes.TSTypeLiteral) => {
  const typeParam = b.tsTypeParameterInstantiation([emitType])
  const defineEmits = b.callExpression.from({
    callee: b.identifier('defineEmits'),
    arguments: []
  })
  // @ts-expect-error https://github.com/benjamn/ast-types/pull/358
  defineEmits.typeParameters = typeParam

  return createConstDeclaration('emit', defineEmits)
}

const checkPropsIsUsed = (setupBody: types.namedTypes.BlockStatement | undefined) => {
  if (!setupBody) return false

  const propDecl = b.variableDeclaration('const', [
    b.variableDeclarator(b.identifier('props'))
  ])

  let isUsed = false
  visit(b.program([propDecl, ...setupBody.body]), {
    visitIdentifier(path) {
      if (types.namedTypes.VariableDeclarator.check(path.parentPath.node)) return false
      if (path.node.name !== 'props') return false
      const s = path.scope.lookup('props')
      if (s.isGlobal) {
        // top level props
        isUsed = true
      }
      return false
    }
  })
  return isUsed
}

export const convert = async (path: string): Promise<string[]> => {
  const warnings: string[] = []
  const outputWarning = (message: string) => {
    console.warn(`  ${message}`)
    warnings.push(message)
  }

  const content = await fs.readFile(path, 'utf-8')
  const { descriptor, errors } = parseVue(content)
  if (errors.length > 0) {
    throw new Error(errors.join('\n'))
  }

  if (descriptor.scriptSetup) {
    return warnings
  }

  const script = descriptor.script
  if (!script) {
    return warnings
  }
  if (script.lang !== 'ts') {
    outputWarning('  Does not support lang other than ts skipping')
    return warnings
  }

  let components: string[] = []
  let setupBody: types.namedTypes.BlockStatement | undefined
  let propType: types.namedTypes.TSTypeLiteral | undefined
  let propDefaults: types.namedTypes.ObjectExpression | undefined
  let emitType: types.namedTypes.TSTypeLiteral | undefined

  const ast = parse(script.content, {
    parser: require('recast/parsers/typescript')
  })
  visit(ast, {
    visitExportDefaultDeclaration(path) {
      if (!(
        types.namedTypes.CallExpression.assert(path.node.declaration) &&
        types.namedTypes.Identifier.assert(path.node.declaration.callee) &&
        path.node.declaration.callee.name === 'defineComponent' &&
        types.namedTypes.ObjectExpression.assert(path.node.declaration.arguments[0])
      )) return false

      const options = path.node.declaration.arguments[0]
      const result = extractInfoFromDefineComponent(options)
      components = result.components
      setupBody = result.setupBody
      propType = result.propType
      propDefaults = result.propDefaults
      emitType = result.emitType

      path.prune()
      return false
    }
  })

  const imports = collectAndDeleteComponentImports(ast, components)

  let aliasesCode: types.namedTypes.VariableDeclaration[] | undefined
  if (setupBody) {
    const aliases = collectReturnFromSetup(outputWarning, setupBody)
    if (aliases.length > 0) {
      aliasesCode = aliases.map(([name, value]) => createConstDeclaration(name, value))
    }
  }

  let setupContent = print(b.program(imports)).code
  if (propType) {
    const isPropsUsed = checkPropsIsUsed(setupBody)

    setupContent += '\n\n'
    setupContent += print(createDefineProps(isPropsUsed, propType, propDefaults)).code
  }
  if (emitType) {
    setupContent += '\n\n'
    setupContent += print(createDefineEmits(emitType)).code
  }
  if (setupBody) {
    setupContent += '\n\n'
    setupContent += print(b.program(setupBody.body)).code
  }
  if (aliasesCode) {
    setupContent += '\n\n'
    setupContent += print(b.program(aliasesCode)).code
  }

  const newScriptContent = print(ast).code

  const scriptTagStart = content.lastIndexOf('<script', script.loc.start.offset)
  const scriptTagEnd = content.indexOf('</script>', script.loc.end.offset) + '</script>'.length

  const ms = new MagicString(content)
  if (newScriptContent.trim() === '') {
    ms.remove(scriptTagStart, scriptTagEnd)
  } else {
    ms.overwrite(script.loc.start.offset, script.loc.end.offset, newScriptContent)
    ms.appendRight(scriptTagEnd, '\n\n')
  }
  if (setupContent.trim() !== '') {
    ms.appendRight(scriptTagEnd, `<script lang="ts" setup>\n${setupContent}\n</script>`)
  }
  const newContent = ms.toString()

  await fs.writeFile(path, newContent, 'utf-8')

  return warnings
}

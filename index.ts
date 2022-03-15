import klaw from 'klaw'
import path from 'node:path'
import { convert } from './convert'

const root = process.cwd()
const ignores = [
  '.git',
  'node_modules',
  'dist',
  'coverage'
]

const toRelative = (p: string) => path.relative(root, p)

const main = async () => {
  const list = klaw(root, {
    filter: p => !ignores.includes(path.basename(p)),
    depthLimit: -1
  })

  const warningList: Array<{ file: string, messages: string[] }> = []

  for await (const file of list) {
    if (file.stats.isDirectory()) continue
    if (path.extname(file.path) !== '.vue') continue

    console.log(`Processing: ${toRelative(file.path)}`)

    const warnings = await convert(file.path)
    if (warnings.length > 0) {
      warningList.push({
        file: file.path,
        messages: warnings
      })
    }
  }

  console.log('\n\nFinished')
  if (warningList.length > 0) {
    console.log('Warnings:')
    for (const w of warningList) {
      console.log(`  ${toRelative(w.file)}`)
      for (const m of w.messages) {
        console.log(`    ${m}`)
      }
    }
  }
}

main()

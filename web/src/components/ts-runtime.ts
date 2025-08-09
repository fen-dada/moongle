import { Parser, Language, Query } from 'web-tree-sitter'

const RUNTIME_WASM_URL = '/tree-sitter.wasm'
const MOONBIT_WASM_URL = '/tree-sitter-moonbit.wasm'
let initP: Promise<void> | null = null
let langP: Promise<any> | null = null
let queryP: Promise<Query> | null = null

export async function getMoonbitWithQuery(highlightsSource: string) {
  if (!initP) initP = Parser.init({ locateFile: () => RUNTIME_WASM_URL })
  await initP

  if (!langP) langP = Language.load(MOONBIT_WASM_URL)
  const language = await langP

  if (!queryP) queryP = Promise.resolve(new Query(language, highlightsSource))
  const query = await queryP

  const parser = new Parser()
  parser.setLanguage(language)

  return { parser, language, query }
}


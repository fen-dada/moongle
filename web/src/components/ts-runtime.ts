// 单例初始化，返回 { parser, language }
import { Parser, Language } from 'web-tree-sitter'

const RUNTIME_WASM_URL = '/tree-sitter.wasm'
const MOONBIT_WASM_URL = '/tree-sitter-moonbit.wasm'

let initPromise: Promise<void> | null = null
let langPromise: Promise<any> | null = null

export async function getMoonbit() {
  if (!initPromise) {
    initPromise = Parser.init({ locateFile: () => RUNTIME_WASM_URL })
  }
  await initPromise

  if (!langPromise) {
    langPromise = Language.load(MOONBIT_WASM_URL)
  }
  const language = await langPromise

  const parser = new Parser()
  parser.setLanguage(language)

  return { parser, language }
}


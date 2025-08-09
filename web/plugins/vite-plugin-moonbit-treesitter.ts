import { Plugin, ViteDevServer } from 'vite'
import { exec as _exec } from 'node:child_process'
import { promisify } from 'node:util'
import { existsSync, mkdirSync, copyFileSync, statSync, readdirSync } from 'node:fs'
import { resolve, dirname, join } from 'node:path'

const exec = promisify(_exec)

export interface MoonbitTsOptions {
  repoPath?: string               // 相对 Vite root，例如 'vendor/tree-sitter-moonbit'
  outName?: string                // 输出 wasm 文件名，默认 'tree-sitter-moonbit.wasm'
  publicDir?: string              // 不填用 Vite 的 publicDir
  watch?: boolean                 // dev 监听源文件
  cliVersion?: string             // tree-sitter-cli 版本（与 web-tree-sitter ABI 一致）
  debounceMs?: number             // 监听防抖
  devSkipBuild?: boolean          // dev 是否跳过“按需编译”（默认 false=按需编）
  forceBuildOnProd?: boolean      // 生产是否强制重编（默认 false=按需）
}

export default function moonbitTreeSitterPlugin(opts: MoonbitTsOptions = {}): Plugin {
  const {
    repoPath = 'vendor/tree-sitter-moonbit',
    outName = 'tree-sitter-moonbit.wasm',
    watch = true,
    cliVersion = '0.25.8',
    debounceMs = 120,
    devSkipBuild = false,       // 默认：dev 也按需编（如果你想 dev 永不编，设为 true）
    forceBuildOnProd = false,   // 默认：prod 也按需编；想每次都编就设 true
  } = opts

  let viteRoot = ''
  let resolvedPublicDir = opts.publicDir as string | undefined
  let server: ViteDevServer | undefined
  let debounceTimer: NodeJS.Timeout | null = null
  let building = false
  let isDev = false

  const repoAbs = (root: string) => resolve(root, repoPath)
  const wasmAbsInRepo = (root: string) => resolve(repoAbs(root), outName)
  const wasmAbsInPublic = (root: string) => resolve(resolvedPublicDir || resolve(root, 'public'), outName)

  const log = (m: string) => console.log(`\x1b[36m[moonbit]\x1b[0m ${m}`)
  const run = async (cmd: string, cwd: string) => {
    log(`$ ${cmd}  (cwd: ${cwd})`)
    const { stdout, stderr } = await exec(cmd, { cwd })
    if (stdout?.trim()) process.stdout.write(stdout)
    if (stderr?.trim()) process.stderr.write(stderr)
  }

  const ensureCli = async () => {
    try { await exec('npx --yes tree-sitter --version') }
    catch {
      throw new Error(
        `未检测到 tree-sitter-cli。请先安装：\n` +
        `  npm i -D tree-sitter-cli@${cliVersion}\n` +
        `或  pnpm add -D tree-sitter-cli@${cliVersion}`
      )
    }
  }

  // 递归收集源码文件（只取：grammar.js / queries/*.scm / src/**/*.js）
  const collectSourceFiles = (dir: string): string[] => {
    const res: string[] = []
    const qDir = join(dir, 'queries')
    const sDir = join(dir, 'src')
    const gFile = join(dir, 'grammar.js')
    if (existsSync(gFile)) res.push(gFile)

    const walk = (d: string, exts: string[]) => {
      if (!existsSync(d)) return
      const stack = [d]
      while (stack.length) {
        const cur = stack.pop()!
        for (const name of readdirSync(cur, { withFileTypes: true })) {
          const p = join(cur, name.name)
          if (name.isDirectory()) {
            // 忽略 node_modules、.git、生成物目录
            if (name.name === 'node_modules' || name.name === '.git') continue
            if (name.name === 'build' || name.name === 'dist') continue
            stack.push(p)
          } else {
            const ext = p.split('.').pop() || ''
            if (exts.includes(ext)) res.push(p)
          }
        }
      }
    }
    walk(qDir, ['scm'])
    walk(sDir, ['js'])
    return res
  }

  const getMtime = (p: string) => {
    try { return statSync(p).mtimeMs } catch { return 0 }
  }

  /** 判断是否需要重编：wasm 不存在或任一源码 mtime > wasm mtime */
  const isStale = (root: string): { stale: boolean; reason: string } => {
    const repo = repoAbs(root)
    const wasm = wasmAbsInRepo(root)
    if (!existsSync(wasm)) return { stale: true, reason: 'wasm 不存在' }
    const wasmTime = getMtime(wasm)

    const files = collectSourceFiles(repo)
    for (const f of files) {
      if (getMtime(f) > wasmTime) {
        return { stale: true, reason: `源码较新: ${f}` }
      }
    }
    return { stale: false, reason: 'wasm 已最新' }
  }

  // 放在插件内部其它函数旁
  const ensurePublicWasmPresent = async (root: string) => {
    const repoWasm = wasmAbsInRepo(root)
    const publicWasm = wasmAbsInPublic(root)

    // public 缺失 → 先尝试用 repo 的（如果 repo 存在且新），否则 build
    if (!existsSync(publicWasm)) {
      const staleInfo = isStale(root) // 用你已有的陈新判断（基于 repo 源码/版本）
      if (existsSync(repoWasm) && !staleInfo.stale) {
        mkdirSync(dirname(publicWasm), { recursive: true })
        copyFileSync(repoWasm, publicWasm)
        log(`public 缺失，已从 repo 复制 wasm → ${publicWasm}`)
        return
      }
      // repo 没有或已过期 → 编译并复制
      await buildOnce(root)
      return
    }
  }


  /** 忽略 .wasm 产物，避免自触发 */
  const isWasmArtifact = (p: string, repo: string) =>
    p.startsWith(repo) && (p.endsWith('.wasm') || p.endsWith('/src/output.wasm') || p.endsWith(`/${outName}`))

  /** 是否我们关心的源文件 */
  const isSourceFile = (p: string, repo: string) =>
    p.startsWith(repo) && (
      p.endsWith('grammar.js') ||
      (p.includes('/queries/') && p.endsWith('.scm')) ||
      (p.includes('/src/') && p.endsWith('.js'))
    )

  /** 真正执行一次编译 + 拷贝 */
  const buildOnce = async (root: string) => {
    await ensureCli()
    const repo = repoAbs(root)
    if (!existsSync(repo)) throw new Error(`未找到语法仓库：${repo}（请确认 submodule）`)
    await run('npx tree-sitter build --wasm', repo)
    const src = wasmAbsInRepo(root)
    const dst = wasmAbsInPublic(root)
    mkdirSync(dirname(dst), { recursive: true })
    if (!existsSync(src)) throw new Error(`未生成 wasm：${src}。检查 grammar 与 CLI 版本/ABI。`)
    copyFileSync(src, dst)
    log(`wasm 已复制到：${dst}`)
  }

  /** 按需构建（如果缺失或过期才编），支持强制 */
  const ensureFreshWasm = async (root: string, force = false) => {
    if (building) return
    const need = force ? { stale: true, reason: 'force' } : isStale(root)
    if (!need.stale) { log(`跳过构建：${need.reason}`); return }
    building = true
    try {
      log(`按需构建：${need.reason}`)
      await buildOnce(root)
      server?.ws.send({ type: 'full-reload' })
    } finally {
      building = false
    }
  }

  // dev 监听触发（按需）
  const scheduleRebuild = (root: string) => {
    if (!watch) return
    if (debounceTimer) clearTimeout(debounceTimer)
    debounceTimer = setTimeout(() => ensureFreshWasm(root, false), debounceMs)
  }

  return {
    name: 'vite-plugin-moonbit-treesitter',

    configResolved(cfg) {
      viteRoot = cfg.root || process.cwd()
      resolvedPublicDir = resolvedPublicDir || (cfg.publicDir as string) || resolve(viteRoot, 'public')
      isDev = cfg.command === 'serve'
      log(`root=${viteRoot}`)
      log(`publicDir=${resolvedPublicDir}`)
      log(`mode=${isDev ? 'dev(serve)' : 'build'}`)
    },

    configureServer(s) {
      server = s
      const repo = repoAbs(viteRoot)
      if (!watch) return

      s.watcher.add(repo)
      s.watcher.on('change', async (p) => {
        if (isWasmArtifact(p, repo)) return
        if (!isSourceFile(p, repo)) return

        // 1) 优先兜底：public 被误删/改名就补齐
        const publicWasm = wasmAbsInPublic(viteRoot)
        if (!existsSync(publicWasm)) {
          log('检测到源码变更且 public wasm 缺失，先补齐 public')
          await ensurePublicWasmPresent(viteRoot)
          return
        }

        // 2) 正常按需
        if (isDev && devSkipBuild && !process.env.MOONBIT_BUILD_WASM) {
          log(`检测到变化但跳过按需构建（devSkipBuild=true）。需要立即重建可设 MOONBIT_BUILD_WASM=1`)
          return
        }
        log(`检测到变化：${p}`)
        scheduleRebuild(viteRoot)
      })
    },


    async buildStart() {
      // dev：可选跳过；prod：按需或强制
      await ensurePublicWasmPresent(viteRoot)
      if (isDev) {
        if (devSkipBuild && !process.env.MOONBIT_BUILD_WASM) {
          log('开发模式跳过按需构建，使用 public 里的现成 wasm')
          return
        }
        await ensureFreshWasm(viteRoot, false)
      } else {
        await ensureFreshWasm(viteRoot, !!forceBuildOnProd)
      }
    }
  }
}


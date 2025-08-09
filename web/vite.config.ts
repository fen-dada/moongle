import { fileURLToPath, URL } from 'node:url'

import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'
import vueJsx from '@vitejs/plugin-vue-jsx'
import vueDevTools from 'vite-plugin-vue-devtools'

import moonbitTreeSitterPlugin from './plugins/vite-plugin-moonbit-treesitter'

// https://vite.dev/config/
export default defineConfig({
  server: {
    proxy: {
      '/api': {
        target: 'http://localhost:3000',
        changeOrigin: true,
      },
    },
  },
  plugins: [
    vue(),
    vueJsx(),
    vueDevTools(),
    moonbitTreeSitterPlugin({
      repoPath: 'vendor/tree-sitter-moonbit',   // submodule 路径
      outName: 'tree-sitter-moonbit.wasm',      // 生成的 wasm 文件名
      // publicDir: 'moongle/web/public',       // 如和默认不一致，手动指定
      cliVersion: '0.25.8',
      watch: true,
      //debounceMs: 150,
      devSkipBuild: true,
      forceBuildOnProd: false    // 生产也“按需构建”；若希望每次构建都重编，设为 true
    })
  ],
  resolve: {
    alias: {
      '@': fileURLToPath(new URL('./src', import.meta.url))
    },
  },
})

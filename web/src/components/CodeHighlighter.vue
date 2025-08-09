<template>
  <div class="moonbit-highlighter">
    <pre v-if="loading" class="loading">正在加载语法高亮...</pre>
    <pre v-else-if="error" class="error">{{ error }}</pre>
    <pre v-else class="code-container"><code v-html="highlightedCode"></code></pre>
  </div>
</template>

<script>
import { getMoonbit } from './ts-runtime'   // 按你的路径改

export default {
  name: 'Highlight',
  props: {
    code: { type: String, required: true, default: '' }
  },
  data() {
    return {
      highlightedCode: '',
      parser: null,
      language: null,
      loading: true,
      error: null
    }
  },
  async mounted() {
    await this.initializeParser()
  },
  watch: {
    code() {
      this.highlightCode()
    }
  },
  methods: {
    async initializeParser () {
      this.loading = true
      this.error = null
      try {
        const { parser, language } = await getMoonbit()   // ✅ 同时拿到 language
        this.parser = parser
        this.language = language

        this.loading = false
        this.highlightCode()
      } catch (err) {
        console.error('初始化解析器失败:', err)
        this.error = `初始化失败: ${err?.message || err}`
        this.loading = false
      }
    },

    dumpLeafTypes(node, set = new Set()) {
      if (!node) return set
      if (!node.children || node.children.length === 0) {
        set.add(node.type)
        return set
      }
      for (const ch of node.children) this.dumpLeafTypes(ch, set)
      return set
    },

    highlightCode() {
      if (this.loading || !this.parser || !this.language) return
      const src = this.code ?? ''
      if (!src) { this.highlightedCode = ''; return }

      const tree = this.parser.parse(src)
      const leaves = this.flattenLeaves(tree.rootNode, src) // ← 新方法

      let html = ''
      let cursor = 0
      for (let i = 0; i < leaves.length; i++) {
        const cur = leaves[i]
        // 补上叶子前的原文（空白/逗号等）
        if (cursor < cur.start) html += this.escapeHtml(src.slice(cursor, cur.start))

        // ---- 识别 "# deprecated"（跨层也能识别）----
        const next = leaves[i + 1]
        if (
          cur.text === '#' &&
          next &&
          next.type === 'lowercase_identifier' &&
          /^deprecated$/i.test(next.text) &&
          /^\s*$/.test(src.slice(cur.end, next.start)) // 中间允许空格
        ) {
          html += `<span class="token deprecated-anno">${this.escapeHtml(src.slice(cur.start, next.end))}</span>`
          cursor = next.end
          i++ // 跳过 next
          continue
        }
        // -------------------------------------------

        // 函数名启发式（lowercase_identifier 后跟 '(' 或前面是 '::'）
        let isFunctionIdent = false
        if (cur.type === 'lowercase_identifier') {
          const after = src[cur.end] || ''
          const before2 = src.slice(Math.max(0, cur.start - 2), cur.start)
          if (after === '(' || before2 === '::') isFunctionIdent = true
        }

        const cls = this.getHighlightClass(cur.type, cur.text, { isFunctionIdent })
        html += `<span class="token ${cls}">${this.escapeHtml(cur.text)}</span>`
        cursor = cur.end
      }
      if (cursor < src.length) html += this.escapeHtml(src.slice(cursor))
      this.highlightedCode = html || this.escapeHtml(src)
    },

    flattenLeaves(node, source, out = []) {
      if (!node) return out
      if (!node.children || node.children.length === 0) {
        out.push({
          start: node.startIndex,
          end: node.endIndex,
          type: node.type,
          text: source.slice(node.startIndex, node.endIndex)
        })
        return out
      }
      for (const ch of node.children) this.flattenLeaves(ch, source, out)
      return out.sort((a,b) => a.start - b.start) // 保序
    },


    generateHTML(node, source) {
      if (!node || node.endIndex <= node.startIndex) return this.escapeHtml(source)

      // 叶子：常规路径
      if (!node.children || node.children.length === 0) {
        const text = source.slice(node.startIndex, node.endIndex)

        // 仍保留函数名启发式
        let isFunctionIdent = false
        if (node.type === 'lowercase_identifier') {
          const nextCh = source[node.endIndex] || ''
          const prev2  = source.slice(Math.max(0, node.startIndex - 2), node.startIndex)
          if (nextCh === '(' || prev2 === '::') isFunctionIdent = true
        }

        const className = this.getHighlightClass(node.type, text, { isFunctionIdent })
        return `<span class="token ${className}">${this.escapeHtml(text)}</span>`
      }

      // 非叶子：遍历 children，加入对 "# deprecated" 的合并处理
      let html = ''
      let i = node.startIndex
      const kids = node.children

      for (let k = 0; k < kids.length; k++) {
        const child = kids[k]

        // 先补充 child 之前的原文（空白/逗号等）
        if (i < child.startIndex) {
          html += this.escapeHtml(source.slice(i, child.startIndex))
          i = child.startIndex
        }

        // ---- 关键：识别 "# deprecated" 模式 ----
        if (
          child.type === '#' &&
          k + 1 < kids.length &&
          kids[k + 1].type === 'lowercase_identifier'
        ) {
          const next = kids[k + 1]
          const between = source.slice(child.endIndex, next.startIndex) // 可能是空串或空格
          const nextText = source.slice(next.startIndex, next.endIndex)

          // 只要下一个单词是 "deprecated"（大小写忽略），就把 "# + (空格) + deprecated" 合并高亮
          if (/^deprecated$/i.test(nextText)) {
            const mergedText = source.slice(child.startIndex, next.endIndex) // '#' + 可能的空格 + 'deprecated'
            html += `<span class="token deprecated-anno">${this.escapeHtml(mergedText)}</span>`
            i = next.endIndex
            k++ // 跳过 next
            continue
          }
        }
        // ---- 合并处理结束 ----

        // 常规递归
        html += this.generateHTML(child, source)
        i = child.endIndex
      }

      if (i < node.endIndex) {
        html += this.escapeHtml(source.slice(i, node.endIndex))
      }
      return html
    },


    getHighlightClass(nodeType, text = '', ctx = {}) {
      if (nodeType === 'ERROR') return 'error'

      const TEXT_KW = new Set(['fn','let','var','if','else','while','for','match',
        'struct','enum','trait','impl','pub','priv','mut','ref','return','break',
        'continue','raise'])
      if (TEXT_KW.has(text)) return 'keyword'

      if (nodeType === 'uppercase_identifier' || nodeType === 'type_identifier') return 'type'
      if (ctx.isFunctionIdent) return 'function'
      if (nodeType === 'lowercase_identifier' || nodeType === 'identifier') return 'identifier'

      if (nodeType === 'string_literal') return 'string'
      if (nodeType === 'number_literal' || nodeType === 'float_literal' || nodeType === 'int_literal') return 'number'
      if (nodeType === 'boolean_literal') return 'boolean'

      const OPS = new Set(['->','=>','==','!=','<=','>=','+','-','*','/','%','=','&&','||','!','&','|','^','<<','>>','::'])
      if (OPS.has(text) || nodeType?.endsWith?.('_operator')) return 'operator'
      const PUNCTS = new Set(['(',')','[',']','{','}','.',',',';',':','<','>'])
      if (PUNCTS.has(text) || nodeType === 'punctuation') return 'punctuation'

      if (nodeType === 'line_comment' || nodeType === 'block_comment') return 'comment'
      return 'default'
    },


    escapeHtml(text) {
      const map = { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }
      return (text || '').replace(/[&<>"']/g, (m) => map[m])
    }
  },

  beforeUnmount() {
    try { this.parser?.delete?.() } catch {}
    try { this.language?.delete?.() } catch {}
  }
}
</script>

<style scoped>
.moonbit-highlighter {
  font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
}

.code-container {
  background-color: #f6f8fa;
  border: 1px solid #e1e4e8;
  border-radius: 6px;
  padding: 16px;
  overflow-x: auto;
  margin: 0;
  line-height: 1.45;
  font-size: 14px;
}

/* 让 v-html 注入的 token 在 scoped 下也能命中样式 */
:deep(.token.keyword) { color: #d73a49; font-weight: bold; }
:deep(.token.string)  { color: #032f62; }
:deep(.token.number),
:deep(.token.boolean) { color: #005cc5; }
:deep(.token.comment) { color: #6a737d; font-style: italic; }
:deep(.token.function){ color: #6f42c1; font-weight: bold; }
:deep(.token.type)    { color: #005cc5; font-weight: bold; }
:deep(.token.identifier) { color: #24292e; }
:deep(.token.operator)   { color: #d73a49; }
:deep(.token.punctuation),
:deep(.token.default)    { color: #24292e; }
:deep(.token.function){ color:#6f42c1; font-weight:bold; }
:deep(.token.deprecated-anno){
  color:#b08800; font-style:italic;
  text-decoration: underline dotted #b08800;
}

</style>


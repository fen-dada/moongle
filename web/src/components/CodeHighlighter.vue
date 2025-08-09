<template>
  <div class="moonbit-highlighter">
    <pre v-if="loading" class="loading">正在加载语法高亮...</pre>
    <pre v-else-if="error" class="error">{{ error }}</pre>
    <pre v-else class="code-container"><code v-html="highlightedCode"></code></pre>
  </div>
</template>

<script>
// 单例初始化（内部只 init 一次，并缓存 language）
import { getMoonbit } from './ts-runtime'

// 把 MoonBit 的高亮查询当纯文本引入（若你暂时没有官方文件，可先用你自己的简化版）
// 路径按你的项目存放位置调整，例如：src/assets/moonbit-highlights.scm
import highlightsSource from '@/assets/moonbit-highlight.scm?raw'

// 如果需要在极端情况下退化到启发式，可保留极少量工具（这里不再用启发式给颜色）
export default {
  name: 'Highlight',
  props: {
    code: { type: String, required: true, default: '' }
  },
  data() {
    return {
      parser: null,
      language: null,
      query: null,              // 编译后的 Query
      highlightedCode: '',
      loading: true,
      error: null
    }
  },
  async mounted() {
    await this.initializeParser()
  },
  watch: {
    code() { this.highlightCode() }
  },
  methods: {
    async initializeParser () {
      this.loading = true
      this.error = null
      try {
        const { parser, language } = await getMoonbit()
        this.parser = parser
        this.language = language

        // v0.25.x 通常有 language.query；否则退回 Parser.Query
        this.query = this.language.query
          ? this.language.query(highlightsSource)
          : new (window.Parser?.Query ?? Query)(this.language, highlightsSource)

        this.loading = false
        this.highlightCode()
      } catch (err) {
        console.error('初始化解析器失败:', err)
        this.error = `初始化失败: ${err?.message || err}`
        this.loading = false
      }
    },

    highlightCode() {
      if (this.loading || !this.parser || !this.language) return
      const src = this.code ?? ''
      if (!src) { this.highlightedCode = ''; return }

      let html = ''
      try {
        const tree = this.parser.parse(src)

        if (!this.query) {
          // 没有查询规则就原文显示（避免空白）
          this.highlightedCode = this.escapeHtml(src)
          return
        }

        // 1) 执行查询：得到 [{ node, name }, ...]
        const caps = this.query.captures(tree.rootNode)

        // 2) 映射为可渲染片段（start/end/cls）
        const spans = caps.map(({ node, name }) => ({
          start: node.startIndex,
          end:   node.endIndex,
          cls:   this.classFromCapture(name)
        })).filter(s => s.end > s.start)

        // 3) 处理重叠：按优先级→长度→起点排序，贪心选取
        const ranked = spans.map(s => ({
          ...s,
          p: this.priorityOf(s.cls),
          len: s.end - s.start
        })).sort((a,b) => b.p - a.p || b.len - a.len || a.start - b.start)

        const chosen = []
        for (const s of ranked) {
          const overlap = chosen.some(t => !(s.end <= t.start || s.start >= t.end))
          if (!overlap) chosen.push({ start: s.start, end: s.end, cls: s.cls })
        }
        chosen.sort((a,b) => a.start - b.start)

        // 4) 按片段拼 HTML（未命中的片段原样转义）
        let i = 0
        for (const s of chosen) {
          if (i < s.start) html += this.escapeHtml(src.slice(i, s.start))
          html += `<span class="token ${s.cls}">${this.escapeHtml(src.slice(s.start, s.end))}</span>`
          i = s.end
        }
        if (i < src.length) html += this.escapeHtml(src.slice(i))

        // 兜底：空则回原文
        this.highlightedCode = html && html.trim() ? html : this.escapeHtml(src)
      } catch (err) {
        console.error('代码高亮失败:', err)
        this.highlightedCode = this.escapeHtml(src)
      }
    },

    // capture 名 → CSS 类名
    classFromCapture(name) {
      if (!name) return 'default'
      // Tree-sitter capture 名可能是 "function.builtin" 这种层级名，这里用 includes 容忍不同后缀
      if (name.includes('keyword'))     return 'keyword'
      if (name.includes('function'))    return 'function'
      if (name.includes('type'))        return 'type'
      if (name.includes('string'))      return 'string'
      if (name.includes('number'))      return 'number'
      if (name.includes('boolean'))     return 'boolean'
      if (name.includes('comment'))     return 'comment'
      if (name.includes('operator'))    return 'operator'
      if (name.includes('punctuation')) return 'punctuation'
      if (name.includes('attribute'))   return 'deprecated-anno' // 用 @attribute 呈现 #deprecated 等标注
      if (name.includes('constant'))    return 'constant'
      if (name.includes('variable'))    return 'identifier'
      return 'default'
    },

    // 冲突时谁盖谁（可按主题调整）
    priorityOf(cls) {
      const table = {
        error: 100,
        keyword: 90,
        function: 80,
        type: 70,
        string: 60, number: 60, boolean: 60, constant: 60,
        'deprecated-anno': 55,
        comment: 50,
        operator: 40,
        identifier: 20,
        punctuation: 10,
        default: 0
      }
      return table[cls] ?? 0
    },

    escapeHtml(text) {
      const map = { '&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;' }
      return (text || '').replace(/[&<>"']/g, m => map[m])
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

/* v-html 动态注入节点，用 :deep 命中 */
:deep(.token.keyword){ color:#d73a49; font-weight:600; }
:deep(.token.function){ color:#6f42c1; font-weight:600; }
:deep(.token.type){ color:#005cc5; font-weight:600; }
:deep(.token.string){ color:#032f62; }
:deep(.token.number), :deep(.token.boolean){ color:#005cc5; }
:deep(.token.comment){ color:#6a737d; font-style:italic; }
:deep(.token.operator){ color:#d73a49; }
:deep(.token.constant){ color:#b31d28; }
:deep(.token.identifier){ color:#24292e; }
:deep(.token.punctuation), :deep(.token.default){ color:#24292e; }
:deep(.token.deprecated-anno){
  color:#b08800; font-style:italic; text-decoration: underline dotted #b08800;
}
</style>


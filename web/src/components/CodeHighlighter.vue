<script setup>
import { ref, watch, onMounted } from 'vue';
import Parser from 'web-tree-sitter';

const props = defineProps({
  code: {
    type: String,
    required: true,
  },
  lang: {
    type: String,
    required: true,
  }
});

const highlightedCode = ref('');
let parser;

// This map translates Tree-sitter node types to our CSS classes.
// You can expand this with more types from the tree-sitter-moonbit grammar.
const nodeTypeToClass = {
  "type_id": "t-type",
  "id": "t-variable",
  "field_id": "t-property",
  "fun": "t-keyword-function",
  "let": "t-keyword",
  "if": "t-keyword-control",
  "else": "t-keyword-control",
  "match": "t-keyword-control",
  "string_literal": "t-string",
  "int_literal": "t-number",
  "comment": "t-comment",
  "line_comment": "t-comment",
  "block_comment": "t-comment",
  "unit": "t-constant",
  "true": "t-constant",
  "false": "t-constant",
  "LPAREN": "t-punctuation",
  "RPAREN": "t-punctuation",
  "LBRACE": "t-punctuation",
  "RBRACE": "t-punctuation",
};

// Function to walk the syntax tree and generate HTML
function highlight(tree) {
  let output = '';
  const cursor = tree.walk();

  function walk() {
    // Go to first child
    if (cursor.gotoFirstChild()) {
      do {
        const nodeType = cursor.nodeType;
        const className = nodeTypeToClass[nodeType];

        if (className) {
          output += `<span class="${className}">`;
        }
        
        // If the node has children, recurse. Otherwise, add its text.
        if (cursor.currentNode().childCount > 0) {
          walk();
        } else {
          // Use a simple text escape to prevent XSS
          output += escapeHtml(cursor.currentNode().text);
        }

        if (className) {
          output += `</span>`;
        }
      } while (cursor.gotoNextSibling());
      // Go back to parent
      cursor.gotoParent();
    }
  }
  
  walk();
  return output;
}

function escapeHtml(text) {
    return text
         .replace(/&/g, "&amp;")
         .replace(/</g, "&lt;")
         .replace(/>/g, "&gt;")
         .replace(/"/g, "&quot;")
         .replace(/'/g, "&#039;");
}


async function initializeAndHighlight() {
  if (!parser) {
    await Parser.init({
      locateFile(scriptName, scriptDirectory) {
        return scriptName;
      },
    });
    parser = new Parser();
    const Lang = await Parser.Language.load(`${props.lang}.wasm`);
    parser.setLanguage(Lang);
  }

  const tree = parser.parse(props.code);
  highlightedCode.value = highlight(tree);
}

onMounted(() => {
  initializeAndHighlight();
});

watch(() => props.code, () => {
  initializeAndHighlight();
});
</script>

<template>
  <pre class="code-container"><code v-html="highlightedCode"></code></pre>
</template>

<style scoped>
/* This component provides the structure, theming is handled by the CSS variables from App.vue */
.code-container {
  color: var(--text-color);
  background-color: var(--input-bg);
  border: 1px solid var(--input-border);
  font-family: 'Fira Code', 'Courier New', monospace;
  font-size: 0.9rem;
  line-height: 1.6;
  padding: 1rem;
  border-radius: 8px;
  white-space: pre-wrap;
  word-wrap: break-word;
}
</style>

<style>
/* Global styles for highlighting tokens. Not scoped. */
/* These use CSS variables to be theme-aware. */

.t-comment {
  color: var(--link-color);
  font-style: italic;
}
.t-keyword, .t-keyword-function, .t-keyword-control {
  color: #c678dd; /* A standard purple for keywords */
}
.t-string {
  color: #98c379; /* A standard green for strings */
}
.t-number, .t-constant {
  color: #d19a66; /* A standard orange for numbers/constants */
}
.t-type {
  color: #e5c07b; /* A standard yellow for types */
}
.t-function, .t-property {
  color: #61afef; /* A standard blue for functions/properties */
}
.t-variable {
  color: var(--text-color); /* Default text color for variables */
}
.t-punctuation {
  color: var(--link-color);
}

/* Dark mode specific overrides if needed, but variables should handle most of it */
html.dark .t-keyword, html.dark .t-keyword-function, html.dark .t-keyword-control {
  color: #d8a6ff;
}
html.dark .t-type {
  color: #f0d698;
}
</style>

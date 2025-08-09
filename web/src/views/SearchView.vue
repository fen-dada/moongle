<script setup>
import { ref, computed, watch } from 'vue';
import { useRoute } from 'vue-router';
import MoonBitHighlighter from '../components/CodeHighlighter.vue'
import CodeHighlighter from '../components/CodeHighlighter.vue'; // Import the new component

const route = useRoute();

const routeSearchQuery = computed(() => route.query.q || '');
const searchResults = ref([]);
const isLoading = ref(false);
const error = ref(null);
const hitsTotal = ref(0);

async function fetchSearchResults() {
  if (!routeSearchQuery.value) {
    searchResults.value = [];
    hitsTotal.value = 0;
    return;
  }

  isLoading.value = true;
  error.value = null;

  try {
    const response = await fetch('/api/search', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        dat: {
          q: routeSearchQuery.value,
          limit: 50,
        },
      }),
    });

    if (!response.ok) {
      const errorText = await response.text();
      throw new Error(`服务器返回错误: ${response.status} - ${errorText}`);
    }

    const data = await response.json();

    if (data.status === 'ok' && data.dat) {
      searchResults.value = data.dat.items;
      hitsTotal.value = data.dat.hitsTotal;
    } else {
      throw new Error(data.err?.message || '获取搜索结果失败。');
    }
  } catch (e) {
    error.value = e.message;
    searchResults.value = [];
    hitsTotal.value = 0;
  } finally {
    isLoading.value = false;
  }
}

watch(
  () => route.query.q,
  () => {
    fetchSearchResults();
  },
  { immediate: true }
);
</script>

<template>
  <div class="search-results-container">
    <h1 class="results-title" v-if="routeSearchQuery">
      搜索结果: <span class="query-text">"{{ routeSearchQuery }}"</span>
    </h1>
    <h1 class="results-title" v-else>
      请输入一个搜索词
    </h1>

    <div v-if="isLoading" class="loading-state">
      <p>正在加载...</p>
    </div>

    <div v-else-if="error" class="error-message">
      <p>出错了: {{ error }}</p>
    </div>

    <div v-else-if="searchResults.length > 0" class="results-list">
      <p class="hits-total">找到约 {{ hitsTotal }} 条结果</p>
      <div v-for="(result, index) in searchResults" :key="index" class="result-item">
        <a :href="`https://mooncakes.io/docs/${result.user}/${result.mod}/${result.package.join('/')}`" class="result-item-title">
          {{ result.user }}/{{ result.mod }}/{{ result.package.join('/') }}
        </a>
        <!--<pre class="result-item-snippet"><code>{{ result.decl }}</code></pre> -->
        <MoonBitHighlighter :code="result.decl" class="result-item-snippet" />
      </div>
    </div>

    <div v-else-if="routeSearchQuery" class="no-results">
      <p>没有找到与 "{{ routeSearchQuery }}" 相关的结果，请尝试其他关键词。</p>
    </div>
  </div>
</template>

<style scoped>
/* Using CSS variables defined in App.vue for theming */
.search-results-container {
  width: 100%;
  max-width: 800px;
  margin: 0 auto;
  padding: 2rem;
  box-sizing: border-box;
}

.results-title {
  color: var(--text-color);
  font-size: 1.75rem;
  font-weight: 600;
  margin-bottom: 1rem;
}

.query-text {
  color: var(--link-hover-color);
}

.hits-total {
  color: var(--link-color);
  font-size: 0.9rem;
  margin-bottom: 2rem;
}

.results-list {
  display: flex;
  flex-direction: column;
}

.result-item {
  border-bottom: 1px solid var(--header-border);
  padding: 1.5rem 0;
}
.result-item:first-child {
  padding-top: 0;
}
.result-item:last-child {
  border-bottom: none;
}

.result-item-title {
  color: var(--link-hover-color);
  font-size: 1.25rem;
  font-weight: 500;
  text-decoration: none;
  display: inline-block;
  margin-bottom: 0.5rem;
}
.result-item-title:hover {
  text-decoration: underline;
}

.result-item-snippet {
  color: var(--text-color);
  background-color: var(--input-bg); /* Using input background for code blocks */
  border: 1px solid var(--input-border);
  font-family: 'Fira Code', 'Courier New', monospace;
  font-size: 0.9rem;
  line-height: 1.6;
  padding: 1rem;
  border-radius: 8px;
  white-space: pre-wrap;
    word-wrap: break-word;
}

.no-results,
.loading-state,
.error-message {
  background-color: var(--input-bg);
  border: 1px solid var(--input-border);
  color: var(--link-color);
  border-radius: 8px;
  padding: 4rem 2rem;
  text-align: center;
}

.error-message {
  color: #b91c1c; /* Dark red for text */
  background-color: #fef2f2; /* Light red background */
  border-color: #fecaca; /* Lighter red border */
}

html.dark .error-message {
  color: #fca5a5; /* Light red text for dark mode */
  background-color: #450a0a; /* Very dark red background */
  border-color: #991b1b; /* Darker red border */
}
</style>

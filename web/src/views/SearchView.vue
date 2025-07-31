<script setup>
import { computed } from 'vue';
import { useRoute } from 'vue-router';

const route = useRoute();

// 计算属性，用于从 URL 实时获取搜索词，并展示在页面上
const routeSearchQuery = computed(() => route.query.q || '');

// 在真实的应用中，你会在这里调用 API 来获取搜索结果
const searchResults = computed(() => {
  if (!routeSearchQuery.value) {
    return [];
  }
  // 模拟 API 调用返回的结果
  return [
    { id: 1, title: `关于 "${routeSearchQuery.value}" 的第一条结果`, snippet: '这是一个结果的摘要内容，帮助用户快速了解链接详情。' },
    { id: 2, title: `"${routeSearchQuery.value}" 的另一个相关结果`, snippet: '摘要内容可以从搜索到的文档中自动生成，通常是包含关键词的段落。' },
    { id: 3, title: `探索更多关于 "${routeSearchQuery.value}"`, snippet: '点击标题可以跳转到该结果的详细页面。' },
  ];
});
</script>

<template>
  <div class="search-results-container">
    <h1 class="results-title" v-if="routeSearchQuery">
      搜索结果: <span class="query-text">"{{ routeSearchQuery }}"</span>
    </h1>
    <h1 class="results-title" v-else>
      请输入一个搜索词
    </h1>

    <div v-if="searchResults.length > 0" class="results-list">
      <div v-for="result in searchResults" :key="result.id" class="result-item">
        <a href="#" class="result-item-title">{{ result.title }}</a>
        <p class="result-item-snippet">{{ result.snippet }}</p>
      </div>
    </div>
    <div v-else-if="routeSearchQuery" class="no-results">
      <p>没有找到与 "{{ routeSearchQuery }}" 相关的结果，请尝试其他关键词。</p>
    </div>
  </div>
</template>

<style scoped>
.search-results-container {
  width: 100%;
  max-width: 800px;
  padding: 2rem;
  box-sizing: border-box;
  font-family: 'Inter', sans-serif;
}

.results-title {
  font-size: 1.75rem;
  font-weight: 600;
  margin-bottom: 2.5rem;
  color: #1e293b;
}
.query-text {
  color: #4f46e5;
}
.results-list {
  display: flex;
  flex-direction: column;
  gap: 1.5rem;
}
.result-item {
  padding: 0.5rem 0;
}
.result-item-title {
  font-size: 1.25rem;
  font-weight: 500;
  color: #4f46e5;
  text-decoration: none;
  display: inline-block;
  margin-bottom: 0.5rem;
}
.result-item-title:hover {
  text-decoration: underline;
}
.result-item-snippet {
  font-size: 1rem;
  line-height: 1.6;
  color: #64748b;
}
.no-results {
  text-align: center;
  padding: 4rem 2rem;
  color: #64748b;
  background-color: #f8fafc;
  border-radius: 8px;
}
</style>

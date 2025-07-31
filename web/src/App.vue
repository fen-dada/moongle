<script setup>
import { ref, watch, computed } from 'vue';
import { useRoute, useRouter } from 'vue-router';

const route = useRoute();
const router = useRouter();
const searchQuery = ref('');

// 计算属性，判断当前是否为主页
const isHomePage = computed(() => route.name === 'Home');

// 搜索处理函数
const handleSearch = () => {
  if (!searchQuery.value.trim()) {
    return;
  }
  router.push({
    name: 'Search',
    query: { q: searchQuery.value },
  });
};

// 监听路由变化，确保搜索框内容与URL参数同步
watch(
  () => route.query.q,
  (newQuery) => {
    searchQuery.value = newQuery || '';
  },
  { immediate: true }
);
</script>

<template>
  <div id="app-layout">
    <header class="app-header">
      <div class="header-left">
        <router-link to="/" class="logo">
          <span class="logo-text">moongle</span>
        </router-link>
      </div>

      <!-- 顶栏搜索框，仅在非主页显示 -->
      <div class="header-center" v-if="!isHomePage">
        <form class="header-search-bar-container" @submit.prevent="handleSearch">
          <input
            type="text"
            class="header-search-input"
            placeholder="Search for..."
            v-model="searchQuery"
            aria-label="Search"
          >
          <button type="submit" class="header-search-button" aria-label="Submit search">
            <svg class="header-search-icon" width="20" height="20" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                <path d="M11 19C15.4183 19 19 15.4183 19 11C19 6.58172 15.4183 3 11 3C6.58172 3 3 6.58172 3 11C3 15.4183 6.58172 19 11 19Z" stroke="#9ca3af" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
                <path d="M21 21L16.65 16.65" stroke="#9ca3af" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            </svg>
          </button>
        </form>
      </div>

      <nav class="main-nav">
        <router-link to="/examples">Tutorial</router-link>
        <router-link to="/stats">Stats</router-link>
        <a href="https://github.com/your-repo" target="_blank" rel="noopener noreferrer" class="github-link">
          <svg height="20" viewBox="0 0 16 16" version="1.1" width="20" aria-hidden="true"><path fill-rule="evenodd" fill="currentColor" d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z"></path></svg>
          <span>GitHub</span>
        </a>
      </nav>
    </header>

    <main class="content-area">
      <router-view />
    </main>
  </div>
</template>

<style scoped>
#app-layout {
  display: flex;
  flex-direction: column;
  min-height: 100vh;
  font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Open Sans', 'Helvetica Neue', sans-serif;
  background-color: #f8fafc;
}
.app-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0 2rem;
  border-bottom: 1px solid #e2e8f0;
  background-color: #ffffff;
  height: 64px;
  box-sizing: border-box;
  flex-shrink: 0;
}
.header-left {
  display: flex;
  align-items: center;
  flex: 1;
}
.logo {
  display: flex;
  align-items: center;
  text-decoration: none;
}
.logo-img {
  height: 30px;
  margin-right: 0.6rem;
}
.logo-text {
  font-size: 1.25rem;
  font-weight: 600;
  color: #1e293b;
  letter-spacing: -0.02em;
}
.header-center {
  flex: 2;
  display: flex;
  justify-content: center;
  align-items: center; /* 确保搜索框垂直居中 */
  padding: 0 2rem;
}
.main-nav {
  display: flex;
  align-items: center;
  justify-content: flex-end;
  gap: 2rem;
  flex: 1;
}
.main-nav a {
  text-decoration: none;
  color: #475569;
  font-weight: 500;
  font-size: 0.9rem;
  transition: color 0.2s ease;
  display: flex;
  align-items: center;
  gap: 0.4rem;
}
.main-nav a:hover {
  color: #4f46e5;
}
.main-nav a.router-link-exact-active {
  color: #4f46e5;
  font-weight: 600;
}
.external-icon {
  margin-bottom: 2px;
  color: #94a3b8;
}
.main-nav a:hover .external-icon {
  color: #4f46e5;
}
.content-area {
  flex: 1;
  display: flex;
  justify-content: center;
  align-items: flex-start;
}

/* 顶栏搜索框样式 (借鉴 HomeView) */
.header-search-bar-container {
  position: relative;
  width: 100%;
  max-width: 580px;
}
.header-search-input {
  width: 100%;
  height: 38px; /* 调整高度以更好地匹配导航链接 */
  padding: 0 3.5rem 0 1.5rem; /* 移除上下内边距，让 flexbox 处理对齐 */
  font-size: 0.9rem;
  border: 1px solid #e5e7eb;
  border-radius: 9999px;
  box-shadow: 0 1px 6px rgba(32, 33, 36, 0.1);
  transition: box-shadow 0.2s ease;
  box-sizing: border-box;
  display: flex; /* 新增 */
  align-items: center; /* 新增 */
}
.header-search-input:focus {
  outline: none;
  box-shadow: 0 2px 8px rgba(32, 33, 36, 0.15);
}
.header-search-button {
  position: absolute;
  top: 50%;
  right: 0.5rem;
  transform: translateY(-50%);
  background: transparent;
  border: none;
  padding: 0.5rem;
  cursor: pointer;
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
}
.header-search-button:hover .header-search-icon path {
  stroke: #685ff8;
}
.header-search-icon path {
  transition: stroke 0.2s ease;
}
</style>

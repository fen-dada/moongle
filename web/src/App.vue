<script setup>
import { ref, watchEffect, computed } from 'vue';
import { useRoute, useRouter } from 'vue-router';

const route = useRoute();
const router = useRouter();
const searchQuery = ref('');

// --- Dark Mode Logic ---
const initialTheme = localStorage.getItem('theme') || (window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light');
const isDarkMode = ref(initialTheme === 'dark');

const toggleTheme = () => {
  isDarkMode.value = !isDarkMode.value;
};

watchEffect(() => {
  document.documentElement.classList.toggle('dark', isDarkMode.value);
  localStorage.setItem('theme', isDarkMode.value ? 'dark' : 'light');
});


// --- Search Logic ---
const isHomePage = computed(() => route.name === 'Home');

const handleSearch = () => {
  if (!searchQuery.value.trim()) {
    return;
  }
  router.push({
    name: 'Search',
    query: { q: searchQuery.value },
  });
};

watchEffect(() => {
  searchQuery.value = route.query.q || '';
});
</script>

<template>
  <div id="app-layout">
    <header class="app-header">
      <div class="header-content">
        <div class="header-left">
          <router-link to="/" class="logo">
            <span class="logo-text">moongle</span>
          </router-link>
        </div>

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
          <div class="action-icons">
            <a href="https://github.com/your-repo" target="_blank" rel="noopener noreferrer" class="github-link">
              <svg height="20" viewBox="0 0 16 16" version="1.1" width="20" aria-hidden="true"><path fill-rule="evenodd" fill="currentColor" d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z"></path></svg>
              <span>GitHub</span>
            </a>
            <button @click="toggleTheme" class="theme-toggle-button" aria-label="Toggle theme">
              <svg v-if="!isDarkMode" xmlns="http://www.w3.org/2000/svg" width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><circle cx="12" cy="12" r="5"></circle><line x1="12" y1="1" x2="12" y2="3"></line><line x1="12" y1="21" x2="12" y2="23"></line><line x1="4.22" y1="4.22" x2="5.64" y2="5.64"></line><line x1="18.36" y1="18.36" x2="19.78" y2="19.78"></line><line x1="1" y1="12" x2="3" y2="12"></line><line x1="21" y1="12" x2="23" y2="12"></line><line x1="4.22" y1="19.78" x2="5.64" y2="18.36"></line><line x1="18.36" y1="5.64" x2="19.78" y2="4.22"></line></svg>
              <svg v-else xmlns="http://www.w3.org/2000/svg" width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z"></path></svg>
            </button>
          </div>
        </nav>
      </div>
    </header>

    <main class="content-area">
      <router-view />
    </main>
  </div>
</template>

<style>
:root {
  --bg-color: #FEFAF7;
  --text-color: #1e293b;
  --header-bg: var(--bg-color);
  --header-border: #e2e8f0;
  --link-color: #475569;
  --link-hover-color: #4f46e5;
  --input-bg: #ffffff;
  --input-border: #e5e7eb;
  --input-focus-shadow: rgba(139, 92, 246, 0.2);
}

html.dark {
  color-scheme: dark;
  --bg-color: #181a1b;
  --text-color: #cbd5e1;
  --header-bg: var(--bg-color);
  --header-border: #334155;
  --link-color: #94a3b8;
  --link-hover-color: #818cf8;
  --input-bg: #334155;
  --input-border: #475569;
  --input-focus-shadow: rgba(130, 140, 248, 0.3);
}

body {
  margin: 0; /* Reset default browser margin */
  background-color: var(--bg-color);
  color: var(--text-color);
  transition: background-color 0.3s, color 0.3s;
}
</style>

<style scoped>
#app-layout {
  display: flex;
  flex-direction: column;
  min-height: 100vh;
  font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Open Sans', 'Helvetica Neue', sans-serif;
  background-color: var(--bg-color);
  transition: background-color 0.3s; /* Add this line */
}
.app-header {
  position: fixed;
  top: 0;
  left: 0;
  z-index: 1000;
  width: 100%;
  background-color: var(--header-bg);
  border-bottom: 1px solid var(--header-border);
  transition: background-color 0.3s, border-color 0.3s;
}

.header-content {
  display: flex;
  align-items: center;
  justify-content: space-between;
  width: 100%;          /* 占满整行 */
  max-width: none;      /* 删掉 1400px 限制 */
  margin: 0;            /* 去掉左右 auto 居中 */
  padding: 0 32px;      /* 固定左右边距（你可以改成 16px/32px） */
  height: 64px;
  box-sizing: border-box;
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
  color: var(--text-color);
  letter-spacing: -0.02em;
}
.header-center {
  flex: 2;
  display: flex;
  justify-content: center;
  align-items: center;
  padding: 0 2rem;
}
.main-nav {
  display: flex;
  align-items: center;
  justify-content: flex-end;
  gap: 1.5rem;
  flex: 1;
}
.action-icons {
  display: flex;
  align-items: center;
  gap: 0.75rem;
}
.main-nav a {
  text-decoration: none;
  color: var(--link-color);
  font-weight: 500;
  font-size: 0.9rem;
  transition: color 0.2s ease;
  display: flex;
  align-items: center;
  gap: 0.4rem;
}
.main-nav a:hover {
  color: var(--link-hover-color);
}
.main-nav a.router-link-exact-active {
  color: var(--link-hover-color);
  font-weight: 600;
}
.github-link {
  color: var(--text-color);
}
.github-link:hover {
  color: var(--link-hover-color);
}

.header-search-bar-container {
  position: relative;
  width: 100%;
  max-width: 580px;
}
.header-search-input {
  width: 100%;
  height: 38px;
  padding: 0 3.5rem 0 1.5rem;
  font-size: 0.9rem;
  border: 1px solid var(--input-border);
  border-radius: 9999px;
  background-color: var(--input-bg);
  color: var(--text-color);
  transition: all 0.2s ease;
  box-sizing: border-box;
  display: flex;
  align-items: center;
}
.header-search-input:focus {
  outline: none;
  box-shadow: 0 0 0 3px var(--input-focus-shadow);
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
  stroke: var(--link-hover-color);
}
.header-search-icon path {
  transition: stroke 0.2s ease;
}

.theme-toggle-button {
  background: none;
  border: none;
  cursor: pointer;
  padding: 0.5rem;
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  color: var(--link-color);
  transition: background-color 0.2s, color 0.2s;
}

.theme-toggle-button:focus {
  outline: none;
}

/* Apply highlight on hover or keyboard focus */
.theme-toggle-button:hover,
.theme-toggle-button:focus-visible {
  color: var(--link-hover-color);
  background-color: var(--input-bg);
}
.content-area {
  flex: 1;
  display: flex;
  /* This is the key change: ensures the child <router-view> stretches vertically */
  align-items: stretch;
  width: 100%;
  padding-top: 64px; /* Account for fixed header */
  box-sizing: border-box;
}
</style>

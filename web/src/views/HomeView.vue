<script setup>
import { ref } from 'vue'
import { useRouter } from 'vue-router' // 1. 导入 useRouter
import logoIconUrl from '../assets/moongle-logo.png'

const searchQuery = ref('')
const router = useRouter() // 2. 获取 router 实例

// 3. 创建处理搜索提交的方法
const handleSearch = () => {
  // 如果输入为空，则不进行搜索
  if (!searchQuery.value.trim()) {
    return;
  }
  // 4. 使用 router.push 进行页面跳转
  // 我们将搜索词作为 URL 的查询参数 (query parameter) 传递
  router.push({
    name: 'Search', // 跳转到名为 'Search' 的路由
    query: {
      q: searchQuery.value // 参数名为 q，值为输入框的内容
    }
  })
}
</script>

<template>
  <div class="page-container">
    <main class="content-wrapper">
      <div class="logo">
        <div
          class="moongle-icon"
          :style="{
            maskImage: `url(${logoIconUrl})`,
            webkitMaskImage: `url(${logoIconUrl})`
          }"
        ></div>
        <h1 class="logo-text">moongle</h1>
      </div>

      <!-- 5. 将搜索框包裹在 form 中，并监听 submit 事件 -->
      <form class="search-bar-container" @submit.prevent="handleSearch">
        <input
          type="text"
          class="search-input"
          placeholder="Search for..."
          v-model="searchQuery"
          aria-label="Search"
        >
        <!-- 6. 将 SVG 图标变成一个可点击的 button -->
        <button type="submit" class="search-button" aria-label="Submit search">
          <svg class="search-icon" width="20" height="20" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
              <path d="M11 19C15.4183 19 19 15.4183 19 11C19 6.58172 15.4183 3 11 3C6.58172 3 3 6.58172 3 11C3 15.4183 6.58172 19 11 19Z" stroke="#9ca3af" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
              <path d="M21 21L16.65 16.65" stroke="#9ca3af" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
          </svg>
        </button>
      </form>
      <p class="search-hint">Search Moonbit docs & APIs</p>
    </main>

    <footer class="page-footer">
      <a href="#">About</a>
      <a href="#">Privacy</a>
      <a href="#">Terms</a>
    </footer>
  </div>
</template>

<style scoped>
/* 保持大部分样式不变，只修改和添加搜索按钮相关的样式 */
.page-container {
  display: flex;
  flex: 1; /* Fill the parent .content-area */
  flex-direction: column;
  justify-content: center; /* Center the content vertically */
  align-items: center; /* Center the content horizontally */
  width: 100%;
  /* background-color is now inherited from #app-layout */
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Open Sans', 'Helvetica Neue', sans-serif;
  box-sizing: border-box;
  padding-bottom: 15vh; /* Push content up from the vertical center */
}

.content-wrapper {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 0.5rem;
  width: 100%;
  padding: 2rem;
  box-sizing: border-box;
  /* The negative margin was a mistake, removing it */
}

.logo {
  display: flex;
  flex-direction: column;
  align-items: center;
}

.logo-text {
  font-size: 6rem;
  font-weight: 700;
  margin: -1.5rem 0 0 0;
  letter-spacing: 0.02em;
  background: linear-gradient(to left, #D474FA, #685ff8);
  -webkit-background-clip: text;
  background-clip: text;
  color: transparent;
  user-select: none;
}

.moongle-icon {
  width: 100px;
  height: 100px;
  background: linear-gradient(to right, #8954f4, #a45af4);
  mask-size: contain;
  -webkit-mask-size: contain;
  mask-repeat: no-repeat;
  -webkit-mask-repeat: no-repeat;
  mask-position: center;
  -webkit-mask-position: center;
}

.search-bar-container {
  position: relative;
  width: 100%;
  max-width: 580px;
}

.search-input {
  width: 100%;
  padding: 0.75rem 3.5rem 0.75rem 1.5rem; /* 右边距稍微增大给按钮留出空间 */
  font-size: 1rem;
  border: 1px solid #e5e7eb;
  border-radius: 9999px;
  box-shadow: 0 1px 6px rgba(32, 33, 36, 0.1);
  transition: box-shadow 0.2s ease;
  box-sizing: border-box;
}

.search-input:focus {
  outline: none;
  box-shadow: 0 2px 8px rgba(32, 33, 36, 0.15);
}

/* --- 新增和修改的样式 --- */
.search-button {
  position: absolute;
  top: 50%;
  right: 0.5rem; /* 调整位置 */
  transform: translateY(-50%);
  background: transparent;
  border: none;
  padding: 0.5rem; /* 增大点击区域 */
  cursor: pointer;
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
}

.search-button:hover .search-icon path {
  stroke: #685ff8; /* 悬停时图标变色 */
}

.search-icon path {
  transition: stroke 0.2s ease;
}

.search-hint {
  font-size: 0.875rem;
  color: #9ca3af;
  margin-top: 0.75rem;
  user-select: none;
}

.page-footer {
  position: absolute;
  bottom: 2rem;
  display: flex;
  gap: 1.5rem;
}

.page-footer a {
  font-size: 0.875rem;
  color: #6b7280;
  text-decoration: none;
  transition: color 0.2s ease;
}

.page-footer a:hover {
  color: #111827;
}
</style>

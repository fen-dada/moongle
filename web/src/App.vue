<template>
  <div class="page-container">
    <main class="content-wrapper">
      <div class="logo">
        <!--
        <svg class="logo-icon" width="60" height="60" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
          <path d="M21 12C21 16.9706 16.9706 21 12 21C7.02944 21 3 16.9706 3 12C3 7.02944 7.02944 3 12 3C13.6559 3 15.2285 3.42479 16.6001 4.17161" stroke="#A855F7" stroke-width="2" stroke-linecap="round"/>
          <path d="M12 9C10.3431 9 9 10.3431 9 12C9 13.6569 10.3431 15 12 15C13.6569 15 15 13.6569 15 12C15 10.3431 13.6569 9 12 9Z" fill="#A855F7"/>
        </svg>
        -->
        <div
          class="moongle-icon"
          :style="{
            maskImage: `url(${logoIconUrl})`,
            webkitMaskImage: `url(${logoIconUrl})`
          }"
        ></div>
        <h1 class="logo-text">moongle</h1>
      </div>

      <div class="search-bar-container">
        <input type="text" class="search-input">
        <svg class="search-icon" width="20" height="20" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M11 19C15.4183 19 19 15.4183 19 11C19 6.58172 15.4183 3 11 3C6.58172 3 3 6.58172 3 11C3 15.4183 6.58172 19 11 19Z" stroke="#9ca3af" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
            <path d="M21 21L16.65 16.65" stroke="#9ca3af" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>
      </div>
      <p class="search-hint">Search Moonbit docs & APIs</p>
    </main>

    <footer class="page-footer">
      <a href="#">About</a>
      <a href="#">Privacy</a>
      <a href="#">Terms</a>
    </footer>
  </div>
</template>

// src/App.vue
<script setup>
import { ref } from 'vue'
import logoIconUrl from './assets/moongle-logo.png'

const searchQuery = ref('')
</script>

<style scoped></style>
/* src/App.vue */
<style scoped>
/* 全局和布局 */
.page-container {
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
  min-height: 100vh;
  background-color: #FEFAF7;;
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Open Sans', 'Helvetica Neue', sans-serif;
  position: relative; /* 为页脚定位提供参照 */
  padding-bottom: 80px; /* 为页脚留出空间 */
}

.content-wrapper {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 0.5rem; /* Logo 和搜索框之间的间距 */
}

/* Logo 样式 */
.logo {
  display: flex;
  flex-direction: column;
  align-items: center;
}

.logo-text {
  font-size: 6rem; /* 大字体 */
  font-weight: 700;
  margin: -1.5rem 0 0 0;
  letter-spacing: 0.02em;
  /* 关键的渐变文字效果 */
  background: linear-gradient(to left, #D474FA, #685ff8);
  -webkit-background-clip: text;
  background-clip: text;
  color: transparent;

  /* --- 新增以下属性来禁止文本选中 --- */
  -webkit-user-select: none; /* 为了兼容 Safari 和旧版 Chrome */
  -moz-user-select: none;    /* 为了兼容 Firefox */
  -ms-user-select: none;     /* 为了兼容 IE/Edge */
  user-select: none;         /* 标准语法 */
}

/* 搜索框样式 */
.search-bar-container {
  position: relative;
  width: 100%;
  max-width: 580px; /* 限制最大宽度 */
}

.search-input {
  width: 100%;
  padding: 0.75rem 3rem 0.75rem 1.5rem;
  font-size: 1rem;
  border: 1px solid #e5e7eb;
  border-radius: 9999px; /* 圆角 */
  box-shadow: 0 1px 6px rgba(32, 33, 36, 0.1);
  transition: box-shadow 0.2s ease;
  box-sizing: border-box; /* 确保 padding 不会撑大宽度 */
}

.search-input:focus {
  outline: none;
  box-shadow: 0 2px 8px rgba(32, 33, 36, 0.15);
}

.search-icon {
  position: absolute;
  top: 50%;
  right: 1.5rem;
  transform: translateY(-50%);
  pointer-events: none; /* 让图标不影响鼠标点击 */

}

.search-hint {
  font-size: 0.875rem; /* 14px，比输入框的字体小 */
  color: #9ca3af;     /* 使用一个柔和的灰色 */
  margin-top: 0.75rem; /* 与搜索框保持一点距离 */
  /* --- 新增以下属性来禁止文本选中 --- */
  -webkit-user-select: none; /* 为了兼容 Safari 和旧版 Chrome */
  -moz-user-select: none;    /* 为了兼容 Firefox */
  -ms-user-select: none;     /* 为了兼容 IE/Edge */
  user-select: none;         /* 标准语法 */
}

.moongle-icon {
  width: 100px; /* 稍微增大了图标尺寸 */
  height: 100px;

  /* 将渐变色作为背景 */
  background: linear-gradient(to right, #8954f4, #a45af4);

  /* --- 关键改动在这里 --- */
  /* `mask-image` 已通过内联 style 绑定 */

  /* 删除了 mask-mode: luminance，让其使用默认的 alpha (透明度) 模式 */

  /* 确保遮罩正确显示 */
  mask-size: contain; /* 改回 contain，因为它对透明 PNG 效果最好 */
  -webkit-mask-size: contain;
  mask-repeat: no-repeat;
  -webkit-mask-repeat: no-repeat;
  mask-position: center;
  -webkit-mask-position: center;
}

/* 页脚样式 */
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

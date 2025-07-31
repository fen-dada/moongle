import { createApp } from 'vue'
import App from './App.vue'

// 1. 导入我们创建的 router
import router from './router' // 会自动寻找 router 文件夹下的 index.js

const app = createApp(App)

// 2. 使用 router
app.use(router)

app.mount('#app')

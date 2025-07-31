import { createRouter, createWebHistory } from 'vue-router';

// 1. 导入你想要作为页面的组件
// 我们稍后会创建这个 HomeView.vue 文件
import HomeView from '../views/HomeView.vue';

// 2. 定义路由
// 每个路由都需要映射到一个组件。
const routes = [
  {
    path: '/',      // 路由路径
    name: 'Home',   // 路由名称（可选）
    component: HomeView // 对应的组件
  },
  {
    path: '/examples',
    name: 'Examples',
    // 使用路由懒加载，优化性能。访问该页面时才会加载对应组件。
    component: () => import('../views/ExamplesView.vue')
  },
  {
    path: '/stats',
    name: 'Stats',
    component: () => import('../views/StatsView.vue')
  },
  {
    path: '/search',
    name: 'Search', // 这个名字要和 HomeView.vue 中 router.push 使用的一致
    component: () => import('../views/SearchView.vue')
  }
  // 你可以在这里添加更多的路由，例如：
  // {
  //   path: '/about',
  //   name: 'About',
  //   component: () => import('../views/AboutView.vue') // 懒加载方式
  // }
];

// 3. 创建 router 实例
const router = createRouter({
  // 使用 history 模式，让 URL 看上去更自然
  // 如果部署时有困难，可以换成 createWebHashHistory()
  history: createWebHistory(),
  routes, // `routes: routes` 的缩写
});

// 4. 导出 router
export default router;

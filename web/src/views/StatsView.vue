<script setup lang="ts">
import { ref, onMounted } from 'vue'

interface Stats {
  packages: number
  modules: number
  functions: number
  lastIndexed: string
}

const stats = ref<Stats | null>(null)
const loading = ref(true)
const error = ref<string | null>(null)

async function fetchStats() {
  loading.value = true
  error.value = null
  try {
    const response = await fetch('/api/stats')

    if (!response.ok) {
      // Handle HTTP errors like 404, 500, etc.
      if (response.status === 500) {
        error.value = '服务器内部错误 (500)，请稍后重试或联系管理员。'
      } else {
        const errorText = await response.text();
        error.value = `服务器返回了非预期的响应: ${response.status} - ${errorText}`;
      }
      // Stop execution since we have an error
      return;
    }

    // Only attempt to parse JSON if the response was OK.
    const data = await response.json();

    if (data.status === 'ok' && data.dat) {
      const receivedStats = data.dat;
      // Basic validation of the data structure
      if (
        typeof receivedStats.decls=== 'number' &&
        typeof receivedStats.modules === 'number' &&
        typeof receivedStats.lastIndexed === 'string'
      ) {
        stats.value = receivedStats;
      } else {
        error.value = '从服务器收到了无效的统计数据格式。';
      }
    } else {
      error.value = data.err?.message || '获取统计数据失败。';
    }
  } catch (e: any) {
    // This will now primarily catch network errors or actual JSON parsing errors
    // on a 2xx response.
    if (e instanceof SyntaxError) {
      error.value = '无法将服务器的成功响应解析为JSON。';
    } else {
      error.value = `请求失败: ${e.message}`;
    }
  } finally {
    loading.value = false
  }
}

function formatDateTime(isoString: string) {
  if (!isoString) return 'N/A';
  return new Date(isoString).toLocaleString();
}

onMounted(() => {
  fetchStats()
})
</script>

<template>
  <div class="page-container">
    <h1>统计数据</h1>
    <div v-if="loading" class="loading-indicator">
      <p>加载中...</p>
    </div>
    <div v-else-if="error" class="error-message">
      <p>加载数据失败: {{ error }}</p>
      <button @click="fetchStats">重试</button>
    </div>
    <div v-else-if="stats" class="stats-grid">
      <div class="stat-item">
        <div class="stat-value">{{ stats.decls.toLocaleString() }}</div>
        <div class="stat-label">Declarations</div>
      </div>
      <div class="stat-item">
        <div class="stat-value">{{ stats.modules.toLocaleString() }}</div>
        <div class="stat-label">Modules</div>
      </div>
      <div class="stat-item stat-item-full">
        <div class="stat-label">最后索引时间</div>
        <div class="stat-value-small">{{ formatDateTime(stats.lastIndexed) }}</div>
      </div>
    </div>
  </div>
</template>

<style scoped>
.page-container {
  display: flex;
  flex: 1;
  flex-direction: column;
  align-items: center;
  justify-content: flex-start;
  width: 100%;
  padding: 2rem;
  box-sizing: border-box;
  text-align: center;
}

h1 {
  margin-bottom: 2rem;
}

.loading-indicator,
.error-message {
  margin-top: 2rem;
}

.error-message button {
  margin-top: 1rem;
  padding: 0.5rem 1rem;
  cursor: pointer;
}

.stats-grid {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 2rem;
  width: 100%;
  max-width: 800px;
}

.stat-item {
  background-color: #f9f9f9;
  padding: 2rem;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}

.stat-item-full {
  grid-column: 1 / -1;
}

.stat-value {
  font-size: 2.5rem;
  font-weight: bold;
}

.stat-value-small {
  font-size: 1.2rem;
  margin-top: 0.5rem;
}

.stat-label {
  font-size: 1rem;
  color: #666;
  margin-top: 0.5rem;
}
</style>

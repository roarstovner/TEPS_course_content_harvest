import { svelte } from '@sveltejs/vite-plugin-svelte'
import { defineConfig } from 'vite'

// base './': the built site works from any path (GitHub Pages project site,
// a local folder served by any static server).
export default defineConfig({
  base: './',
  plugins: [svelte()],
  worker: { format: 'es' },
  test: { include: ['src/**/*.test.js'] },
})

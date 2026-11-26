import { readdirSync, readFileSync } from 'node:fs'
import { afterEach, expect, it } from 'vitest'
import { num, pct } from './format.js'
import { SHORTCUTS, setLang, t } from './i18n.js'

afterEach(() => setLang('en'))

it('has every key the components use, in both languages', () => {
  const dir = new URL('.', import.meta.url)
  const src = readdirSync(dir)
    .filter((f) => f.endsWith('.svelte'))
    .map((f) => readFileSync(new URL(f, dir), 'utf8'))
    .concat(readFileSync(new URL('../App.svelte', dir), 'utf8'))
    .join('\n')
  const keys = new Set([...src.matchAll(/\btr?\('([\w.]+)'/g)].map((m) => m[1]))
  keys.add('app.title')
  for (const group of Object.values(SHORTCUTS)) for (const [, k] of group) keys.add(k)
  expect(keys.size).toBeGreaterThan(100)
  for (const lang of ['en', 'nb']) {
    setLang(lang)
    const missing = [...keys].filter((k) => t(k) === k)
    expect(missing, lang).toEqual([])
  }
})

it('fills in parameters and formats numbers in the language', () => {
  expect(t('r.hits', { n: 3 })).toBe('3 hits')
  setLang('nb')
  expect(t('r.hits', { n: 3 })).toBe('3 treff')
  expect(pct(1, 3)).toBe('33,3 %')
  expect(num(1234)).toBe((1234).toLocaleString('nb'))
})

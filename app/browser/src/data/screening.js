// Screening: a seeded random sample of the hits (keyword in context) and the
// tags a reader gives them (include, exclude, maybe, a note), kept in the
// browser and exported as CSV (README "Layout", Screening).

import { toCsv } from '../download/encode.js'

/** Tags of a hit, with their keys in the screening view. */
export const TAGS = { include: 'i', exclude: 'e', maybe: 'm' }

/** A random generator in [0, 1) from a seed string (FNV-1a hash into
 *  mulberry32): the same seed gives the same numbers in every browser. */
export function seededRandom(seed) {
  let h = 0x811c9dc5
  for (const ch of String(seed)) h = Math.imul(h ^ ch.codePointAt(0), 0x01000193)
  let a = h >>> 0
  return () => {
    a = (a + 0x6d2b79f5) | 0
    let t = Math.imul(a ^ (a >>> 15), 1 | a)
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296
  }
}

/** A new seed for the URL: six digits. */
export const newSeed = () => String(100000 + Math.floor(Math.random() * 900000))

/**
 * `n` of the indices 0..total-1, without replacement, each equally likely
 * (a partial Fisher-Yates shuffle), in ascending order, so that the hits of
 * a plan stay together. All indices when `n >= total`.
 * @param {number} total
 * @param {number} n
 * @param {string} seed
 * @returns {number[]}
 */
export function sampleIndices(total, n, seed) {
  if (n >= total) return Array.from({ length: total }, (_, i) => i)
  const rnd = seededRandom(seed)
  const idx = new Int32Array(total).map((_, i) => i)
  for (let i = 0; i < n; i++) {
    const j = i + Math.floor(rnd() * (total - i))
    ;[idx[i], idx[j]] = [idx[j], idx[i]]
  }
  return [...idx.subarray(0, n)].sort((a, b) => a - b)
}

/** The identity of a hit (a row of the download unit `hit`): plan version,
 *  term, section and position. */
export const hitId = (h) => [h.plan_id, h.term, h.section ?? '', h.position].join('|')

/** The storage key of the tags of a query in a frame: tags of different
 *  queries (with their scope and unticked word forms) or frames never mix;
 *  the filters, sample size and seed do not change a hit's identity. */
export function storeKey(frame, { q, scope, n, excl }) {
  const ex = Object.entries(excl ?? {})
    .filter(([, f]) => f.length)
    .map(([k, f]) => [k, [...f].sort()])
    .sort(([a], [b]) => (a < b ? -1 : 1))
  return `screening:${frame}:${JSON.stringify([q, scope, scope === 'words' ? n : null, ex])}`
}

/** The tags under a key: {hitId: {tag, note}}; {} without storage. */
export function loadTags(key, store) {
  try {
    store ??= globalThis.localStorage // the access itself throws where storage is blocked
    const v = JSON.parse(store.getItem(key) ?? '{}')
    return v && typeof v === 'object' ? v : {}
  } catch {
    return {}
  }
}

/** Store the tags under a key (empty: removed); false when storage fails. */
export function saveTags(key, tags, store) {
  try {
    store ??= globalThis.localStorage
    if (Object.keys(tags).length) store.setItem(key, JSON.stringify(tags))
    else store.removeItem(key)
    return true
  } catch {
    return false
  }
}

/** Columns of the screening CSV. */
export const CSV_COLS = [
  'sample_rank', 'institution', 'course_code', 'course_name', 'plan_id', 'year_from', 'year_to',
  'section', 'term', 'position', 'before', 'match', 'after', 'tag', 'note', 'query', 'seed',
]

/**
 * The screened hits as CSV (encode.js `toCsv`), one row per hit in the
 * sample, untagged ones included (tag empty).
 * @param {object[]} hits rows of the download unit `hit`
 * @param {Object<string, {tag, note}>} tags
 * @param {{q: string, seed: string}} info
 */
export function screeningCsv(hits, tags, { q, seed }) {
  const rows = hits.map((h, i) => {
    const t = tags[hitId(h)] ?? {}
    return { ...h, sample_rank: i + 1, tag: t.tag ?? null, note: t.note || null, query: q, seed }
  })
  return toCsv(CSV_COLS, rows)
}

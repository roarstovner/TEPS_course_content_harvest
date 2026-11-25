// Holds the plan text and runs the search engine, off the main thread. The
// page gets the plans' metadata, search results and the text of the plan it
// shows; the text itself stays here (README "Data Contract"), each text once
// (search/text.js `FoldedText`). The sections' text is read on first need:
// a section-based query, the Sections tab or margin of an opened plan, a
// section download. Searches and word forms run in chunks, so that a newer
// request supersedes an older one (search/chunks.js).
//
// Events (no id): {type: 'progress', file, loaded, total}, {type: 'plans',
// plans, ms}, {type: 'sectionIndex', has: [[plan key, [section codes]]]},
// {type: 'sectionsLoading'}, {type: 'sections', ms}, {type: 'error', message}.
// Requests ({id, type, ...}) are answered with {id, result} or {id, error}.

import { chunked, compileQuery, FoldedText, generations, highlights, kwic, mergeForms, pause, prepareDocs, search, wordForms } from './search/index.js'
import { fetchCached, parquetInfo, readParquet } from './data/load.js'
import { planKey } from './data/keys.js'
import { sampleIndices } from './data/screening.js'

const CHUNK = 1000 // plans per chunk of a search
let docs = []
const byKey = new Map()
let plansDone
const plansReady = new Promise((r) => (plansDone = r))
let sectionsFile // promise of the bytes of sections.parquet
let sectionsReady = null // promise, from the first need
let sectionsLoaded = false
const newSearch = generations()
const newForms = generations()
let scans = 0 // searches and word forms running

// a search or word forms over plans in chunks; null when superseded
async function scan(items, fn, stale) {
  scans++
  try {
    return await chunked(items, CHUNK, fn, stale)
  } finally {
    scans--
  }
}
// the next turn of the event loop with no scan running (reading the sections
// gives way to searches)
async function idle() {
  do await pause()
  while (scans)
}

// The rows of a Parquet file without its text column, and that column as
// FoldedText, folded as it is decoded (the decoded strings never pile up), in
// slices of rows with `wait()` between them (other requests go on meanwhile).
async function readWithText(buf, col, wait = pause, slice = 10000) {
  const { columns, rows: n } = parquetInfo(buf)
  const rows = await readParquet(buf, columns.filter((c) => c !== col))
  const dec = new TextDecoder()
  const parsers = { stringFromBytes: (b) => new FoldedText(dec.decode(b)) }
  const texts = []
  for (let s = 0; s < n; s += slice) {
    if (s) await wait()
    for (const r of await readParquet(buf, [col], { parsers, rowStart: s, rowEnd: Math.min(n, s + slice) })) texts.push(r[col])
  }
  return [rows, texts]
}

async function load({ base, version }) {
  const progress = (file) => (loaded, total) => postMessage({ type: 'progress', file, loaded, total })
  const url = (f) => new URL(f, base).href
  const buf = await fetchCached(url('plans.parquet'), version, progress('plans'))
  const t = performance.now()
  const [rows, texts] = await readWithText(buf, 'course_plan')
  docs = prepareDocs(rows.map((r, i) => ({ id: planKey(r), text: texts[i] })))
  for (const d of docs) byKey.set(d.id, d)
  sectionsFile = fetchCached(url('sections.parquet'), version, progress('sections'))
  plansDone()
  postMessage({ type: 'plans', plans: rows, ms: performance.now() - t })

  // which sections each plan has (the sections filter), without their text
  const has = new Map()
  const sbuf = await sectionsFile
  for (const r of await readParquet(sbuf, parquetInfo(sbuf).columns.filter((c) => c !== 'text'))) {
    const k = planKey(r)
    if (!has.has(k)) has.set(k, [])
    has.get(k).push(r.section)
  }
  postMessage({ type: 'sectionIndex', has: [...has] })
}

/** Read the sections' text into the plans, once (after the plans). */
function loadSections() {
  if (sectionsReady) return sectionsReady
  sectionsReady = (async () => {
    postMessage({ type: 'sectionsLoading' })
    const buf = await sectionsFile
    const t = performance.now()
    const [rows, texts] = await readWithText(buf, 'text', idle)
    rows.forEach((r, i) => {
      const d = byKey.get(planKey(r))
      if (d && texts[i]?.lc) d.sections[r.section] = texts[i]
    })
    sectionsLoaded = true
    postMessage({ type: 'sections', ms: performance.now() - t })
  })()
  sectionsReady.catch((e) => postMessage({ type: 'error', message: String(e?.message ?? e) }))
  return sectionsReady
}

let last = { q: null, m: null }
const matcherOf = (q) => (last.q === q ? last.m : (last = { q, m: compileQuery(q) }).m)

// a matcher that needs the sections' text waits for them
async function ready(q, scope) {
  await plansReady
  const m = matcherOf(q)
  if (!m.error && (scope === 'section' || m.terms.some((t) => t.section))) await loadSections()
  return m
}

const handlers = {
  /** Matching plans: {matches: [[plan key, counts]], ms}, {error}, or
   *  {superseded: true} when a newer search came first. */
  async search({ q, scope, n, excl }) {
    const stale = newSearch()
    newForms()
    const m = await ready(q, scope)
    if (m.error) return { error: m.error }
    const t = performance.now()
    const opts = { scope, words: n || 10, excludedForms: excl }
    const parts = await scan(docs, (ds) => search(ds, m, opts), stale)
    if (!parts) return { superseded: true }
    return { matches: parts.flat().map((r) => [r.id, r.counts]), ms: performance.now() - t }
  },
  /** The word forms of each term over the plans `keys` (those in the
   *  filters; asked after `search`, so that the results show first); null
   *  when superseded. */
  async forms({ q, keys }) {
    const stale = newForms()
    const m = await ready(q, 'plan')
    if (m.error) return []
    const ds = keys.map((k) => byKey.get(k)).filter(Boolean)
    const parts = await scan(ds, (d) => wordForms(d, m), stale)
    return parts && mergeForms(parts)
  },
  /** Stop a running search and word forms (the query was cleared). */
  cancel() {
    newSearch()
    newForms()
  },
  /** Keyword in context for some plans: {key: [{before, match, after, ...}]}. */
  async kwic({ q, excl, keys, max = 2 }) {
    const m = await ready(q, 'plan')
    const out = {}
    if (m.error || !m.ast) return out
    for (const k of keys) {
      const d = byKey.get(k)
      if (d) out[k] = kwic(d, m, { excludedForms: excl, max, context: 50 })
    }
    return out
  },
  /** The text of one plan and of its sections, with the ranges to mark. */
  async doc({ key, q, excl }) {
    await plansReady
    const d = byKey.get(key)
    if (!d) return null
    const m = q ? matcherOf(q) : null
    const marks = (text, section) => (m && m.ast ? highlights(text, m, { section, excludedForms: excl }) : [])
    const shown = (s, code) => {
      const text = s.text // rebuilt from the folded copy
      return { text, marks: marks(text, code) }
    }
    let sections = null // null: not loaded yet (the margin and the tab ask for them)
    if (sectionsLoaded) sections = Object.fromEntries(Object.entries(d.sections).map(([code, s]) => [code, shown(s, code)]))
    else loadSections()
    return { ...shown(d, null), sections }
  },
  /** A seeded random sample of `size` hits of the selection (rows of the
   *  download unit `hit`): {total, hits} (data/screening.js). */
  async sample({ sel, ctx, size, seed }) {
    const rows = await downloadRows({ unit: 'hit', sel, ctx })
    return { total: rows.length, hits: sampleIndices(rows.length, size, seed).map((i) => rows[i]) }
  },
  /** Rows of a download and the texts over Excel's cell limit: {rows, long}. */
  async downloadCount(args) {
    const [rows, { EXCEL_MAX }] = await Promise.all([downloadRows(args), import('./download/encode.js')])
    return { rows: rows.length, long: rows.filter((r) => (r.course_plan ?? r.text)?.length > EXCEL_MAX).length }
  },
  /** A download as {name, blob} (download/bundle.js). */
  async download(args) {
    const [rows, { buildDownload }, { default: dictionary }] = await Promise.all([
      downloadRows(args),
      import('./download/bundle.js'),
      import('./download/dictionary.js'),
    ])
    if (!rows.length) throw new Error('Nothing to download')
    const { name, bytes, truncated } = await buildDownload({ ...args, rows, dictionary })
    return { name, truncated, blob: new Blob([bytes], { type: 'application/zip' }) }
  },
}

// The table of a download unit, with the text this worker holds.
async function downloadRows({ unit, sel, ctx, sectionOrder }) {
  const { unitRows, TEXT_UNITS } = await import('./download/table.js')
  if (!TEXT_UNITS.has(unit)) return unitRows(unit, sel)
  const m = ctx.q ? await ready(ctx.q, ctx.scope) : null
  if (unit === 'section') await loadSections()
  const on = m && !m.error && m.ast
  const excludedForms = ctx.excl
  const hits = (k, section) => {
    const d = byKey.get(k)
    return d && on ? kwic(d, m, { excludedForms, section, max: Infinity, context: 80 }) : []
  }
  return unitRows(
    unit,
    sel,
    {
      plan: (k) => byKey.get(k)?.text,
      sections: (k) => Object.fromEntries(Object.entries(byKey.get(k)?.sections ?? {}).map(([c, s]) => [c, s.text])),
      sectionHits: on
        ? (k) => Object.fromEntries(Object.keys(byKey.get(k)?.sections ?? {}).map((c) => [c, hits(k, c).filter((h) => h.source === c).length]))
        : null,
      hits: (k) => hits(k, null).map((h) => ({ ...h, term: m.terms[h.term].key })),
    },
    sectionOrder,
  )
}

self.onmessage = async ({ data }) => {
  if (data.type === 'load') {
    load(data).catch((e) => postMessage({ type: 'error', message: String(e?.message ?? e) }))
    return
  }
  try {
    postMessage({ id: data.id, result: await handlers[data.type](data) })
  } catch (e) {
    postMessage({ id: data.id, error: String(e?.message ?? e) })
  }
}

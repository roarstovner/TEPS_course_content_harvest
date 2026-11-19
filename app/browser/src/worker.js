// Holds the plan text and runs the search engine, off the main thread. The
// page gets the plans' metadata, search results and the text of the plan it
// shows; the text itself stays here (README "Data Contract").
//
// Events (no id): {type: 'progress', file, loaded, total}, {type: 'plans',
// plans, ms}, {type: 'sections', ms, has: [[plan key, [section codes]]]},
// {type: 'error', message}.
// Requests ({id, type, ...}) are answered with {id, result} or {id, error}.

import { compileQuery, fold, highlights, kwic, prepareDocs, search, wordForms } from './search/index.js'
import { fetchCached, readParquet } from './data/load.js'
import { planKey } from './data/keys.js'

let docs = []
const byKey = new Map()
let plansDone
let sectionsDone
let sectionsLoaded = false
const plansReady = new Promise((r) => (plansDone = r))
const sectionsReady = new Promise((r) => (sectionsDone = r))

async function load({ base, version }) {
  const progress = (file) => (loaded, total) => postMessage({ type: 'progress', file, loaded, total })
  const rows = await readParquet(await fetchCached(new URL('plans.parquet', base).href, version, progress('plans')))
  let t = performance.now()
  docs = prepareDocs(rows.map((r) => ({ id: planKey(r), text: r.course_plan })))
  for (const d of docs) byKey.set(d.id, d)
  plansDone()
  postMessage({ type: 'plans', plans: rows.map(({ course_plan, ...m }) => m), ms: performance.now() - t })

  const srows = await readParquet(await fetchCached(new URL('sections.parquet', base).href, version, progress('sections')))
  t = performance.now()
  for (const r of srows) {
    const d = byKey.get(planKey(r))
    if (d && r.text) d.sections[r.section] = { text: r.text, lc: fold(r.text) }
  }
  sectionsLoaded = true
  sectionsDone()
  postMessage({ type: 'sections', ms: performance.now() - t, has: docs.map((d) => [d.id, Object.keys(d.sections)]) })
}

let last = { q: null, m: null }
const matcherOf = (q) => (last.q === q ? last.m : (last = { q, m: compileQuery(q) }).m)

// a matcher that needs the sections' text waits for them
async function ready(q, scope) {
  await plansReady
  const m = matcherOf(q)
  if (!m.error && (scope === 'section' || m.terms.some((t) => t.section))) await sectionsReady
  return m
}

const handlers = {
  /** Matching plans: {matches: [[plan key, counts]], ms} or {error}. */
  async search({ q, scope, n, excl }) {
    const m = await ready(q, scope)
    if (m.error) return { error: m.error }
    const t = performance.now()
    const res = search(docs, m, { scope, words: n || 10, excludedForms: excl })
    return { matches: res.map((r) => [r.id, r.counts]), ms: performance.now() - t }
  },
  /** The word forms of each term, over all plans (asked after `search`, so
   *  that the results show first). */
  async forms({ q }) {
    const m = await ready(q, 'plan')
    return m.error ? [] : wordForms(docs, m)
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
    let sections = null // null: not loaded yet
    if (sectionsLoaded) {
      sections = {}
      for (const [code, s] of Object.entries(d.sections)) sections[code] = { text: s.text, marks: marks(s.text, code) }
    }
    return { text: d.text, marks: marks(d.text, null), sections }
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
  if (unit === 'section') await sectionsReady
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

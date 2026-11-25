// Saved term lists: named lists of queries (a term with its Norwegian and
// English variants), kept in the browser, read from and written to YAML in
// the format of LK20_terms.yaml (README "Term Lists"), and run together as a
// heatmap term x institution x year, counted as the trend view counts.

import { toCsv } from '../download/encode.js'
import { yearFilter } from './model.js'
import { add, blank, eachCourseYear, share } from './trend.js'

const asList = (v) => (v == null ? [] : Array.isArray(v) ? v.map(String) : [String(v)])
const isObj = (v) => v != null && typeof v === 'object' && !Array.isArray(v)

/** A variant as a query term: a phrase (words with spaces) quoted, anything
 *  else as written (a word, a wildcard, a quoted term or a /regex/). */
const variantTerm = (v) => (/\s/.test(v) && !/^(".*"|\/.*\/)$/.test(v) ? `"${v.replace(/"/g, '')}"` : v)

/**
 * The query of a term from its fields: `query` (the browser's syntax) when
 * given, else `regex` as a regular expression, else its variants
 * (`variants`, `nynorsk`, `english`; a string or a list each) joined by OR.
 * @returns {string} '' when the term has none
 */
export function termQuery(t) {
  if (typeof t.query === 'string' && t.query.trim()) return t.query.trim()
  if (typeof t.regex === 'string' && t.regex) return `/${t.regex}/`
  return [...asList(t.variants), ...asList(t.nynorsk), ...asList(t.english)]
    .map((v) => v.trim())
    .filter(Boolean)
    .map(variantTerm)
    .join(' OR ')
}

/** An id from a label: lower case, runs of other characters as `_`. */
export const slug = (s) =>
  String(s)
    .toLowerCase()
    .replace(/[^\p{L}\p{N}]+/gu, '_')
    .replace(/^_|_$/g, '') || 'term'

/**
 * The terms of a list in any of the accepted shapes: a list of term objects
 * (LK20_terms.yaml), a list of strings, or a map from a term to its query (a
 * string), its variants (a list) or its fields (a map).
 * @returns {{id, label, query, ...}[]} every field kept, `query` set
 */
export function toTerms(v) {
  const term = (fields, id) => {
    const t = { ...fields }
    t.id = String(t.id ?? id ?? t.label ?? '')
    t.label = String(t.label ?? t.id)
    t.query = termQuery(t)
    return t
  }
  const items = Array.isArray(v)
    ? v.map((x) => (isObj(x) ? term(x) : term({ query: variantTerm(String(x)) }, String(x))))
    : isObj(v)
      ? Object.entries(v).map(([k, x]) =>
          isObj(x) ? term(x, k) : Array.isArray(x) ? term({ variants: x }, k) : term({ query: x == null ? '' : String(x) }, k),
        )
      : []
  return items.map((t, i) => (t.id ? t : { ...t, id: `term${i + 1}`, label: t.label || `term${i + 1}` }))
}

/**
 * The lists in a YAML text. A document with `terms` is one list (named by
 * its `name`, else `name`), its other top-level fields kept as `meta`
 * (LK20_terms.yaml's `categories`); without `terms`, each top-level entry
 * is a list: `{list name: {term: [variants]}}` or `{list name: {terms: ...}}`.
 * @param {object} [yaml] the yaml module; loaded when not given
 * @returns {Promise<{name, terms, meta}[]>} lists with at least one term
 * @throws when the text is not YAML or holds no term
 */
export async function parseTermLists(text, name, yaml) {
  yaml ??= await import('yaml')
  const doc = yaml.parse(text)
  if (!isObj(doc)) throw new Error('Not a term list: expected a YAML map')
  const list = (n, v) => {
    const { name: own, terms, ...meta } = isObj(v) && 'terms' in v ? v : { terms: v }
    return { name: String(own ?? n), terms: toTerms(terms), meta }
  }
  const lists = 'terms' in doc ? [list(name, doc)] : Object.entries(doc).map(([n, v]) => list(n, v))
  const out = lists.filter((l) => l.terms.length)
  if (!out.length) throw new Error('No terms found')
  return out
}

/**
 * A list as YAML that `parseTermLists` reads back and R reads with
 * `yaml::read_yaml()`: `name`, the list's other fields, `terms` (each with
 * all its fields; `query` left out where it is the one `regex` or the
 * variants give, so an imported LK20_terms.yaml keeps its shape).
 * @param {object} [yaml] the yaml module; loaded when not given
 */
export async function serializeTermList({ name, terms, meta = {} }, yaml, date = new Date()) {
  yaml ??= await import('yaml')
  const out = terms.map((t) => {
    const { query, ...rest } = t
    return query && query !== termQuery({ ...rest, query: null }) ? { ...rest, query } : rest
  })
  const doc = new yaml.Document({ name, ...meta, terms: out })
  doc.commentBefore =
    ` Term list "${name}", exported from the course plan browser ${date.toISOString().slice(0, 10)}.\n` +
    ' query: the browser\'s query syntax; without it, regex, else variants / nynorsk / english joined by OR.'
  return doc.toString({ lineWidth: 0 })
}

/** The storage key of the term lists. */
export const STORE_KEY = 'termlists'

/** The saved lists: {name: {terms, meta}}; {} without storage. */
export function loadLists(store) {
  try {
    store ??= globalThis.localStorage // the access itself throws where storage is blocked
    const v = JSON.parse(store.getItem(STORE_KEY) ?? '{}')
    return isObj(v) ? v : {}
  } catch {
    return {}
  }
}

/** Save the lists; false when storage fails. */
export function saveLists(lists, store) {
  try {
    store ??= globalThis.localStorage
    store.setItem(STORE_KEY, JSON.stringify(lists))
    return true
  } catch {
    return false
  }
}

/**
 * The heatmap: per term, by year, the course-years in the filter (course
 * filters, years, study types) counted as the trend view counts them, overall
 * and per institution.
 * @param {(Set<string>|null)[]} sets per term the plan keys that match (null: not run)
 * @returns {{years: number[], institutions: string[], terms: ({all: object[], inst: Map<string, object[]>}|null)[]}}
 *   points `{year, n, withPlan, matching}`, one per year
 */
export function termMatrix(model, f, sets) {
  const { y0, y1 } = yearFilter(model, f)
  const years = []
  for (let y = y0; y <= y1; y++) years.push(y)
  const row = () => years.map(blank)
  const terms = sets.map((s) => (s ? { all: row(), inst: new Map() } : null))
  const institutions = new Set()
  eachCourseYear(model, f, (c, cy) => {
    institutions.add(c.institution)
    const i = cy.year - y0
    sets.forEach((s, t) => {
      if (!s) return
      const m = terms[t]
      add(m.all[i], cy, s)
      if (!m.inst.has(c.institution)) m.inst.set(c.institution, row())
      add(m.inst.get(c.institution)[i], cy, s)
    })
  })
  return { years, institutions: [...institutions].sort(), terms }
}

/** Columns of the heatmap CSV. */
export const MATRIX_COLS = ['list', 'term_id', 'term_label', 'query', 'scope', 'institution', 'year', 'course_years', 'with_plan', 'matching', 'share']

/**
 * The heatmap as CSV (encode.js `toCsv`), one row per term, institution
 * (`all` for the total) and year: course-years in the filter, those with a
 * plan, those whose plan matches, and the share (matching of with a plan;
 * empty without plans).
 */
export function matrixCsv(name, terms, matrix, scope) {
  const rows = []
  terms.forEach((t, k) => {
    const m = matrix.terms[k]
    if (!m) return
    const base = { list: name, term_id: t.id, term_label: t.label, query: t.query, scope }
    for (const [inst, pts] of [['all', m.all], ...matrix.institutions.map((i) => [i, m.inst.get(i)])])
      for (const p of pts ?? [])
        rows.push({ ...base, institution: inst, year: p.year, course_years: p.n, with_plan: p.withPlan, matching: p.matching, share: share(p.matching, p.withPlan) })
  })
  return toCsv(MATRIX_COLS, rows)
}

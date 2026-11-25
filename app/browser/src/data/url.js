// The app state in the URL query string, so that every view can be
// bookmarked and cited. Only values that differ from the defaults are written.

export const SCOPES = ['plan', 'section', 'paragraph', 'sentence', 'words']
export const TABS = ['plan', 'sections', 'versions', 'source']
export const VIEWS = ['list', 'trend', 'coverage', 'screen', 'compare', 'terms', 'help']
export const COMPARE_MODES = ['pair', 'across']
export const BY = ['all', 'inst', 'st']

export const defaultState = () => ({
  q: '', // query
  scope: 'plan', // scope of AND
  n: 10, // words, for scope 'words'
  excl: {}, // excluded word forms: {termKey: [form, ...]}
  inst: [], // institutions; empty = all
  y0: null, // year range; null = open
  y1: null,
  st: [], // study types; empty = all
  subj: [], // DBH subject names (repeated `subj`: names hold commas)
  lvl: [], // level codes
  cr: [], // credit ranges (filters.js)
  lang: [], // language codes
  sec: [], // sections a plan must all have
  course: null, // selected course, `institution|course_code`
  version: null, // its version (versionId)
  tab: 'plan', // one of TABS
  cmp: null, // the version compared with it, tab 'versions' (versionId)
  view: 'list', // middle pane, one of VIEWS ('help': the user guide)
  by: 'all', // trend lines: overall, per institution or per study type (BY)
  sample: 50, // screening: hits in the random sample
  seed: null, // screening: the sample's seed (data/screening.js)
  cm: 'pair', // compare: two plans side by side, or one year across institutions (COMPARE_MODES)
  ca: null, // compare: plan A, `institution|course_code@versionId` (data/compare.js)
  cb: null, // compare: plan B
  csec: null, // compare: one section only (its code); null = the whole plan
  cyear: null, // compare across: the year; null = the last year of the filter
  diff: true, // compare two plans: mark the differences
  tl: null, // term lists: the list shown (its name; the lists are kept in the browser)
  trows: 'all', // term heatmap rows: a term ('all') or a term per institution ('inst')
  tval: 'share', // term heatmap cells: 'share' of course-years with a plan, or 'count'
})

const list = (s) => (s ? s.split(',').filter(Boolean) : [])
const year = (s) => (/^\d{4}$/.test(s ?? '') ? Number(s) : null)

/** State -> query string (without '?'). */
export function encodeState(s) {
  const p = new URLSearchParams()
  if (s.q) p.set('q', s.q)
  if (s.scope !== 'plan') p.set('scope', s.scope)
  if (s.scope === 'words' && Number.isInteger(s.n) && s.n !== 10) p.set('n', String(s.n))
  const ex = Object.entries(s.excl).filter(([, f]) => f.length)
  if (ex.length) p.set('x', JSON.stringify(Object.fromEntries(ex)))
  if (s.inst.length) p.set('inst', s.inst.join(','))
  if (s.y0 != null) p.set('from', String(s.y0))
  if (s.y1 != null) p.set('to', String(s.y1))
  if (s.st.length) p.set('st', s.st.join(','))
  for (const v of s.subj) p.append('subj', v)
  for (const k of ['lvl', 'cr', 'lang', 'sec']) if (s[k].length) p.set(k, s[k].join(','))
  if (s.view !== 'list') p.set('view', s.view)
  if (s.view === 'trend' && s.by !== 'all') p.set('by', s.by)
  if (s.view === 'screen') {
    if (Number.isInteger(s.sample) && s.sample !== 50) p.set('sample', String(s.sample))
    if (s.seed) p.set('seed', s.seed)
  }
  if (s.view === 'compare') {
    if (s.cm !== 'pair') p.set('cm', s.cm)
    // `inst:code@version`; institution codes hold no ':'
    if (s.cm === 'pair') for (const k of ['ca', 'cb']) if (s[k]) p.set(k, s[k].replace('|', ':'))
    if (s.cm === 'pair' && !s.diff) p.set('diff', '0')
    if (s.cm === 'across' && s.cyear != null) p.set('cyear', String(s.cyear))
    if (s.csec) p.set('csec', s.csec)
  }
  if (s.view === 'terms') {
    if (s.tl) p.set('tl', s.tl)
    if (s.trows === 'inst') p.set('trows', 'inst')
    if (s.tval === 'count') p.set('tval', 'count')
  }
  if (s.course) {
    // `inst:code`; institution codes hold no ':'
    p.set('course', s.course.replace('|', ':'))
    if (s.version) p.set('v', s.version)
    if (s.tab !== 'plan') p.set('tab', s.tab)
    if (s.tab === 'versions' && s.cmp) p.set('cmp', s.cmp)
  }
  return p.toString()
}

/** The state after an example link of the user guide (`?q=...&view=...`):
 *  its query, scope, sections filter, view and view settings; the other
 *  filters and the open course are kept. */
export function applyExample(s, search) {
  const d = decodeState(search)
  for (const k of ['q', 'scope', 'n', 'sec', 'view', 'by', 'sample', 'seed']) s[k] = d[k]
  return s
}

/** Query string -> state; invalid values fall back to the defaults. */
export function decodeState(search) {
  const p = new URLSearchParams(search)
  const s = defaultState()
  s.q = p.get('q') ?? ''
  if (SCOPES.includes(p.get('scope'))) s.scope = p.get('scope')
  const n = Number(p.get('n'))
  if (Number.isInteger(n) && n >= 1 && n <= 1000) s.n = n
  try {
    const x = JSON.parse(p.get('x') ?? '{}')
    for (const [k, f] of Object.entries(x)) if (Array.isArray(f)) s.excl[k] = f.filter((v) => typeof v === 'string')
  } catch {
    // ignore a broken x
  }
  s.inst = list(p.get('inst'))
  s.y0 = year(p.get('from'))
  s.y1 = year(p.get('to'))
  s.st = list(p.get('st'))
  s.subj = p.getAll('subj')
  for (const k of ['lvl', 'cr', 'lang', 'sec']) s[k] = list(p.get(k))
  if (VIEWS.includes(p.get('view'))) s.view = p.get('view')
  if (BY.includes(p.get('by'))) s.by = p.get('by')
  const k = Number(p.get('sample'))
  if (Number.isInteger(k) && k >= 1 && k <= 100000) s.sample = k
  if (/^[\w-]{1,40}$/.test(p.get('seed') ?? '')) s.seed = p.get('seed')
  if (COMPARE_MODES.includes(p.get('cm'))) s.cm = p.get('cm')
  for (const k of ['ca', 'cb']) if (/^[^:]+:.+@\w+$/.test(p.get(k) ?? '')) s[k] = p.get(k).replace(':', '|')
  s.csec = p.get('csec') || null
  s.cyear = year(p.get('cyear'))
  s.diff = p.get('diff') !== '0'
  s.tl = p.get('tl') || null
  if (p.get('trows') === 'inst') s.trows = 'inst'
  if (p.get('tval') === 'count') s.tval = 'count'
  const c = p.get('course')
  if (c && c.includes(':')) {
    s.course = c.replace(':', '|')
    s.version = p.get('v') || null
    if (TABS.includes(p.get('tab'))) s.tab = p.get('tab')
    s.cmp = p.get('cmp') || null
  }
  return s
}

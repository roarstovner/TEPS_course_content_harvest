// The app state in the URL query string, so that every view can be
// bookmarked and cited. Only values that differ from the defaults are written.

export const SCOPES = ['plan', 'section', 'paragraph', 'sentence', 'words']

export const defaultState = () => ({
  q: '', // query
  scope: 'plan', // scope of AND
  n: 10, // words, for scope 'words'
  excl: {}, // excluded word forms: {termKey: [form, ...]}
  inst: [], // institutions; empty = all
  y0: null, // year range; null = open
  y1: null,
  st: [], // study types; empty = all
  course: null, // selected course, `institution|course_code`
  version: null, // its version (versionId)
  tab: 'plan', // 'plan' or 'sections'
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
  if (s.course) {
    // `inst:code`; institution codes hold no ':'
    p.set('course', s.course.replace('|', ':'))
    if (s.version) p.set('v', s.version)
    if (s.tab !== 'plan') p.set('tab', s.tab)
  }
  return p.toString()
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
  const c = p.get('course')
  if (c && c.includes(':')) {
    s.course = c.replace(':', '|')
    s.version = p.get('v') || null
    if (p.get('tab') === 'sections') s.tab = 'sections'
  }
  return s
}

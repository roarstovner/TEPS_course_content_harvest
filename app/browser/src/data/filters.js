// The course filters beside study type and years: institution, subject,
// level, credits and language (attributes of courses.parquet, one value per
// course), and the readable names of their values.

export const LEVELS = {
  LN: 'Lower degree',
  HN: 'Higher degree',
  AR: 'One-year programme',
  VS: 'Upper secondary',
  50: 'Older code (50)',
}

export const LANGUAGES = {
  NOR: 'Norwegian',
  NOB: 'Norwegian (Bokmål)',
  NNO: 'Norwegian (Nynorsk)',
  ENG: 'English',
  SME: 'Northern Sami',
  SMA: 'Southern Sami',
  SMJ: 'Lule Sami',
  SWE: 'Swedish',
  DAN: 'Danish',
  FIN: 'Finnish',
  FRE: 'French',
  GER: 'German',
  SPA: 'Spanish',
  ITA: 'Italian',
  RUS: 'Russian',
}

// credits in ranges: [key, label, test], the first that holds
const CREDIT_BINS = [
  ['0', '0', (x) => x === 0],
  ['lt7.5', '< 7.5', (x) => x < 7.5],
  ['7.5', '7.5–9.9', (x) => x < 10],
  ['10', '10–14.9', (x) => x < 15],
  ['15', '15–19.9', (x) => x < 20],
  ['20', '20–29.9', (x) => x < 30],
  ['30', '30', (x) => x === 30],
  ['gt30', '> 30', () => true],
]
export const CREDITS = Object.fromEntries(CREDIT_BINS.map(([k, l]) => [k, l]))
export const creditBin = (x) => (x == null || Number.isNaN(x) ? '' : CREDIT_BINS.find(([, , t]) => t(Number(x)))[0])

/** The course filters: state key -> value of a course ('' when missing). */
export const COURSE_FILTERS = {
  inst: (c) => c.institution,
  subj: (c) => c.subject_name ?? '',
  lvl: (c) => c.level_code ?? '',
  cr: (c) => creditBin(c.credits),
  lang: (c) => c.language_code ?? '',
}

/** The name of a filter value. */
export const valueLabel = (key, v) =>
  v === '' ? '(not given)' : ({ lvl: LEVELS, lang: LANGUAGES, cr: CREDITS }[key]?.[v] ?? v)

/** The values of a course filter in display order: credits by range, the
 *  others by number of courses. */
export function filterValues(courses, key) {
  const n = new Map()
  for (const c of courses) {
    const v = COURSE_FILTERS[key](c)
    n.set(v, (n.get(v) ?? 0) + 1)
  }
  if (key === 'cr') return CREDIT_BINS.map(([k]) => k).filter((k) => n.has(k)).concat(n.has('') ? [''] : [])
  return [...n.keys()].sort((a, b) => n.get(b) - n.get(a) || a.localeCompare(b, 'nb'))
}

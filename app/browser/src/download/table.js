// The tables of a download: the selection the list shows (the filters plus
// the query, with unticked word forms) as one row per course, course-year,
// plan version, section or hit. Pure functions; the text columns are added
// in the worker, which holds the text (README "Layout", Download).

import { planKey } from '../data/keys.js'
import { yearFilter } from '../data/model.js'

/** Units of a download, in the order the dialog offers them. */
export const UNITS = {
  course: 'course',
  plan: 'plan version',
  course_year: 'course-year',
  section: 'section',
  hit: 'hit (keyword in context)',
}

/** Formats of a download (bundle.js). */
export const FORMATS = {
  csv: 'CSV (UTF-8, Excel)',
  xlsx: 'XLSX (Excel)',
  parquet: 'Parquet (R, Python)',
  qda: 'QDA: text files + classification sheet (NVivo, MAXQDA, ATLAS.ti)',
}

/** Units whose rows hold plan or section text. */
export const TEXT_UNITS = new Set(['plan', 'section', 'hit'])

const join = (set) => [...set].sort().join('; ') || null
const sum = (a) => a.reduce((s, x) => s + x, 0)

/**
 * The selection as tables without text.
 * @param {object} model from `addPlans` (versions attached)
 * @param {{rows}} view from `filterView`
 * @param {object} f the filters (`inst`, `y0`, `y1`, `st`)
 * @param {Map|null} matches plan key -> hit counts per term; null without a query
 * @param {Object<string, string>} instNames institution code -> name
 * @returns {{courses, courseYears, plans, planKeys}} rows of each table;
 *   `planKeys`: the plan versions of the course-years in the selection (for
 *   `keys.csv` of the course and course-year units)
 */
export function selection(model, view, f, matches, instNames = {}) {
  const { cyIn } = yearFilter(model, f)
  const q = !!matches
  const courses = []
  const courseYears = []
  const plans = []
  const planKeys = new Map()
  for (const { course: c, versions } of view.rows) {
    const cys = [...c.years.values()].filter(cyIn).sort((a, b) => a.year - b.year)
    const byKey = new Map(c.versions.map((v) => [v.key, v]))
    const matching = (cy) => [...cy.plans].some((k) => matches?.has(k))
    const studyTypes = new Set(cys.flatMap((cy) => [...cy.studyTypes]))
    const base = { institution: c.institution, institution_name: instNames[c.institution] ?? null, course_code: c.course_code }
    courses.push({
      ...base,
      course_name: c.course_name,
      level_code: c.level_code,
      credits: c.credits,
      language_code: c.language_code,
      subject_code: c.subject_code,
      subject_name: c.subject_name,
      nus_code: c.nus_code,
      department_name: c.department_name,
      year_first: c.year_first,
      year_last: c.year_last,
      study_types: join(studyTypes),
      course_years: cys.length,
      course_years_with_plan: cys.filter((cy) => cy.plans.size).length,
      course_years_matching: q ? cys.filter(matching).length : null,
      plan_versions: versions.length,
      hits: q ? sum(versions.map((v) => sum(matches.get(v.key)))) : null,
    })
    for (const cy of cys) {
      for (const k of cy.plans) if (byKey.has(k)) planKeys.set(k, byKey.get(k))
      courseYears.push({
        ...base,
        course_name: c.course_name,
        year: cy.year,
        study_types: join(cy.studyTypes),
        programme_codes: join(cy.programmes),
        had_students: cy.hadStudents,
        has_plan: cy.plans.size > 0,
        plan_ids: join(cy.plans),
        matching: q ? matching(cy) : null,
        no_plan_reason: cy.noPlanReason,
      })
    }
    for (const v of versions) {
      const st = new Set(cys.filter((cy) => cy.plans.has(v.key)).flatMap((cy) => [...cy.studyTypes]))
      plans.push({
        plan_id: planKey(v),
        plan_content_id: v.plan_content_id,
        ...base,
        course_name: c.course_name,
        year_from: v.year_from,
        year_to: v.year_to,
        level_code: c.level_code,
        credits: c.credits,
        language_code: c.language_code,
        subject_code: c.subject_code,
        subject_name: c.subject_name,
        study_types: join(st),
        source_url: v.source_url ?? null,
        harvested_at: v.harvested_at ?? null,
        hits: q ? sum(matches.get(v.key)) : null,
      })
    }
  }
  return { courses, courseYears, plans, planKeys: [...planKeys.values()].map(keyRow) }
}

/** The key columns of a plan version, as written to `keys.csv`. */
export const keyRow = (p) => ({
  plan_id: planKey(p),
  plan_content_id: p.plan_content_id,
  institution: p.institution,
  course_code: p.course_code,
})

/**
 * The table of a unit, text included.
 * @param {string} unit one of `UNITS`
 * @param {object} sel from `selection`
 * @param {object} text accessors by plan key: `plan(key)` -> text,
 *   `sections(key)` -> {code: text}, `hits(key)` -> kwic lines
 *   ({term, source, start, before, match, after}); only those the unit needs
 * @param {string[]} [sectionOrder] section codes in display order
 * @returns {object[]} rows
 */
export function unitRows(unit, sel, text = {}, sectionOrder = []) {
  if (unit === 'course') return sel.courses
  if (unit === 'course_year') return sel.courseYears
  if (unit === 'plan') return sel.plans.map((p) => ({ ...p, course_plan: text.plan(p.plan_id) ?? null }))
  const head = (p) => ({
    plan_id: p.plan_id,
    plan_content_id: p.plan_content_id,
    institution: p.institution,
    course_code: p.course_code,
    course_name: p.course_name,
    year_from: p.year_from,
    year_to: p.year_to,
  })
  const out = []
  if (unit === 'section') {
    const rank = (code) => (sectionOrder.includes(code) ? sectionOrder.indexOf(code) : sectionOrder.length)
    for (const p of sel.plans) {
      const secs = Object.entries(text.sections(p.plan_id) ?? {}).sort(([a], [b]) => rank(a) - rank(b) || (a < b ? -1 : 1))
      const hits = text.sectionHits?.(p.plan_id)
      for (const [section, t] of secs) out.push({ ...head(p), section, hits: hits ? (hits[section] ?? 0) : null, text: t })
    }
  } else if (unit === 'hit') {
    for (const p of sel.plans)
      for (const h of text.hits(p.plan_id) ?? [])
        out.push({ ...head(p), term: h.term, section: h.source || null, position: h.start, before: h.before, match: h.match, after: h.after })
  } else throw new Error(`unknown unit ${unit}`)
  return out
}

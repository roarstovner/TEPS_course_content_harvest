// What a plan version is connected to: the programmes and years that use it,
// and the other courses of the institution with the same text.

const cmp = (a, b) => (a < b ? -1 : a > b ? 1 : 0)

/**
 * The years a version is in force and the programmes that list the course
 * in those years (every programme of a course-year uses each of its plans,
 * README "Data Contract").
 * @param {object} course a model course (`years` from `buildModel`)
 * @param {string} key the version's plan key
 * @param {object[]} programmes rows of programmes.parquet
 * @returns {{years: number[], programmes: {programme_code, programme_name,
 *   study_type, years: number[]}[]}} programmes by name
 */
export function planUse(course, key, programmes) {
  const years = []
  const by = new Map()
  for (const cy of [...course.years.values()].sort((a, b) => a.year - b.year)) {
    if (!cy.plans.has(key)) continue
    years.push(cy.year)
    for (const code of cy.programmes) {
      if (!by.has(code)) {
        const p = programmes.find((r) => r.institution === course.institution && r.programme_code === code)
        by.set(code, { programme_code: code, programme_name: p?.programme_name ?? code, study_type: p?.study_type ?? null, years: [] })
      }
      by.get(code).years.push(cy.year)
    }
  }
  return { years, programmes: [...by.values()].sort((a, b) => cmp(a.programme_name, b.programme_name)) }
}

const index = new WeakMap() // model.courses -> Map(institution|plan_content_id -> [{course, version}])

/** Other courses of the institution with a version of the same text
 *  (same `plan_content_id`), by course code. */
export function sameText(model, version) {
  let ix = index.get(model.courses)
  if (!ix) {
    ix = new Map()
    for (const course of model.courses)
      for (const v of course.versions) {
        const k = `${v.institution}|${v.plan_content_id}`
        if (!ix.has(k)) ix.set(k, [])
        ix.get(k).push({ course, version: v })
      }
    index.set(model.courses, ix)
  }
  return (ix.get(`${version.institution}|${version.plan_content_id}`) ?? [])
    .filter((r) => r.course.course_code !== version.course_code)
    .sort((a, b) => cmp(a.course.course_code, b.course.course_code))
}

/** Years as compact ranges: [2019, 2020, 2021, 2023] -> "2019–2021, 2023". */
export function yearRanges(years) {
  const out = []
  for (const y of years) {
    const last = out.at(-1)
    if (last && y === last[1] + 1) last[1] = y
    else out.push([y, y])
  }
  return out.map(([a, b]) => (a === b ? `${a}` : `${a}–${b}`)).join(', ')
}

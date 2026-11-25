// The compare view: two plan versions side by side (any two, or two versions
// of one course), or the plans of the courses in the list in one year, a
// column per institution (README "Layout", Compare). Pure functions.

import { versionId } from './keys.js'

/** A plan version as it is kept in the state: `institution|course_code@versionId`. */
export const planRef = (course, version) => `${course.key}@${versionId(version)}`

/**
 * The course and version of a reference; the latest version when the
 * version is unknown (data rebuilt since the link was made).
 * @returns {{course, version}|null} null when the course is unknown or has no plan
 */
export function resolveRef(model, ref) {
  const i = ref?.lastIndexOf('@') ?? -1
  if (i < 0) return null
  const course = model.byKey.get(ref.slice(0, i))
  if (!course?.versions.length) return null
  const vid = ref.slice(i + 1)
  return { course, version: course.versions.find((v) => versionId(v) === vid) ?? course.versions[0] }
}

/** The version compared with A when B is not chosen: the one before it in
 *  the same course (versions are latest first), else the one after. */
export function defaultOther(a) {
  if (!a) return null
  const vs = a.course.versions
  const i = vs.indexOf(a.version)
  const v = vs[i + 1] ?? vs[i - 1]
  return v ? { course: a.course, version: v } : null
}

/**
 * Courses whose institution, code or name contain every word of `text`
 * (case ignored), with a plan, for the plan picker.
 * @returns {object[]} at most `max` courses, in model order
 */
export function findCourses(model, text, max = 30) {
  const words = text.toLowerCase().split(/\s+/).filter(Boolean)
  if (!words.length) return []
  const out = []
  for (const c of model.courses) {
    if (!c.versions.length) continue
    const hay = `${c.institution} ${c.course_code} ${c.course_name ?? ''}`.toLowerCase()
    if (words.every((w) => hay.includes(w)) && out.push(c) >= max) break
  }
  return out
}

/**
 * The plans in force in `year` of the courses in the list (their matching
 * versions), a column per institution.
 * @param {{course, versions}[]} rows `filterView` rows
 * @returns {{total: number, columns: {institution, plans: {course, version}[]}[]}}
 *   `total` the plans in force; the columns hold the first `max` of them, in
 *   the order of the list
 */
export function acrossPlans(rows, year, max = 40) {
  const columns = new Map()
  let total = 0
  for (const { course, versions } of rows)
    for (const version of versions) {
      if (version.year_from > year || version.year_to < year) continue
      if (total++ >= max) continue
      if (!columns.has(course.institution)) columns.set(course.institution, [])
      columns.get(course.institution).push({ course, version })
    }
  return { total, columns: [...columns].map(([institution, plans]) => ({ institution, plans })) }
}

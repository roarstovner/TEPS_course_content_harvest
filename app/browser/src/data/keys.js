// Identity of courses and plan versions. A plan version is plan_content_id +
// institution + course_code (the id alone is not unique, README "Data
// Contract"), which the column `plan_id` holds in one; keep that knowledge
// here only.

/** Key of a course: any row with `institution` and `course_code`. */
export const courseKey = (r) => `${r.institution}|${r.course_code}`

/** Key of a plan version: a row of plans, sections or course_years (with a
 *  plan). Data built without `plan_id` get the same key from its parts. */
export const planKey = (r) => r.plan_id ?? `${r.institution}_${r.course_code}_${r.plan_content_id.slice(0, 10)}`

/** Short id of a version within its course, for the URL. */
export const versionId = (r) => r.plan_content_id.slice(0, 12)

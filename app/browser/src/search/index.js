// The search engine of the course plan browser (README, "Query Syntax").
export { parseQuery, SECTIONS } from './parse.js'
export { compileQuery } from './match.js'
export { prepareDocs, search, wordForms, kwic, highlights } from './search.js'
export { fold, formAt } from './text.js'

// The search engine of the course plan browser (README, "Query Syntax").
export { parseQuery, SECTIONS, inSection, sectionCode, sectionName } from './parse.js'
export { compileQuery } from './match.js'
export { prepareDocs, search, wordForms, mergeForms, kwic, highlights } from './search.js'
export { fold, FoldedText, formAt } from './text.js'
export { chunked, generations, pause } from './chunks.js'

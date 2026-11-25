// A small Markdown renderer for the user guide (GUIDE.md): headings,
// paragraphs, lists, tables, fenced code, and inline code, bold, italics and
// links. Everything is escaped first, so the output holds no HTML of the
// source. Links: `?...` are views of the app (kept as they are), `#...`
// headings of the guide, `http(s):` external; any other is a file of the
// repository, relative to `app/browser/` (README.md, ../../docs/...), and
// points to it on GitHub.

export const REPO = 'https://github.com/roarstovner/TEPS_course_content_harvest/blob/main/app/browser/'

const esc = (s) => s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;')

/** A heading's anchor, as GitHub makes it: lower case, spaces to hyphens. */
export const slug = (s) =>
  s
    .toLowerCase()
    .replace(/[^\p{L}\p{N}\s-]/gu, '')
    .trim()
    .replace(/\s/g, '-')

/** The href of a link in the guide. */
export function href(url, repo = REPO) {
  if (/^(\?|#|https?:)/.test(url)) return url
  return new URL(url, repo).href
}

function inline(s, repo) {
  // code spans first, kept out of the other rules by placeholders
  const codes = []
  s = s.replace(/`([^`]+)`/g, (_, c) => `\u0000${codes.push(c) - 1}\u0000`)
  s = esc(s)
    .replace(/\[([^\]]+)\]\(((?:[^()\s]|\([^()\s]*\))+)\)/g, (_, text, url) => {
      const h = href(url.replace(/&amp;/g, '&'), repo)
      const ext = /^https?:/.test(h)
      return `<a href="${esc(h)}"${ext ? ' target="_blank" rel="noopener"' : ''}>${text}</a>`
    })
    .replace(/\*\*(.+?)\*\*/g, '<strong>$1</strong>')
    .replace(/(^|[^\w*])\*([^*\s][^*]*?)\*(?![\w*])/g, '$1<em>$2</em>')
  return s.replace(/\u0000(\d+)\u0000/g, (_, i) => `<code>${esc(codes[i].replace(/\\\|/g, '|'))}</code>`)
}

// table cells, split on | but not on \|
const cells = (line) =>
  line
    .trim()
    .replace(/^\||\|$/g, '')
    .split(/(?<!\\)\|/)
    .map((c) => c.trim())

/**
 * Markdown -> HTML.
 * @returns {{html: string, toc: {id: string, text: string}[]}} toc: the
 *   second-level headings
 */
export function renderMarkdown(md, repo = REPO) {
  const lines = md.replace(/\r\n?/g, '\n').split('\n')
  const out = []
  const toc = []
  let i = 0
  while (i < lines.length) {
    const line = lines[i]
    let m
    if (!line.trim()) {
      i++
    } else if (line.startsWith('```')) {
      const body = []
      for (i++; i < lines.length && !lines[i].startsWith('```'); i++) body.push(lines[i])
      i++
      out.push(`<pre><code>${esc(body.join('\n'))}</code></pre>`)
    } else if ((m = /^(#{1,4})\s+(.*)$/.exec(line))) {
      const n = m[1].length
      const id = slug(m[2])
      if (n === 2) toc.push({ id, text: m[2] })
      out.push(`<h${n} id="${id}">${inline(m[2], repo)}</h${n}>`)
      i++
    } else if (line.trimStart().startsWith('|')) {
      const rows = []
      for (; i < lines.length && lines[i].trimStart().startsWith('|'); i++) rows.push(cells(lines[i]))
      const body = rows.filter((r, k) => !(k === 1 && r.every((c) => /^:?-+:?$/.test(c))))
      const [head, ...rest] = body
      const tr = (r, t) => `<tr>${r.map((c) => `<${t}>${inline(c, repo)}</${t}>`).join('')}</tr>`
      out.push(`<table><thead>${tr(head, 'th')}</thead><tbody>${rest.map((r) => tr(r, 'td')).join('')}</tbody></table>`)
    } else if (/^\s*[-*]\s/.test(line)) {
      const items = []
      for (; i < lines.length && lines[i].trim(); i++) {
        if (/^\s*[-*]\s/.test(lines[i])) items.push(lines[i].replace(/^\s*[-*]\s+/, ''))
        else items[items.length - 1] += ' ' + lines[i].trim() // a continued item
      }
      out.push(`<ul>${items.map((t) => `<li>${inline(t, repo)}</li>`).join('')}</ul>`)
    } else {
      const para = []
      for (; i < lines.length && lines[i].trim() && !/^(#|```|\s*\||\s*[-*]\s)/.test(lines[i]); i++) para.push(lines[i].trim())
      out.push(`<p>${inline(para.join(' '), repo)}</p>`)
    }
  }
  return { html: out.join('\n'), toc }
}

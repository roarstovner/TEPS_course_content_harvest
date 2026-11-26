import { readFileSync } from 'node:fs'
import { describe, expect, it } from 'vitest'
import { href, renderMarkdown, REPO, slug } from './markdown.js'
import { parseQuery, sectionCode } from '../search/index.js'

describe('markdown for the user guide', () => {
  it('renders headings, paragraphs, lists, tables and code', () => {
    const { html, toc } = renderMarkdown('# Guide\n\n## What the Numbers Mean\n\nA *b* **c**\nd.\n\n- one\n  more\n- two\n\n| Q | N |\n|---|---|\n| `a\\|b` | 1 |\n\n```\nx < y\n```')
    expect(toc).toEqual([{ id: 'what-the-numbers-mean', text: 'What the Numbers Mean' }])
    expect(html).toContain('<h2 id="what-the-numbers-mean">')
    expect(html).toContain('<p>A <em>b</em> <strong>c</strong> d.</p>')
    expect(html).toContain('<ul><li>one more</li><li>two</li></ul>')
    expect(html).toContain('<thead><tr><th>Q</th><th>N</th></tr></thead><tbody><tr><td><code>a|b</code></td><td>1</td></tr></tbody>')
    expect(html).toContain('<pre><code>x &lt; y</code></pre>')
  })

  it('escapes HTML and keeps code out of the other rules', () => {
    const { html } = renderMarkdown('<b>x</b> `*samisk` and `samisk*` *em* **a *b* c**')
    expect(html).toBe('<p>&lt;b&gt;x&lt;/b&gt; <code>*samisk</code> and <code>samisk*</code> <em>em</em> <strong>a <em>b</em> c</strong></p>')
  })

  it('links: app views as they are, files of the repository on GitHub', () => {
    const { html } = renderMarkdown('[`(a OR b) c`](?q=(a%20OR%20b)%20c&scope=section) [x](../../docs/data-files.md) [y](README.md#term-lists)')
    expect(html).toContain('<a href="?q=(a%20OR%20b)%20c&amp;scope=section"><code>(a OR b) c</code></a>')
    expect(html).toContain(`href="https://github.com/roarstovner/TEPS_course_content_harvest/blob/main/docs/data-files.md" target="_blank"`)
    expect(href('README.md#term-lists')).toBe(REPO + 'README.md#term-lists')
    expect(href('#keyboard')).toBe('#keyboard')
    expect(slug('Searching Within a Section')).toBe('searching-within-a-section')
  })

  it('the guide: every example link parses, every #anchor exists', () => {
    const md = readFileSync(new URL('../../GUIDE.md', import.meta.url), 'utf8')
    const { html } = renderMarkdown(md)
    const ids = new Set([...html.matchAll(/ id="([^"]+)"/g)].map((m) => m[1]))
    for (const [, h] of html.matchAll(/href="([?#][^"]*)"/g)) {
      const url = h.replace(/&amp;/g, '&')
      if (url.startsWith('#')) expect(ids, url).toContain(url.slice(1))
      else {
        const p = new URLSearchParams(url)
        expect(parseQuery(p.get('q') ?? '').error, url).toBeFalsy()
        if (p.has('in')) expect(sectionCode(p.get('in')), url).toBeTruthy()
      }
    }
  })
})

import { describe, expect, it } from 'vitest'
import { chunked, generations, pause } from './chunks.js'
import { compileQuery, mergeForms, prepareDocs, search, wordForms } from './index.js'
import { FoldedText } from './text.js'

describe('chunked scans', () => {
  it('applies fn per chunk, and once to no items', async () => {
    const never = () => false
    expect(await chunked([1, 2, 3, 4, 5], 2, (c) => c.length, never)).toEqual([2, 2, 1])
    expect(await chunked([], 2, (c) => c.length, never)).toEqual([0])
  })
  it('a newer request makes the older stop at its next chunk', async () => {
    const next = generations()
    const seen = []
    const run = (name, n) => {
      const stale = next()
      return chunked(Array.from({ length: n }, (_, i) => i), 1, (c) => seen.push(`${name}${c[0]}`), stale)
    }
    const old = run('a', 5)
    await pause() // the first chunk of a has run
    const newer = run('b', 2)
    expect(await old).toBeNull()
    expect((await newer).length).toBe(2)
    expect(seen.filter((s) => s.startsWith('a')).length).toBeLessThan(5)
    expect(seen).toContain('b1')
  })
  it('a request superseded before its first chunk does nothing', async () => {
    const next = generations()
    const stale = next()
    next()
    let calls = 0
    expect(await chunked([1, 2], 1, () => calls++, stale)).toBeNull()
    expect(calls).toBe(0)
  })
  it('search and word forms in chunks equal one pass', async () => {
    const docs = prepareDocs([
      { id: 1, text: 'Utforsking og IKT' },
      { id: 2, text: 'utforskning' },
      { id: 3, text: 'ingen treff' },
      { id: 4, text: 'UTFORSKING, utforsking og utforskende' },
    ])
    const m = compileQuery('utforsk*')
    const never = () => false
    expect((await chunked(docs, 3, (d) => search(d, m), never)).flat()).toEqual(search(docs, m))
    expect(mergeForms(await chunked(docs, 1, (d) => wordForms(d, m), never))).toEqual(wordForms(docs, m))
  })
})

describe('FoldedText', () => {
  const s = 'İzmir: Utforsking OG IKT-basert ΟΔΟΣ 𝐀b'
  const t = new FoldedText(s)
  it('rebuilds the original and any piece of it', () => {
    expect(t.lc.length).toBe(s.length)
    expect(t.text).toBe(s)
    for (const [a, b] of [[0, 1], [3, 12], [18, 24], [21, 21], [30, 99], [36, s.length]]) expect(t.slice(a, b)).toBe(s.slice(a, b))
    expect(t.differs(7)).toBe(true) // U
    expect(t.differs(8)).toBe(false)
    expect(new FoldedText('bare små').up).toBeNull()
  })
  it('lets regular expressions run on the folded copy only when that is exact', () => {
    // İ folds to i, but a regex with i and u does not fold it: the original is searched
    expect(t.regexOnLc).toBe(false)
    expect(search(prepareDocs([{ id: 1, text: t }]), compileQuery('/İzmir/')).length).toBe(1)
    expect(search(prepareDocs([{ id: 1, text: t }]), compileQuery('/izmir/')).length).toBe(0)
    expect(new FoldedText('Utforsking ØKONOMI').regexOnLc).toBe(true)
    // the Kelvin sign lower-cases to k, and a regex with i and u folds it so
    expect(new FoldedText('K').regexOnLc).toBe(true)
    // U+1E9E (capital sharp s) lower-cases to ß, but simple case folding maps it to ß too
    expect(new FoldedText('ẞ').regexOnLc).toBe(true)
  })
  it('accepts a FoldedText in prepareDocs', () => {
    const [d] = prepareDocs([{ id: 1, text: t, sections: { x: new FoldedText('Ab') } }])
    expect(d).toBe(t)
    expect(d.sections.x.text).toBe('Ab')
  })
})

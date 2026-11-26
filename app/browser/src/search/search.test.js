import { describe, expect, it } from 'vitest'
import { compileQuery, fold, formAt, highlights, inSection, kwic, parseQuery, prepareDocs, search, wordForms } from './index.js'

const PLANS = [
  { id: 'a', text: 'Emnet gir innføring i fremmedspråksdidaktikk og morsmålsdidaktikk.' },
  { id: 'b', text: 'Fremmedspråkdidaktikk. Studentene lærer om morsmålsundervisning.' },
  { id: 'c', text: 'Kurset gis i samarbeid med Fremmedspråksenteret. Morsmål er ikke tema.' },
  { id: 'd', text: 'Emnet tar for seg fremmedspråks- og morsmålsdidaktikk i skolen.' },
  { id: 'e', text: 'Utforsking i naturfag.\nUtforskning og IKT-basert læring.' },
  { id: 'f', text: 'Konflikthåndtering og ØKONOMI.' },
  { id: 'g', text: 'Temaet er folkehelse og\nlivsmestring i skolen. Bruk av ikt i undervisningen.' },
]
const docs = prepareDocs(PLANS)
const ids = (q, opts) => search(docs, compileQuery(q), opts).map((r) => r.id)

describe('fold and formAt', () => {
  it('keeps the length, so offsets point into the original', () => {
    const s = 'İstanbul ØKONOMI ΟΔΟΣ'
    expect(s.toLowerCase().length).not.toBe(s.length)
    expect(fold(s)).toBe('istanbul økonomi οδοσ')
    expect(fold(s).length).toBe(s.length)
  })
  it('finds the word a match falls in', () => {
    const lc = fold('om fremmedspråks- og IKT-basert -x læring-')
    expect(formAt(lc, 5, 10)).toBe('fremmedspråks-')
    expect(formAt(lc, 22, 25)).toBe('ikt-basert')
    expect(formAt(lc, 33, 34)).toBe('x')
    expect(formAt(lc, 36, 40)).toBe('læring-')
  })
})

describe('search', () => {
  it('matches plain terms anywhere, also inside words, ignoring case', () => {
    expect(ids('didaktikk')).toEqual(['a', 'b', 'd'])
    expect(ids('ikt')).toEqual(['e', 'f', 'g']) // konflikt too
    expect(ids('økonomi')).toEqual(['f'])
    expect(ids('ØKONOMI')).toEqual(['f'])
  })
  it('finds the fremmedspråk(s)didaktikk variants with wildcards and AND', () => {
    expect(ids('fremmedspråksdidaktikk morsmålsdidaktikk')).toEqual(['a'])
    expect(ids('fremmedspråk* morsmål*')).toEqual(['a', 'b', 'c', 'd'])
    expect(ids('*didaktikk')).toEqual(['a', 'b', 'd'])
  })
  it('drops matches inside excluded forms, per term', () => {
    const excludedForms = { 'fremmedspråk*': ['Fremmedspråksenteret'] }
    expect(ids('fremmedspråk* morsmål*', { excludedForms })).toEqual(['a', 'b', 'd'])
    // the same form under another term key is not excluded
    expect(ids('fremmedspråk', { excludedForms })).toEqual(['a', 'b', 'c', 'd'])
    expect(ids('fremmedspråk', { excludedForms: new Map([['fremmedspråk', new Set(['fremmedspråksenteret'])]]) })).toEqual([
      'a',
      'b',
      'd',
    ])
  })
  it('lists word forms with plan and occurrence counts, the hyphen ellipsis kept', () => {
    const [t] = wordForms(docs, compileQuery('fremmedspråk*'))
    expect(t.key).toBe('fremmedspråk*')
    expect(t.forms.map((f) => f.form).sort()).toEqual([
      'fremmedspråkdidaktikk',
      'fremmedspråks-',
      'fremmedspråksdidaktikk',
      'fremmedspråksenteret',
    ])
    expect(t.forms.every((f) => f.plans === 1 && f.count === 1)).toBe(true)
    const [u] = wordForms(docs, compileQuery('utforsk*'))
    expect(u.forms).toEqual([
      { form: 'utforsking', plans: 1, count: 1 },
      { form: 'utforskning', plans: 1, count: 1 },
    ])
    const many = prepareDocs([{ id: 1, text: 'ikt IKT ikt-basert' }, { id: 2, text: 'IKT' }])
    expect(wordForms(many, compileQuery('ikt'))[0].forms).toEqual([
      { form: 'ikt', plans: 2, count: 3 },
      { form: 'ikt-basert', plans: 1, count: 1 },
    ])
  })
  it('matches word starts, ends and both', () => {
    expect(ids('utforsk*')).toEqual(['e'])
    const d = prepareDocs([{ id: 1, text: 'utøving utforsking uting kutting utfor' }])
    const m = compileQuery('ut*ing')
    expect(highlights(d[0].text, m).map((h) => d[0].text.slice(h.start, h.end))).toEqual(['utøving', 'utforsking', 'uting'])
    expect(highlights('matematikkdidaktikk didaktikken', compileQuery('*didaktikk')).length).toBe(1)
  })
  it('matches quoted terms as whole words only', () => {
    expect(ids('"ikt"')).toEqual(['e', 'g']) // not konflikt; IKT-basert has the word IKT before the hyphen
    expect(ids('"økonomi"')).toEqual(['f'])
    expect(ids('"didaktikk"')).toEqual([])
  })
  it('matches a phrase across a line break, whole words at both ends', () => {
    expect(ids('"folkehelse og livsmestring"')).toEqual(['g'])
    expect(ids('"folkehelse og livs"')).toEqual([])
    expect(ids('"helse og livsmestring"')).toEqual([])
    const [h] = highlights(PLANS[6].text, compileQuery('"folkehelse og livsmestring"'))
    expect(PLANS[6].text.slice(h.start, h.end)).toBe('folkehelse og\nlivsmestring')
  })
  it('combines OR and AND with the right precedence and grouping', () => {
    expect(ids('morsmål fremmedspråksenteret OR fremmedspråkdidaktikk')).toEqual(['b', 'c'])
    expect(ids('(utforsking OR økonomi) ikt')).toEqual(['e', 'f'])
    expect(ids('utforsking OR økonomi ikt')).toEqual(['e', 'f'])
    expect(ids('utforsking OR (økonomi skolen)')).toEqual(['e'])
  })
  it('excludes with - and NOT', () => {
    expect(ids('morsmål -fremmedspråksenteret')).toEqual(['a', 'b', 'd'])
    expect(ids('morsmål NOT didaktikk')).toEqual(['c'])
    expect(ids('-didaktikk')).toEqual(['c', 'e', 'f', 'g'])
    expect(ids('-(didaktikk OR ikt)')).toEqual(['c'])
  })
  it('matches regular expressions, case-insensitive and Unicode', () => {
    expect(ids('/utforsk(ing|ning)/')).toEqual(['e'])
    expect(ids('/(?<!\\p{L})økonomi(?!\\p{L})|^temaet/')).toEqual(['f', 'g'])
    expect(ids('/^utforskning/')).toEqual(['e']) // ^ and $ match at line breaks too
    expect(ids('/fremmedspråks?didaktikk/')).toEqual(['a', 'b'])
    expect(ids('/x*/')).toEqual([]) // empty matches do not count
  })
  it('returns per-term hit counts', () => {
    const r = search(prepareDocs([{ id: 1, text: 'IKT, ikt og konflikt' }]), compileQuery('ikt "ikt"'))
    expect(r).toEqual([{ id: 1, counts: [3, 2] }])
  })
})

describe('sections', () => {
  const sdocs = prepareDocs([
    {
      id: 1,
      text: 'Læringsutbytte: dybdelæring. Innhold: praksis og lesing.',
      sections: { learning_outcomes: 'dybdelæring i faget', course_content: 'praksis og lesing' },
    },
    {
      id: 2,
      text: 'Innhold: dybdelæring og lesing. Praksis i læringsutbyttet.',
      sections: { course_content: 'dybdelæring og lesing', learning_outcomes: 'praksis' },
    },
    { id: 3, text: 'dybdelæring', sections: {} },
  ])
  const sids = (q, opts) => search(sdocs, compileQuery(q), opts).map((r) => r.id)
  it('restricts prefixed terms to their section', () => {
    expect(sids('dybdelæring')).toEqual([1, 2, 3])
    expect(sids('outcomes:dybdelæring')).toEqual([1])
    expect(sids('læringsutbytte:dybdelæring')).toEqual([1])
    expect(sids('innhold:(dybdelæring OR praksis)')).toEqual([1, 2])
    expect(sids('dybdelæring', { section: 'course_content' })).toEqual([2])
  })
  it('applies a prefixed NOT to that section', () => {
    expect(sids('dybdelæring -outcomes:praksis')).toEqual([1, 3])
  })
  it('requires all terms in one section with scope section', () => {
    expect(sids('dybdelæring lesing')).toEqual([1, 2])
    expect(sids('dybdelæring lesing', { scope: 'section' })).toEqual([2])
    expect(sids('praksis lesing', { scope: 'section' })).toEqual([1])
  })
  it('marks only the terms of the shown section', () => {
    const m = compileQuery('outcomes:dybdelæring lesing')
    expect(highlights('dybdelæring og lesing', m, { section: 'course_content' })).toEqual([{ start: 15, end: 21, terms: [1] }])
    expect(highlights('dybdelæring og lesing', m).length).toBe(2)
    expect(kwic(sdocs[0], m).map((k) => [k.source, k.match])).toEqual([
      ['', 'lesing'],
      ['learning_outcomes', 'dybdelæring'],
    ])
  })
})

describe('searching in one section (the header control)', () => {
  const sdocs = prepareDocs([
    { id: 1, text: 'x', sections: { learning_outcomes: 'samisk kultur', course_content: 'urfolk' } },
    { id: 2, text: 'x', sections: { learning_outcomes: 'samisk og urfolk', course_content: 'lesing' } },
    { id: 3, text: 'samisk urfolk', sections: { course_content: 'samisk urfolk' } },
    { id: 4, text: 'x', sections: { learning_outcomes: 'lesing praksis' } },
  ])
  const sids = (q, opts) => search(sdocs, compileQuery(q), opts).map((r) => r.id)
  const LO = 'learning_outcomes'
  it('puts the section prefix on the whole query', () => {
    expect(inSection('samisk content:urfolk', LO)).toBe('outcomes:(samisk content:urfolk)')
    expect(inSection(' samisk ', 'course_content')).toBe('content:(samisk)')
    expect(inSection('samisk', null)).toBe('samisk')
    expect(inSection('', LO)).toBe('')
    expect(inSection('samisk (', LO)).toBe('samisk (') // an error stays where it is
    expect(inSection('"samisk', LO)).toBe('"samisk')
  })
  it('lets a term\'s own prefix win for that term', () => {
    const t = parseQuery(inSection('samisk content:urfolk -praksis', LO)).terms
    expect(t.map((x) => [x.key, x.section])).toEqual([
      ['outcomes:samisk', LO],
      ['content:urfolk', 'course_content'],
      ['outcomes:praksis', LO],
    ])
    expect(sids(inSection('samisk content:urfolk', LO))).toEqual([1])
    expect(sids(inSection('samisk content:urfolk', LO))).toEqual(sids('outcomes:samisk content:urfolk'))
  })
  it('matches as the prefix on every term, and as search\'s section option', () => {
    for (const q of ['samisk', 'samisk urfolk', 'samisk OR lesing', '-samisk', '/urfolk|lesing/', '(samisk OR urfolk) -praksis'])
      for (const scope of ['plan', 'section', 'paragraph']) {
        const got = sids(inSection(q, LO), { scope })
        expect(got).toEqual(sids(q, { scope, section: LO }))
      }
    expect(sids(inSection('samisk urfolk', LO))).toEqual(sids('outcomes:samisk outcomes:urfolk'))
    expect(sids(inSection('-samisk', LO))).toEqual([3, 4]) // plans without the section included, as -outcomes:samisk
  })
})

describe('AND scopes', () => {
  const text =
    'Studentene skal lese tekster. De skal skrive om dem, bl.a. essay.\n' +
    'Vurdering skjer ved mappe.\n\nPraksis: lese og observere i klasserommet.'
  const sdocs = prepareDocs([{ id: 1, text }])
  const ok = (q, opts) => search(sdocs, compileQuery(q), opts).length === 1
  it('plan (default)', () => {
    expect(ok('lese mappe')).toBe(true)
  })
  it('paragraph = a line', () => {
    expect(ok('lese skrive', { scope: 'paragraph' })).toBe(true)
    expect(ok('skrive mappe', { scope: 'paragraph' })).toBe(false)
    expect(ok('lese observere', { scope: 'paragraph' })).toBe(true) // the second line
  })
  it('sentence: ends at .!? before a capital or digit, or at a line break', () => {
    expect(ok('lese skrive', { scope: 'sentence' })).toBe(false)
    expect(ok('skrive essay', { scope: 'sentence' })).toBe(true) // "bl.a. essay" does not end it
    expect(ok('lese tekster', { scope: 'sentence' })).toBe(true)
    expect(ok('essay vurdering', { scope: 'sentence' })).toBe(false)
  })
  it('within N words', () => {
    // Studentene(0) skal(1) lese(2) tekster(3) De(4) skal(5) skrive(6)
    expect(ok('lese skrive', { scope: 'words', words: 4 })).toBe(true)
    expect(ok('lese skrive', { scope: 'words', words: 3 })).toBe(false)
    expect(ok('skrive lese', { scope: 'words', words: 4 })).toBe(true) // order does not matter
    expect(ok('studentene observere', { scope: 'words', words: 10 })).toBe(false)
  })
  it('evaluates OR inside a unit and NOT on the whole plan', () => {
    expect(ok('lese (skrive OR mappe)', { scope: 'sentence' })).toBe(false)
    expect(ok('observere (skrive OR lese)', { scope: 'sentence' })).toBe(true)
    expect(ok('observere lese -skrive', { scope: 'sentence' })).toBe(false)
    expect(ok('observere lese -eksamen', { scope: 'sentence' })).toBe(true)
    expect(ok('-eksamen', { scope: 'sentence' })).toBe(true)
  })
  it('respects excluded forms in units', () => {
    expect(ok('lese observere', { scope: 'paragraph', excludedForms: { lese: ['lese'] } })).toBe(false)
  })
})

describe('kwic and highlights', () => {
  const doc = prepareDocs([{ id: 1, text: 'İzmir: utforsking og\nIKT. Mer UTFORSKING.' }])[0]
  it('returns offsets into the original text, also after İ', () => {
    const k = kwic(doc, compileQuery('utforsk* ikt'), { context: 8 })
    expect(k.map((x) => doc.text.slice(x.start, x.end))).toEqual(['utforsking', 'IKT', 'UTFORSKING'])
    expect(k[1]).toMatchObject({ term: 1, before: 'king og ', match: 'IKT', after: '. Mer UT' })
    expect(kwic(doc, compileQuery('utforsk*'), { max: 1 }).length).toBe(1)
  })
  it('merges overlapping ranges', () => {
    const h = highlights('utforskende', compileQuery('utforsk* forsk'))
    expect(h).toEqual([{ start: 0, end: 11, terms: [0, 1] }])
    expect(highlights('utforskende', compileQuery('-utforsk'))).toEqual([])
  })
  it('skips excluded forms', () => {
    const m = compileQuery('fremmedspråk*')
    expect(highlights(PLANS[2].text, m, { excludedForms: { 'fremmedspråk*': ['fremmedspråksenteret'] } })).toEqual([])
  })
})

<script>
  // Middle pane, term lists: named lists of queries kept in this browser,
  // imported from and exported to YAML (the format of LK20_terms.yaml), run
  // in the worker as a heatmap term x institution x year, counted as the
  // trend view counts; a cell lists its courses; the matrix exports as CSV.
  import { inSection, parseQuery } from '../search/index.js'
  import { loadLists, matrixCsv, parseTermLists, saveLists, serializeTermList, slug, termMatrix } from '../data/termlists.js'
  import { share } from '../data/trend.js'
  import { num, pct } from './format.js'

  let { model, s = $bindable(), meta, worker, plansReady, onpick } = $props()

  let lists = $state(loadLists())
  let stored = $state(true) // false when the browser does not keep them
  let error = $state(null)
  let fileInput = $state()
  const save = () => (stored = saveLists($state.snapshot(lists)))

  const names = $derived(Object.keys(lists).sort((a, b) => a.localeCompare(b)))
  const name = $derived(s.tl != null && lists[s.tl] ? s.tl : (names[0] ?? null))
  const list = $derived(name ? lists[name] : null)
  const terms = $derived(list?.terms ?? [])

  const unique = (base) => {
    let n = base
    for (let i = 2; lists[n]; i++) n = `${base} (${i})`
    return n
  }
  function newList() {
    const n = unique('New list')
    lists[n] = { terms: [], meta: {} }
    s.tl = n
    save()
  }
  function rename(to) {
    to = to.trim()
    if (!to || to === name || !list) return
    to = unique(to)
    lists[to] = list
    delete lists[name]
    s.tl = to
    save()
  }
  function remove() {
    if (!list || !confirm(`Delete the term list "${name}" from this browser?`)) return
    delete lists[name]
    s.tl = null
    save()
  }
  function addTerm(q = '') {
    if (!list) newList()
    lists[name].terms.push({ id: q ? slug(q) : `term${terms.length + 1}`, label: q, query: q })
    save()
  }
  function edit(i, field, value) {
    const t = lists[name].terms[i]
    t[field] = value.trim()
    if (field === 'label' && /^term\d+$/.test(t.id) && t.label) t.id = slug(t.label)
    save()
  }
  function drop(i) {
    lists[name].terms.splice(i, 1)
    save()
  }
  async function importFile(file) {
    error = null
    try {
      const got = await parseTermLists(await file.text(), file.name.replace(/\.ya?ml$/i, ''))
      let first = null
      for (const l of got) {
        const n = unique(l.name)
        lists[n] = { terms: l.terms, meta: l.meta }
        first ??= n
      }
      s.tl = first
      save()
    } catch (e) {
      error = `${file.name}: ${e?.message ?? e}`
    }
  }
  function saveFile(text, filename, type) {
    const a = document.createElement('a')
    a.href = URL.createObjectURL(new Blob([text], { type }))
    a.download = filename.replace(/[^\p{L}\p{N}._-]+/gu, '-')
    a.click()
    setTimeout(() => URL.revokeObjectURL(a.href), 10000)
  }
  async function exportYaml() {
    saveFile(await serializeTermList({ name, ...$state.snapshot(list) }), `${name}.yaml`, 'application/yaml')
  }
  function exportCsv() {
    const stamp = new Date().toISOString().slice(0, 19).replace(/[-:]/g, '')
    saveFile(matrixCsv(name, terms.map((t, k) => ({ ...t, query: queries[k] })), matrix, s.scope), `terms_${meta.version}_${name}_${stamp}Z.csv`, 'text/csv;charset=utf-8')
  }

  // the run: the plans each term matches, in the worker (filters apply here),
  // in the header's section when one is chosen (a term's own prefix wins)
  const queries = $derived(terms.map((t) => inSection(t.query, s.in)))
  const sig = $derived(JSON.stringify([queries, s.scope, s.scope === 'words' ? s.n : null]))
  let run = $state.raw(null) // {sig, results: [{keys: Set} | {error}], ms}
  let running = $state(false)
  $effect(() => {
    const [k, qs, scope, n] = [sig, queries, s.scope, s.n]
    if (!plansReady || !qs.length) return
    running = true
    const t0 = performance.now()
    worker.call('terms', { queries: $state.snapshot(qs), scope, n }).then(
      (r) => {
        if (r.superseded || k !== sig) return
        run = { sig: k, results: r.results.map((x) => (x.keys ? { keys: new Set(x.keys) } : x)), ms: performance.now() - t0 }
        running = false
      },
      (e) => ((error = String(e?.message ?? e)), (running = false)),
    )
  })
  const ready = $derived(run?.sig === sig)
  const matrix = $derived(ready ? termMatrix(model, s, run.results.map((r) => r.keys ?? null)) : null)

  // the heatmap's rows and the scale of its cells
  const rows = $derived(
    matrix
      ? terms.flatMap((t, k) => {
          const m = matrix.terms[k]
          if (!m) return [{ t, k, inst: null, error: run.results[k].error }]
          const all = { t, k, inst: null, points: m.all }
          return s.trows === 'inst' ? [all, ...matrix.institutions.map((i) => ({ t, k, inst: i, points: m.inst.get(i) }))] : [all]
        })
      : [],
  )
  const value = (p) => (s.tval === 'count' ? (p.withPlan ? p.matching : null) : share(p.matching, p.withPlan))
  const top = $derived(Math.max(0, ...rows.flatMap((r) => (r.points ?? []).map(value).filter((v) => v != null))))
  const shade = (v) => (top ? Math.round((100 * v) / top) : 0)
  // a share as a percentage: one decimal under 10%, so that a few plans do not read as 0
  const cellText = (v) => (s.tval === 'count' ? num(v) : v && v < 0.1 ? (100 * v).toFixed(1) : `${Math.round(100 * v)}`)
  const instName = $derived(new Map(meta.institutions.map((i) => [i.code, i.institution_name])))
</script>

<div class="trend terms">
  <div class="trendbar">
    <label
      >List <select value={name} onchange={(e) => (s.tl = e.currentTarget.value)} disabled={!names.length}>
        {#each names as n (n)}<option value={n}>{n}</option>{/each}
      </select></label
    >
    {#if list}<input class="lname" value={name} aria-label="Name of the list" onchange={(e) => rename(e.currentTarget.value)} />{/if}
    <button class="link" onclick={newList}>new list</button>
    <button class="link" onclick={() => fileInput.click()}>import YAML</button>
    <input type="file" accept=".yaml,.yml,text/yaml" hidden bind:this={fileInput} onchange={(e) => (e.currentTarget.files[0] && importFile(e.currentTarget.files[0]), (e.currentTarget.value = ''))} />
    {#if list}
      <button class="link" onclick={exportYaml}>export YAML</button>
      <button class="link" onclick={remove}>delete list</button>
    {/if}
  </div>
  <p class="note sub">
    A term is a query (Norwegian and English variants joined by OR, as <code>dybdelæring OR "deep learning"</code>), run
    with the scope of AND and the section set above. Lists are kept in this browser{#if !stored}<strong class="fail">
        — this browser does not store them: export before leaving</strong
      >{/if}; YAML in the format of <code>LK20_terms.yaml</code> (README "Term Lists").
  </p>
  {#if error}<p class="fail">{error}</p>{/if}

  {#if !list}
    <p class="hint">No term list yet: make a new one or import a YAML file.</p>
  {:else}
    {#if terms.length}
      <h3>Share of course-years with a plan whose plan matches each term</h3>
      <div class="trendbar">
        <span class="note">Rows</span>
        <label><input type="radio" bind:group={s.trows} value="all" /> term</label>
        <label><input type="radio" bind:group={s.trows} value="inst" /> term per institution</label>
        <span class="note">Cells</span>
        <label><input type="radio" bind:group={s.tval} value="share" /> share (%)</label>
        <label><input type="radio" bind:group={s.tval} value="count" /> course-years</label>
        <span class="note">{#if running || !ready}running {terms.length} terms…{:else}{num(Math.round(run.ms))} ms{/if}</span>
        <button class="dlbutton" onclick={exportCsv} disabled={!matrix}>Export CSV</button>
      </div>
      <p class="note sub">
        Course-years in force in the filter, as in the trend view: a course-year matches when any of its plans matches the
        term; the share is of the course-years with a plan (empty: none). Darkest: {#if s.tval === 'count'}{num(top)}{:else}{pct(top, 1)}{/if}.
        Click a cell to list its courses.
      </p>
      {#if matrix}
        <div class="heatwrap">
          <table class="heat">
            <thead>
              <tr>
                <th>Term</th>
                {#if s.trows === 'inst'}<th>Institution</th>{/if}
                {#each matrix.years as y (y)}<th class="yr">{String(y).slice(2)}</th>{/each}
              </tr>
            </thead>
            <tbody>
              {#each rows as r (`${r.k}|${r.inst}`)}
                <tr class:first={!r.inst}>
                  <th title={r.t.query}>{r.inst ? '' : r.t.label || r.t.id}</th>
                  {#if s.trows === 'inst'}<td class="inst" title={instName.get(r.inst)}>{r.inst ? r.inst.toUpperCase() : 'all'}</td>{/if}
                  {#if r.error}
                    <td class="fail" colspan={matrix.years.length}>{r.error}</td>
                  {:else}
                    {#each r.points as p (p.year)}
                      {@const v = value(p)}
                      <td>
                        {#if v != null}
                          <button
                            class="cell"
                            class:dark={shade(v) > 55}
                            style:background="color-mix(in srgb, var(--cell-match) {shade(v)}%, transparent)"
                            title="{r.t.label || r.t.id} · {r.inst ? r.inst.toUpperCase() : 'all'} · {p.year}: {num(p.matching)} matching of {num(p.withPlan)} with a plan ({pct(p.matching, p.withPlan)}); {num(p.n)} course-years"
                            onclick={() => onpick({ q: r.t.query, inst: r.inst, y: p.year })}>{cellText(v)}</button
                          >
                        {/if}
                      </td>
                    {/each}
                  {/if}
                </tr>
              {/each}
            </tbody>
          </table>
        </div>
      {/if}
    {/if}
    <details class="ttable" open>
      <summary>Terms ({terms.length})</summary>
      <table class="progs termtable">
        <thead><tr><th>Label</th><th>Query</th><th></th></tr></thead>
        <tbody>
          {#each terms as t, i (i)}
            {@const err = t.query ? parseQuery(t.query).error : null}
            <tr>
              <td><input value={t.label} placeholder={t.id} aria-label="Label" onchange={(e) => edit(i, 'label', e.currentTarget.value)} /></td>
              <td>
                <input class="q" value={t.query} aria-label="Query" aria-invalid={!!err} title={err?.message ?? ''} onchange={(e) => edit(i, 'query', e.currentTarget.value)} />
              </td>
              <td><button class="link" onclick={() => drop(i)} aria-label="Remove {t.label || t.id}">remove</button></td>
            </tr>
          {/each}
        </tbody>
      </table>
      <div class="trendbar">
        <button class="link" onclick={() => addTerm()}>add a term</button>
        {#if s.q.trim()}<button class="link" onclick={() => addTerm(s.q.trim())}>add the current query</button>{/if}
      </div>
    </details>
  {/if}
</div>

<style>
  .lname { width: 14em; }
  .termtable { margin: 6px 0 4px; width: 100%; max-width: 70rem; }
  .termtable td:first-child { width: 14em; }
  .termtable input { width: 100%; font-size: 13px; }
  .termtable input.q { font-family: var(--mono); }
  .termtable input[aria-invalid='true'] { border-color: var(--error); }
  .heatwrap { overflow-x: auto; margin-top: 6px; }
  .heat { border-collapse: collapse; font-size: 12px; }
  .heat th { text-align: left; font-weight: 600; padding: 1px 8px 1px 0; white-space: nowrap; max-width: 16em; overflow: hidden; text-overflow: ellipsis; }
  .heat th.yr { text-align: center; color: var(--fg-2); font-weight: normal; padding: 0; }
  .heat td { padding: 1px; }
  .heat td.inst { color: var(--fg-2); padding-right: 8px; }
  .heat tr.first > * { border-top: 1px solid var(--line); }
  .cell { width: 2.6em; height: 1.7em; border: 0; border-radius: 2px; font: 11px var(--font); font-variant-numeric: tabular-nums; cursor: pointer; padding: 0; outline: 1px solid var(--line); outline-offset: -1px; }
  .cell.dark { color: var(--bg); }
  .cell:hover { outline: 2px solid var(--accent); }
</style>

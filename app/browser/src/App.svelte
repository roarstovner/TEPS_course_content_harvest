<script>
  // The app shell: loads the data, keeps the state (mirrored in the URL),
  // sends searches to the worker and lays out the panes (README "Layout").
  import { onMount, untrack } from 'svelte'
  import { parseQuery } from './search/index.js'
  import { fetchCached, fetchMeta, readParquet } from './data/load.js'
  import { startWorker } from './data/client.js'
  import { addPlans, buildModel, filterView, pickVersion } from './data/model.js'
  import { decodeState, encodeState } from './data/url.js'
  import { versionId } from './data/keys.js'
  import Header from './lib/Header.svelte'
  import Filters from './lib/Filters.svelte'
  import Results from './lib/Results.svelte'
  import CourseView from './lib/CourseView.svelte'
  import Download from './lib/Download.svelte'

  const base = new URL('data/', document.baseURI).href

  let s = $state(decodeState(location.search))
  let meta = $state.raw(null)
  let baseModel = $state.raw(null)
  let plansMeta = $state.raw(null)
  let sectionsLoaded = $state(false)
  let progress = $state({}) // file -> {loaded, total}
  let error = $state(null)
  let result = $state.raw(null) // {q, excl, matches: Map, ms, forms}
  let searching = $state(false)
  let active = $state(0) // keyboard cursor in the results
  let worker = $state.raw(null)
  let searchBox = $state()
  let downloadOpen = $state(false)

  onMount(async () => {
    try {
      meta = await fetchMeta(base)
      worker = startWorker(onEvent)
      worker.load(base, meta.version)
      const get = (f) =>
        fetchCached(new URL(`${f}.parquet`, base).href, meta.version, (loaded, total) => (progress[f] = { loaded, total })).then(
          (buf) => readParquet(buf),
        )
      const [courses, courseYears, programmes] = await Promise.all(['courses', 'course_years', 'programmes'].map(get))
      baseModel = { ...buildModel({ courses, courseYears }), programmes }
    } catch (e) {
      error = String(e?.message ?? e)
    }
  })

  function onEvent(e) {
    if (e.type === 'progress') progress[e.file] = { loaded: e.loaded, total: e.total }
    else if (e.type === 'plans') plansMeta = e.plans
    else if (e.type === 'sections') sectionsLoaded = true
    else if (e.type === 'error') error = e.message
  }

  // versions are attached once the worker has sent the plans' metadata
  const model = $derived(baseModel && plansMeta ? addPlans(baseModel, plansMeta) : baseModel)

  const parsed = $derived(parseQuery(s.q))
  const queryOn = $derived(!!parsed.ast && !parsed.error)
  // excluded forms of the terms in the query (others stay in `s.excl`, so
  // that retyping a term brings them back, but are not applied or written)
  const excl = $derived(
    Object.fromEntries(parsed.terms.filter((t) => s.excl[t.key]?.length).map((t) => [t.key, [...s.excl[t.key]]])),
  )

  let timer
  let searchId = 0
  $effect(() => {
    const { q, scope, n } = s
    const ex = excl
    const on = queryOn && !!plansMeta
    clearTimeout(timer)
    if (!queryOn) {
      searchId++
      result = null
      searching = false
      return
    }
    if (!on) return
    timer = setTimeout(async () => {
      const id = ++searchId
      searching = true
      try {
        const r = await worker.call('search', { q, scope, n, excl: ex })
        if (id !== searchId) return
        searching = false
        // the word forms do not depend on the excluded forms or the scope
        const forms = q === result?.q ? result.forms : []
        result = r.error ? null : { q, excl: ex, matches: new Map(r.matches), ms: r.ms, forms }
        if (r.error || forms.length) return
        const f = await worker.call('forms', { q })
        if (id === searchId) result = { ...result, forms: f }
      } catch (e) {
        if (id === searchId) {
          searching = false
          error = String(e?.message ?? e)
        }
      }
    }, 150)
  })

  const matches = $derived(queryOn ? (result?.matches ?? null) : null)
  const view = $derived(model ? filterView(model, s, matches) : null)
  const y0 = $derived(s.y0 ?? model?.yearMin)
  const y1 = $derived(s.y1 ?? model?.yearMax)
  const selected = $derived(model && s.course ? (model.byKey.get(s.course) ?? null) : null)
  const selectedRow = $derived(selected && view ? view.rows.find((r) => r.course === selected) : null)
  const version = $derived(selected ? pickVersion(selected, selectedRow, s.version, versionId) : null)

  // a new result list starts the cursor at the top
  $effect(() => {
    view?.rows
    active = 0
  })

  // state -> URL: replace while typing, push when a course is opened or closed
  let urlCourse = untrack(() => s.course)
  $effect(() => {
    const qs = encodeState({ ...s, excl })
    if (`?${qs}` === location.search || (!qs && !location.search)) return
    const url = qs ? `?${qs}` : location.pathname
    if (s.course !== urlCourse) history.pushState(null, '', url)
    else history.replaceState(null, '', url)
    urlCourse = s.course
  })
  function onPopState() {
    const d = decodeState(location.search)
    urlCourse = d.course
    Object.assign(s, d)
  }

  function open(course) {
    s.version = null
    s.course = course.key
  }
  function close() {
    s.course = null
    s.version = null
  }

  function onKey(e) {
    const t = e.target
    const typing = t instanceof HTMLElement && (t.isContentEditable || /^(INPUT|TEXTAREA|SELECT)$/.test(t.tagName))
    if (e.key === 'Escape') {
      if (typing) t.blur()
      else if (s.course) close()
      return
    }
    if (typing || e.ctrlKey || e.metaKey || e.altKey || !view) return
    const n = view.rows.length
    if (e.key === '/') {
      e.preventDefault()
      searchBox?.focus()
    } else if (e.key === 'j' && n) active = Math.min(n - 1, active + 1)
    else if (e.key === 'k' && n) active = Math.max(0, active - 1)
    else if (e.key === 'Enter' && n && !(t instanceof Element && t.closest('button, a, summary'))) open(view.rows[active].course)
    else return
    e.preventDefault()
  }
</script>

<svelte:window onkeydown={onKey} onpopstate={onPopState} />

<div class="app">
  <Header
    bind:state={s}
    bind:input={searchBox}
    {parsed}
    {result}
    {searching}
    counts={view?.counts}
    {queryOn}
    waiting={queryOn && !plansMeta}
    {progress}
    plansReady={!!plansMeta}
    {sectionsLoaded}
    {error}
    ondownload={model && plansMeta && !searching && (!queryOn || result) ? () => (downloadOpen = true) : null}
  />
  {#if downloadOpen && model && plansMeta}
    <Download bind:open={downloadOpen} {model} {view} {s} {excl} {matches} {meta} {worker} />
  {/if}
  {#if model}
    <aside class="filters"><Filters bind:state={s} {meta} {model} instCounts={view.instCounts} /></aside>
    <main class="results">
      <Results
        rows={view.rows}
        {y0}
        {y1}
        {matches}
        {active}
        selectedKey={s.course}
        {worker}
        q={queryOn ? result?.q : null}
        excl={result?.excl}
        onopen={open}
      />
    </main>
    <section class="course">
      {#if selected}
        <CourseView
          course={selected}
          {version}
          matched={selectedRow?.versions ?? []}
          {worker}
          q={queryOn ? (result?.q ?? '') : ''}
          excl={result?.excl}
          plansReady={!!plansMeta}
          {sectionsLoaded}
          sectionsMeta={meta.sections}
          {model}
          studyTypes={meta.study_types}
          bind:tab={s.tab}
          bind:cmp={s.cmp}
          onversion={(v) => (s.version = versionId(v))}
          onopen={(c, v) => ((s.course = c.key), (s.version = versionId(v)))}
          onclose={close}
        />
      {:else}
        <p class="hint">Select a course to read its plan. Keys: <kbd>/</kbd> search, <kbd>j</kbd>/<kbd>k</kbd> move, <kbd>Enter</kbd> open, <kbd>Esc</kbd> back.</p>
      {/if}
    </section>
  {:else if !error}
    <p class="loading">Loading courses…</p>
  {/if}
</div>

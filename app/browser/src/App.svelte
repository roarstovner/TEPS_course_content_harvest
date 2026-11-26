<script>
  // The app shell: loads the data, keeps the state (mirrored in the URL),
  // sends searches to the worker and lays out the panes (README "Layout").
  import { onMount, tick, untrack } from 'svelte'
  import { inSection, parseQuery } from './search/index.js'
  import { fetchCached, fetchMeta, readParquet } from './data/load.js'
  import { startWorker } from './data/client.js'
  import { addPlans, buildModel, filterView, pickVersion } from './data/model.js'
  import { applyExample, decodeState, encodeState } from './data/url.js'
  import { versionId } from './data/keys.js'
  import Header from './lib/Header.svelte'
  import Filters from './lib/Filters.svelte'
  import Results from './lib/Results.svelte'
  import CourseView from './lib/CourseView.svelte'
  import Download from './lib/Download.svelte'
  import Trend from './lib/Trend.svelte'
  import Coverage from './lib/Coverage.svelte'
  import Screen from './lib/Screen.svelte'
  import Compare from './lib/Compare.svelte'
  import TermLists from './lib/TermLists.svelte'
  import Help from './lib/Help.svelte'
  import Shortcuts from './lib/Shortcuts.svelte'
  import { initialLang, setLang, t } from './lib/i18n.js'
  import { planRef } from './data/compare.js'
  import { trend } from './data/trend.js'

  const base = new URL('data/', document.baseURI).href

  let s = $state(decodeState(location.search))
  let meta = $state.raw(null)
  let baseModel = $state.raw(null)
  let plansMeta = $state.raw(null)
  let sectionsLoaded = $state(false)
  let sectionsLoading = $state(false) // the worker reads the sections' text (on first need)
  let planSections = $state.raw(null) // plan key -> Set of its sections
  let progress = $state({}) // file -> {loaded, total}
  let error = $state(null)
  let result = $state.raw(null) // {q, excl, matches: Map, ms, forms}
  let searching = $state(false)
  let active = $state(0) // keyboard cursor in the results
  let worker = $state.raw(null)
  let searchBox = $state()
  let downloadOpen = $state(false)
  let keysOpen = $state(false) // the shortcuts overlay
  let resultsPane = $state()

  // the UI language: the page re-renders under {#key lang} when it changes
  let lang = $state(initialLang())
  setLang(untrack(() => lang))
  async function switchLang() {
    const l = lang === 'nb' ? 'en' : 'nb'
    setLang(l, true)
    lang = l
    await tick()
    document.getElementById('lang-toggle')?.focus()
  }

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
    else if (e.type === 'sectionIndex') planSections = new Map(e.has.map(([k, codes]) => [k, new Set(codes)]))
    else if (e.type === 'sectionsLoading') sectionsLoading = true
    else if (e.type === 'sections') sectionsLoaded = true
    else if (e.type === 'error') error = e.message
  }

  // versions are attached once the worker has sent the plans' metadata
  const model = $derived(baseModel && plansMeta ? addPlans(baseModel, plansMeta) : baseModel)

  // the query as run: in the section of the header's control, when one is
  // chosen (a term's own prefix wins; search/parse.js `inSection`)
  const query = $derived(inSection(s.q, s.in))
  const parsed = $derived(parseQuery(query))
  const queryOn = $derived(!!parsed.ast && !parsed.error)
  // excluded forms of the terms in the query (others stay in `s.excl`, so
  // that retyping a term brings them back, but are not applied or written)
  const excl = $derived(
    Object.fromEntries(parsed.terms.filter((t) => s.excl[t.key]?.length).map((t) => [t.key, [...s.excl[t.key]]])),
  )

  let timer
  let searchId = 0
  $effect(() => {
    const { scope, n } = s
    const q = query
    const ex = excl
    const on = queryOn && !!plansMeta
    clearTimeout(timer)
    if (!queryOn) {
      worker?.call('cancel') // stop a running search
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
        if (id !== searchId || r.superseded) return
        searching = false
        // the word forms do not depend on the excluded forms or the scope
        const forms = q === result?.q ? result.forms : []
        result = r.error ? null : { q, excl: ex, matches: new Map(r.matches), ms: r.ms, forms }
      } catch (e) {
        if (id === searchId) {
          searching = false
          error = String(e?.message ?? e)
        }
      }
    }, 150)
  })

  // the word forms of the terms over the plans in the filters (asked after
  // the search, so that the results show first; again when the filters change)
  const formKeys = $derived(
    model && plansMeta && queryOn ? filterView(model, s, null, planSections).rows.flatMap((r) => r.versions.map((v) => v.key)) : null,
  )
  let formsFor = null // {q, keys} of the forms in `result`
  let formsId = 0
  $effect(() => {
    const q = result?.q
    const keys = formKeys
    if (!q || !keys || (formsFor?.q === q && formsFor.keys === keys)) return
    const id = ++formsId
    worker.call('forms', { q, keys }).then(
      (f) => {
        if (id !== formsId || !f || result?.q !== q) return
        formsFor = { q, keys }
        result = { ...result, forms: f }
      },
      () => {},
    )
  })

  const matches = $derived(queryOn ? (result?.matches ?? null) : null)
  const view = $derived(model ? filterView(model, s, matches, planSections) : null)
  // the latest year of the filter, for the counts line
  const latest = $derived(model && matches ? trend(model, s, matches).total.at(-1) : null)
  const y0 = $derived(s.y0 ?? model?.yearFrom)
  const y1 = $derived(s.y1 ?? model?.yearMax)
  const selected = $derived(model && s.course ? (model.byKey.get(s.course) ?? null) : null)
  const selectedRow = $derived(selected && view ? view.rows.find((r) => r.course === selected) : null)
  const version = $derived(selected ? pickVersion(selected, selectedRow, s.version, versionId) : null)
  // the plan pane takes room only when it shows a plan (list view, a course open)
  const showCourse = $derived(!!selected && s.view === 'list')

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
  // a number in the trend or coverage view: its courses in the list
  function pick({ y0, y1, inst, st }) {
    if (y0 != null) [s.y0, s.y1] = [y0, y1]
    if (inst) s.inst = [inst]
    if (st) s.st = [st]
    s.view = 'list'
  }
  // a cell of the term heatmap: the term as the query, its courses in the list
  function pickTerm({ q, inst, y }) {
    s.q = q
    s.course = null
    pick({ y0: y, y1: y, inst })
  }
  // a hit in the screening view: its plan version in the list view
  function openHit(h) {
    s.course = `${h.institution}|${h.course_code}`
    s.version = versionId(h)
    s.tab = 'plan'
    s.view = 'list'
  }
  // a plan in the compare view: its version in the list view
  function openPlan(course, version) {
    s.course = course.key
    s.version = versionId(version)
    s.tab = 'plan'
    s.view = 'list'
  }
  // the shown plan to the compare view: side A, or B when A holds another plan
  function compare(course, version) {
    const ref = planRef(course, version)
    if (!s.ca || s.ca === ref) s.ca = ref
    else s.cb = ref
    s.cm = 'pair'
    s.view = 'compare'
  }
  // the focus goes back to the course's row (the pane it was in is gone)
  async function close() {
    const key = s.course
    s.course = null
    s.version = null
    await tick()
    const f = document.activeElement
    if (key && (!f || f === document.body)) resultsPane?.querySelector(`.row[data-key="${CSS.escape(key)}"]`)?.focus()
  }
  // the user guide: opened from the header or with `?`, closed back to the
  // view it was opened from
  let helpFrom = 'list'
  function toggleHelp() {
    if (s.view === 'help') s.view = helpFrom
    else [helpFrom, s.view] = [s.view, 'help']
  }

  // j/k: the cursor moves and the row takes the focus (screen readers follow)
  async function move(i) {
    active = i
    await tick()
    resultsPane?.querySelector(`.row[data-i="${i}"]`)?.focus()
  }

  function onKey(e) {
    const el = e.target
    // a dialog (download, shortcuts) keeps its own keys; Esc closes it
    if (keysOpen || downloadOpen || (el instanceof Element && el.closest('dialog'))) return
    const typing = el instanceof HTMLElement && (el.isContentEditable || /^(INPUT|TEXTAREA|SELECT)$/.test(el.tagName))
    if (e.key === 'Escape') {
      if (typing) el.blur()
      else if (s.view === 'help') toggleHelp()
      else if (s.course) close()
      return
    }
    if (typing || e.ctrlKey || e.metaKey || e.altKey) return
    if (e.key === '?') {
      e.preventDefault()
      keysOpen = true
      return
    }
    if (!view) return
    const n = view.rows.length
    const row = el instanceof Element ? el.closest('.row') : null
    const onRow = !!row
    const cur = row ? Number(row.dataset.i) : active
    if (e.key === '/') {
      e.preventDefault()
      searchBox?.focus()
    } else if (s.view === 'screen') return // the screening view has its own keys
    else if (s.view !== 'list') return
    else if ((e.key === 'j' || (e.key === 'ArrowDown' && onRow)) && n) move(Math.min(n - 1, cur + 1))
    else if ((e.key === 'k' || (e.key === 'ArrowUp' && onRow)) && n) move(Math.max(0, cur - 1))
    else if (e.key === 'Enter' && n && s.view === 'list' && !(el instanceof Element && el.closest('button, a, summary'))) open(view.rows[active].course)
    else return
    e.preventDefault()
  }
</script>

<svelte:window onkeydown={onKey} onpopstate={onPopState} />

{#key lang}
<a class="skip" href="#results" onclick={(e) => (e.preventDefault(), resultsPane?.focus())}>{t('app.skip')}</a>
<div class="app" class:withcourse={showCourse}>
  <Header
    bind:state={s}
    bind:input={searchBox}
    {parsed}
    {result}
    {searching}
    counts={view?.counts}
    {latest}
    {queryOn}
    waiting={queryOn && !plansMeta}
    {progress}
    plansReady={!!plansMeta}
    {sectionsLoaded}
    {sectionsLoading}
    sections={meta?.sections ?? []}
    {error}
    onhelp={toggleHelp}
    onkeys={() => (keysOpen = true)}
    onlang={switchLang}
    ondownload={model && plansMeta && !searching && (!queryOn || result) ? () => (downloadOpen = true) : null}
  />
  {#if downloadOpen && model && plansMeta}
    <Download bind:open={downloadOpen} {model} {view} {s} {query} {excl} {matches} {meta} {worker} />
  {/if}
  {#if keysOpen}
    <Shortcuts bind:open={keysOpen} onguide={() => s.view !== 'help' && toggleHelp()} />
  {/if}
  {#if model}
    <aside class="filters" aria-label={t('f.label')}><Filters bind:state={s} {meta} {model} facets={view.facets} sectionsReady={!!planSections} /></aside>
    <main class="results" id="results" tabindex="-1" bind:this={resultsPane}>
      <nav class="viewtabs" aria-label={t('app.views')}>
        {#each ['list', 'trend', 'coverage', 'screen', 'compare', 'terms'] as v (v)}
          <button class:current={s.view === v} aria-pressed={s.view === v} onclick={() => (s.view = v)}>{t(`view.${v}`)}</button>
        {/each}
      </nav>
      {#if s.view === 'help'}
        <Help onexample={(h) => applyExample(s, h)} />
      {:else if s.view === 'trend'}
        <Trend {model} f={s} {matches} {queryOn} q={result?.q} {meta} bind:by={s.by} onpick={pick} />
      {:else if s.view === 'coverage'}
        <Coverage {model} bind:f={s} {meta} {planSections} onpick={pick} />
      {:else if s.view === 'screen'}
        <Screen {model} {view} bind:s q={matches ? result.q : ''} excl={result?.excl ?? {}} {matches} {meta} {worker} onopen={openHit} />
      {:else if s.view === 'compare'}
        <Compare {model} {view} bind:s {y0} {y1} {meta} {worker} q={queryOn ? (result?.q ?? '') : ''} excl={result?.excl} onopen={openPlan} />
      {:else if s.view === 'terms'}
        <TermLists {model} bind:s {meta} {worker} plansReady={!!plansMeta} onpick={pickTerm} />
      {:else}
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
        {#if !selected}<p class="hint">{t('app.hint')}</p>{/if}
      {/if}
    </main>
    {#if showCourse}
      <section class="course" aria-label={selected.course_code}>
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
          oncompare={(v) => compare(selected, v)}
          onclose={close}
        />
      </section>
    {/if}
  {:else if !error}
    <p class="loading" role="status">{t('app.loading')}</p>
  {/if}
</div>
{/key}

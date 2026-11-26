<script>
  // Search box, scope of AND, the section searched, word forms and the counts line.
  import WordForms from './WordForms.svelte'
  import { mb, num, pct } from './format.js'
  import { local, other, t } from './i18n.js'

  let {
    state = $bindable(),
    input = $bindable(),
    parsed,
    result,
    searching,
    counts,
    latest,
    queryOn,
    waiting,
    progress,
    plansReady,
    sectionsLoaded,
    sectionsLoading,
    sections, // meta.sections: {code, query, label_en, label_no}
    error,
    ondownload,
    onhelp,
    onkeys,
    onlang,
  } = $props()

  const SCOPES = ['plan', 'section', 'paragraph', 'sentence', 'words']
  const FILES = ['courses', 'course_years', 'programmes', 'plans', 'sections']
  const loading = $derived(FILES.filter((f) => progress[f] && progress[f].loaded < progress[f].total))
  const err = $derived(parsed.error)

  function toggleForm(key, form, on) {
    const cur = new Set(state.excl[key] ?? [])
    if (on) cur.delete(form)
    else cur.add(form)
    state.excl = { ...state.excl, [key]: [...cur] }
  }
</script>

<header class="header">
  <div class="searchrow">
    <input
      bind:this={input}
      bind:value={state.q}
      type="search"
      placeholder={t('h.placeholder')}
      aria-label={t('h.search')}
      aria-invalid={!!err}
      aria-describedby={err ? 'qerror' : undefined}
      spellcheck="false"
      autocomplete="off"
    />
    <label>
      {t('h.scope')}
      <select bind:value={state.scope} aria-label={t('h.scopeAria')}>
        {#each SCOPES as v (v)}<option value={v}>{t(`scope.${v}`)}</option>{/each}
      </select>
    </label>
    {#if state.scope === 'words'}
      <label>N <input class="n" type="number" min="1" max="1000" bind:value={state.n} /></label>
    {/if}
    <label title={t('h.inTitle')}>
      {t('h.in')}
      <select bind:value={state.in} aria-label={t('h.inAria')} class:on={!!state.in}>
        <option value={null}>{t('h.full')}</option>
        {#each sections as x (x.code)}<option value={x.code} title="{other(x, 'label')} ({x.query}:)">{local(x, 'label').toLowerCase()}</option>{/each}
      </select>
    </label>
    <button type="button" class="dlbutton" onclick={ondownload} disabled={!ondownload} title={t('h.downloadTitle')}>{t('h.download')}</button>
    <button type="button" class="dlbutton" aria-pressed={state.view === 'help'} onclick={onhelp} title={t('h.helpTitle')}>{t('h.help')}</button>
    <button type="button" class="dlbutton" onclick={onkeys} title={t('h.keysTitle')} aria-keyshortcuts="?">{t('h.keys')}</button>
    <button type="button" class="dlbutton langbtn" id="lang-toggle" onclick={onlang} lang={t('h.lang') === 'Norsk' ? 'nb' : 'en'} title={t('h.langTitle')} aria-label={t('h.langTitle')}>{t('h.lang')}</button>
  </div>

  {#if err}
    <div class="qerror" id="qerror" role="alert">
      <code
        >{state.q.slice(0, err.start)}<mark>{state.q.slice(err.start, err.end) || ' '}</mark>{state.q.slice(err.end)}</code
      >
      <span>{err.message}</span>
    </div>
  {/if}

  {#if queryOn && result?.forms.length}
    <WordForms forms={result.forms} excl={state.excl} ontoggle={toggleForm} />
  {/if}

  <div class="counts">
    <!-- the counts are announced; the loading notes are not -->
    <span aria-live="polite" aria-atomic="true">
      {#if counts}
        <strong>{num(counts.courses)}</strong> {t('c.courses')}{#if plansReady}, <strong>{num(counts.versions)}</strong> {t('c.versions')}{/if}
        {#if queryOn && result}
          · {t('c.match', { m: num(counts.matching), w: num(counts.withPlan), p: pct(counts.matching, counts.withPlan) })}
          {#if latest?.withPlan}
            · {t('c.latest', { p: pct(latest.matching, latest.withPlan), y: latest.year, c: pct(latest.withPlan, latest.n) })}
          {/if}
        {/if}
        · {t('c.coverage', { p: pct(counts.withPlan, counts.courseYears), n: num(counts.courseYears) })}
      {/if}
    </span>
    {#if waiting}<span class="note">· {t('c.waiting')}</span>
    {:else if searching}<span class="note">· {t('c.searching')}</span>
    {:else if queryOn && result}<span class="note">· {Math.round(result.ms)} ms</span>{/if}
    {#each loading as f (f)}
      <span class="note">· {t('c.loading', { f, a: mb(progress[f].loaded), b: mb(progress[f].total) })}</span>
    {/each}
    {#if progress.plans && !loading.length && !plansReady}<span class="note">· {t('c.preparing')}</span>{/if}
    {#if sectionsLoading && !sectionsLoaded}<span class="note">· {t('c.sections')}</span>{/if}
    {#if error}<span class="fail" role="alert">· {error}</span>{/if}
  </div>
</header>

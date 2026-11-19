<script>
  // Search box, scope of AND, word forms and the counts line.
  import WordForms from './WordForms.svelte'
  import { mb, num, pct } from './format.js'

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
    error,
    ondownload,
  } = $props()

  const SCOPES = [
    ['plan', 'plan'],
    ['section', 'section'],
    ['paragraph', 'line'],
    ['sentence', 'sentence'],
    ['words', 'within N words'],
  ]
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
      placeholder="Search the plans, e.g. fremmedspråk* morsmål*  (press / to focus)"
      aria-label="Search"
      aria-invalid={!!err}
      spellcheck="false"
      autocomplete="off"
    />
    <label>
      all terms in the same
      <select bind:value={state.scope} aria-label="Scope of AND">
        {#each SCOPES as [v, label] (v)}<option value={v}>{label}</option>{/each}
      </select>
    </label>
    {#if state.scope === 'words'}
      <label>N <input class="n" type="number" min="1" max="1000" bind:value={state.n} /></label>
    {/if}
    <button type="button" class="dlbutton" onclick={ondownload} disabled={!ondownload} title="Download the selection">Download</button>
  </div>

  {#if err}
    <div class="qerror" role="alert">
      <code
        >{state.q.slice(0, err.start)}<mark>{state.q.slice(err.start, err.end) || ' '}</mark>{state.q.slice(err.end)}</code
      >
      <span>{err.message}</span>
    </div>
  {/if}

  {#if queryOn && result?.forms.length}
    <WordForms forms={result.forms} excl={state.excl} ontoggle={toggleForm} />
  {/if}

  <div class="counts" aria-live="polite">
    {#if counts}
      <strong>{num(counts.courses)}</strong> courses{#if plansReady}, <strong>{num(counts.versions)}</strong> plan versions{/if}
      {#if queryOn && result}
        · {num(counts.matching)} of {num(counts.withPlan)} course-years with a plan match ({pct(
          counts.matching,
          counts.withPlan,
        )})
        {#if latest?.withPlan}
          · {pct(latest.matching, latest.withPlan)} of course-years in force in {latest.year} (coverage {pct(
            latest.withPlan,
            latest.n,
          )})
        {/if}
      {/if}
      · coverage {pct(counts.withPlan, counts.courseYears)} of {num(counts.courseYears)} course-years
    {/if}
    {#if waiting}<span class="note">· search runs when the plan text has loaded</span>
    {:else if searching}<span class="note">· searching…</span>
    {:else if queryOn && result}<span class="note">· {Math.round(result.ms)} ms</span>{/if}
    {#each loading as f (f)}
      <span class="note">· loading {f} {mb(progress[f].loaded)} / {mb(progress[f].total)}</span>
    {/each}
    {#if progress.plans && !loading.length && !plansReady}<span class="note">· preparing text…</span>{/if}
    {#if sectionsLoading && !sectionsLoaded}<span class="note">· loading sections…</span>{/if}
    {#if error}<span class="fail">· {error}</span>{/if}
  </div>
</header>

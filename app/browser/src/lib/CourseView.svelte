<script>
  // Right pane: one course, a version of its plan with the matches marked,
  // its versions, and its sections. The text comes from the worker.
  import { segments } from './format.js'

  let { course, version, matched, worker, q, excl, plansReady, sectionsLoaded, sectionsMeta, tab = $bindable(), onversion, onclose } =
    $props()

  let doc = $state.raw(null)
  let body = $state()
  const matchedKeys = $derived(new Set(matched.map((v) => v.key)))

  let reqId = 0
  $effect(() => {
    const key = version?.key
    sectionsLoaded // fetch again when the sections have arrived
    if (!key || !plansReady) return
    const id = ++reqId
    worker.call('doc', { key, q, excl }).then((d) => {
      if (id === reqId) doc = d ? { ...d, key } : null
    })
  })

  // open at the first match
  $effect(() => {
    doc
    tab
    queueMicrotask(() => {
      const m = body?.querySelector('mark')
      if (m) m.scrollIntoView({ block: 'center' })
      else body?.scrollTo?.(0, 0)
    })
  })

  const years = (v) => (v.year_from === v.year_to ? `${v.year_from}` : `${v.year_from}–${v.year_to}`)
  const sections = $derived(
    doc?.sections ? sectionsMeta.filter((s) => doc.sections[s.code]).map((s) => ({ ...s, ...doc.sections[s.code] })) : [],
  )
</script>

<div class="courseview">
  <div class="chead">
    <button class="link back" onclick={onclose} title="Back to the list (Esc)">← list</button>
    <h2><span class="code">{course.course_code}</span> {course.course_name}</h2>
    <p class="sub">
      {course.institution.toUpperCase()} · {course.credits} credits · level {course.level_code}
      {#if course.subject_name}· {course.subject_name}{/if} · taught {course.year_first}–{course.year_last}
    </p>
    {#if course.versions.length}
      <div class="versions" role="tablist" aria-label="Plan versions">
        {#each course.versions as v (v.key)}
          <button
            class="ver"
            class:current={v.key === version?.key}
            class:match={matchedKeys.has(v.key)}
            onclick={() => onversion(v)}
            title={v.source_url}>{years(v)}</button
          >
        {/each}
      </div>
    {/if}
    <div class="tabs">
      <button class:current={tab === 'plan'} onclick={() => (tab = 'plan')}>Plan</button>
      <button class:current={tab === 'sections'} onclick={() => (tab = 'sections')}>Sections</button>
      {#if version}<a class="src" href={version.source_url} target="_blank" rel="noopener noreferrer">source</a>{/if}
    </div>
  </div>

  <div class="cbody" bind:this={body}>
    {#if !course.versions.length && plansReady}
      <p class="hint">No plan was found for this course.</p>
    {:else if !doc || doc.key !== version?.key}
      <p class="hint">Loading the plan text…</p>
    {:else if tab === 'plan'}
      <div class="plantext">{#each segments(doc.text, doc.marks) as g, i (i)}{#if g.term < 0}{g.text}{:else}<mark class="t{g.term % 6}">{g.text}</mark>{/if}{/each}</div>
    {:else if !doc.sections}
      <p class="hint">Loading the sections…</p>
    {:else if !sections.length}
      <p class="hint">No sections were extracted from this plan.</p>
    {:else}
      {#each sections as s (s.code)}
        <h3 title={s.label_en}>{s.label_no} <small>{s.query}:</small></h3>
        <div class="plantext">{#each segments(s.text, s.marks) as g, i (i)}{#if g.term < 0}{g.text}{:else}<mark class="t{g.term % 6}">{g.text}</mark>{/if}{/each}</div>
      {/each}
    {/if}
  </div>
</div>

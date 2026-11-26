<script>
  // Right pane: one course, a version of its plan with the matches marked
  // and the sections in the margin, its sections (also those it lacks), a
  // comparison of two versions, and the source. The text comes from the
  // worker.
  import { segments } from './format.js'
  import { local, other, t } from './i18n.js'
  import PlanText from './PlanText.svelte'
  import Versions from './Versions.svelte'
  import Source from './Source.svelte'

  let {
    course,
    version,
    matched,
    worker,
    q,
    excl,
    plansReady,
    sectionsLoaded,
    sectionsMeta,
    model,
    studyTypes,
    tab = $bindable(),
    cmp = $bindable(),
    onversion,
    onopen,
    oncompare,
    onclose,
  } = $props()

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
      else (body?.querySelector('.ptext') ?? body)?.scrollTo?.(0, 0)
    })
  })

  const years = (v) => (v.year_from === v.year_to ? `${v.year_from}` : `${v.year_from}–${v.year_to}`)
  const TABS = [
    ['plan', 'cv.plan'],
    ['sections', 'cv.sections'],
    ['versions', 'cv.versionsTab'],
    ['source', 'cv.source'],
  ]
</script>

{#snippet marked(text, marks)}{#each segments(text, marks) as g, i (i)}{#if g.term < 0}{g.text}{:else}<mark class="t{g.term % 6}">{g.text}</mark>{/if}{/each}{/snippet}

<div class="courseview">
  <div class="chead">
    <button class="link back" onclick={onclose} title={t('cv.backTitle')} aria-keyshortcuts="Escape">{t('cv.back')}</button>
    <h2><span class="code">{course.course_code}</span> {course.course_name}</h2>
    <p class="sub">
      {course.institution.toUpperCase()} · {t('cv.credits', { n: course.credits })} · {t('cv.level', { l: course.level_code })}
      {#if course.subject_name}· {course.subject_name}{/if} · {t('cv.taught', { a: course.year_first, b: course.year_last })}
    </p>
    {#if course.versions.length}
      <div class="versions" role="group" aria-label={t('cv.versions')}>
        {#each course.versions as v (v.key)}
          <button
            class="ver"
            class:current={v.key === version?.key}
            class:match={matchedKeys.has(v.key)}
            aria-pressed={v.key === version?.key}
            onclick={() => onversion(v)}
            title={v.source_url}>{years(v)}</button
          >
        {/each}
      </div>
    {/if}
    <div class="tabs" role="group" aria-label={t('cv.tabs')}>
      {#each TABS as [k, name] (k)}
        <button class:current={tab === k} aria-pressed={tab === k} onclick={() => (tab = k)}>{t(name)}{#if k === 'versions' && course.versions.length > 1}&nbsp;<small>{course.versions.length}</small>{/if}</button>
      {/each}
      {#if version}<button class="link cmpbtn" onclick={() => oncompare(version)} title={t('cv.compareTitle')}>{t('cv.compare')}</button>{/if}
      {#if version}<a class="src" href={version.source_url} target="_blank" rel="noopener noreferrer">{t('cv.sourceLink')}<span class="sr">{t('cv.newTab')}</span></a>{/if}
    </div>
  </div>

  <!-- the plan scrolls in its own box under the section legend (PlanText) -->
  <div class="cbody" class:plan={tab === 'plan' && doc?.key === version?.key} bind:this={body}>
    {#if !course.versions.length && plansReady}
      <p class="hint">{t('cv.noPlan')}</p>
    {:else if !doc || doc.key !== version?.key}
      <p class="hint" role="status">{t('cv.loadingText')}</p>
    {:else if tab === 'plan'}
      <PlanText text={doc.text} marks={doc.marks} sections={doc.sections} {sectionsMeta} />
    {:else if tab === 'versions'}
      <Versions {course} {version} {doc} {worker} {q} {excl} bind:cmp {onversion} />
    {:else if tab === 'source'}
      <Source {course} {version} {model} {studyTypes} {onopen} />
    {:else if !doc.sections}
      <p class="hint" role="status">{t('cv.loadingSections')}</p>
    {:else}
      {#each sectionsMeta as s, i (s.code)}
        {@const sec = doc.sections[s.code]}
        <h3 class="sechead s{i}" class:absent={!sec} title={other(s, 'label')}>
          {local(s, 'label')} <small>{s.query}:</small>{#if !sec}<small> {t('cv.notIn')}</small>{/if}
        </h3>
        {#if sec}<div class="plantext">{@render marked(sec.text, sec.marks)}</div>{/if}
      {/each}
    {/if}
  </div>
</div>

<script>
  // Middle pane, compare view: two plan versions side by side (any two, or
  // two versions of one course; differences marked word by word, as the
  // versions tab does), or the plans in force in one year of the courses in
  // the list, a column per institution; the whole plan or one section. The
  // text comes from the worker.
  import { acrossPlans, defaultOther, findCourses, planRef, resolveRef } from '../data/compare.js'
  import { versionId } from '../data/keys.js'
  import { diffStats, sides, wordDiff } from './diff.js'
  import { num, segments, segmentsIn } from './format.js'
  import PlanText from './PlanText.svelte'

  let { model, view, s = $bindable(), y0, y1, meta, worker, q, excl, onopen } = $props()

  const MAX = 40 // plans across institutions
  const years = (v) => (v.year_from === v.year_to ? `${v.year_from}` : `${v.year_from}–${v.year_to}`)
  const secLabel = $derived(new Map(meta.sections.map((x) => [x.code, x.label_en])))

  // two plans: B defaults to the version before A
  const A = $derived(resolveRef(model, s.ca))
  const B = $derived(resolveRef(model, s.cb) ?? (s.cb ? null : defaultOther(A)))
  const pair = $derived([
    ['ca', 'A', A],
    ['cb', 'B', B],
  ])
  // across: the year, by default the last of the filter
  const year = $derived(s.cyear != null && s.cyear >= y0 && s.cyear <= y1 ? s.cyear : y1)
  const across = $derived(s.cm === 'across' ? acrossPlans(view.rows, year, MAX) : null)

  const keys = $derived(
    s.cm === 'pair' ? [A, B].filter(Boolean).map((x) => x.version.key) : across.columns.flatMap((c) => c.plans.map((p) => p.version.key)),
  )
  let docs = $state.raw(new Map()) // plan key -> doc (worker `doc`)
  let loading = $state(false)
  let reqId = 0
  $effect(() => {
    const ks = keys
    const [qq, ex] = [q, $state.snapshot(excl)]
    const id = ++reqId
    if (!ks.length) return void (docs = new Map())
    loading = true
    worker.call('docs', { keys: ks, q: qq, excl: ex }).then((ds) => {
      if (id !== reqId) return
      docs = new Map(ks.map((k, i) => [k, ds[i]]))
      loading = false
    })
  })
  // the text shown of a plan: the whole plan or one section (undefined: not in this plan)
  const shown = (key) => {
    const d = docs.get(key)
    return d && (s.csec ? d.sections?.[s.csec] : d)
  }

  // the differences of the two texts shown
  let diff = $state.raw(null) // {key, a, b, stats}
  const diffKey = $derived(A && B && s.diff ? `${A.version.key}>${B.version.key}>${s.csec}` : null)
  $effect(() => {
    const k = diffKey
    if (!k) return
    const [a, b] = [shown(A.version.key), shown(B.version.key)]
    if (!a || !b) return
    wordDiff(a.text, b.text).then((parts) => {
      if (k === diffKey) diff = { key: k, ...sides(parts), stats: diffStats(parts) }
    })
  })
  const diffReady = $derived(diff?.key === diffKey)

  // the plan picker of a side
  let typed = $state({ ca: '', cb: '' })
  const optLabel = (c) => `${c.institution.toUpperCase()} ${c.course_code} ${c.course_name ?? ''}`.trim()
  const options = $derived(Object.fromEntries(['ca', 'cb'].map((k) => [k, findCourses(model, typed[k])])))
  function pickCourse(k, text) {
    const c = options[k].find((o) => optLabel(o) === text)
    if (!c) return
    s[k] = planRef(c, c.versions[0])
    typed[k] = ''
  }
  const setVersion = (k, side, vid) => (s[k] = planRef(side.course, side.course.versions.find((v) => versionId(v) === vid)))
  function swap() {
    const [a, b] = [A, B]
    s.ca = b ? planRef(b.course, b.version) : null
    s.cb = a ? planRef(a.course, a.version) : null
  }
</script>

{#snippet marked(text, marks)}{#each segments(text, marks) as g, i (i)}{#if g.term < 0}{g.text}{:else}<mark class="t{g.term % 6}">{g.text}</mark>{/if}{/each}{/snippet}
{#snippet diffed(parts, src)}{#each parts as p, i (i)}{@const segs = segmentsIn(src.text, p.start, p.start + p.text.length, src.marks)}{#if p.op === 'eq'}{@render pieces(segs)}{:else if p.op === 'ins'}<ins>{@render pieces(segs)}</ins>{:else}<del>{@render pieces(segs)}</del>{/if}{/each}{/snippet}
{#snippet pieces(segs)}{#each segs as g, j (j)}{#if g.term < 0}{g.text}{:else}<mark class="t{g.term % 6}">{g.text}</mark>{/if}{/each}{/snippet}
{#snippet planHead(course, version)}
  <span class="inst">{course.institution.toUpperCase()}</span> <span class="code">{course.course_code}</span>
  {course.course_name} <span class="note">{years(version)}</span>
  <button class="link" onclick={() => onopen(course, version)} title="Open in the list view">open</button>
{/snippet}

<div class="trend compare">
  <div class="trendbar">
    <label><input type="radio" bind:group={s.cm} value="pair" /> two plans</label>
    <label><input type="radio" bind:group={s.cm} value="across" /> one year across institutions</label>
    <label
      >Text <select bind:value={s.csec}>
        <option value={null}>whole plan</option>
        {#each meta.sections as x (x.code)}<option value={x.code}>{x.label_en}</option>{/each}
      </select></label
    >
    {#if s.cm === 'pair'}
      <label><input type="checkbox" bind:checked={s.diff} /> mark differences</label>
      <button class="link" onclick={swap} disabled={!A && !B}>swap A and B</button>
      {#if s.diff && diffReady}<span class="note">words only in A <del>{num(diff.stats.del)}</del>, only in B <ins>{num(diff.stats.ins)}</ins></span>{/if}
    {:else}
      <label
        >Year <select bind:value={() => year, (v) => (s.cyear = v)}>
          {#each Array.from({ length: y1 - y0 + 1 }, (_, i) => y1 - i) as y (y)}<option value={y}>{y}</option>{/each}
        </select></label
      >
    {/if}
    {#if loading}<span class="note">loading…</span>{/if}
  </div>

  {#if s.cm === 'pair'}
    <p class="note sub">
      Any two plan versions: pick a course on each side, or use <em>compare</em> in a course's plan pane. B is by default the
      version before A. Words only in A are struck through, words only in B underlined; matches of the query stay marked.
    </p>
    <div class="cmpcols">
      {#each pair as [k, name, side] (k)}
        {@const doc = side && docs.get(side.version.key)}
        {@const t = side && shown(side.version.key)}
        <section class="cmpcol">
          <header>
            <div class="pick">
              <strong>{name}</strong>
              <input
                list="cmp-{k}"
                placeholder={side ? 'another course…' : 'find a course: institution, code or name'}
                value={typed[k]}
                oninput={(e) => ((typed[k] = e.currentTarget.value), pickCourse(k, e.currentTarget.value))}
              />
              <datalist id="cmp-{k}">{#each options[k] as c (c.key)}<option value={optLabel(c)}></option>{/each}</datalist>
              {#if side && side.course.versions.length > 1}
                <select value={versionId(side.version)} onchange={(e) => setVersion(k, side, e.currentTarget.value)} aria-label="Version">
                  {#each side.course.versions as v (v.key)}<option value={versionId(v)}>{years(v)} ({versionId(v).slice(0, 6)})</option>{/each}
                </select>
              {/if}
            </div>
            {#if side}<div class="phead">{@render planHead(side.course, side.version)}</div>{/if}
          </header>
          <div class="cbox" class:planbox={!s.diff && !s.csec && t}>
            {#if !side}
              <p class="hint">{k === 'cb' && A ? 'This course has one version: pick another plan.' : 'No plan chosen.'}</p>
            {:else if !doc}
              <p class="hint">Loading the plan text…</p>
            {:else if !t}
              <p class="hint">{secLabel.get(s.csec)}: not in this plan.</p>
            {:else if s.diff && A && B && shown(A.version.key) && shown(B.version.key)}
              {#if diffReady}<div class="plantext diff">{@render diffed(k === 'ca' ? diff.a : diff.b, t)}</div>{:else}<p class="hint">Comparing…</p>{/if}
            {:else if s.csec}
              <div class="plantext">{@render marked(t.text, t.marks)}</div>
            {:else}
              <PlanText text={t.text} marks={t.marks} sections={doc.sections} sectionsMeta={meta.sections} />
            {/if}
          </div>
        </section>
      {/each}
    </div>
  {:else}
    <p class="note sub">
      The plans in force in {year} of the courses in the list (the filters and the query; set the subject or study type to
      line up one subject), a column per institution{#if s.csec}, section {secLabel.get(s.csec)}{/if}.
      {#if across.total > MAX}Showing the first {MAX} of {num(across.total)} plans: narrow the filters.{:else}{num(across.total)} plans.{/if}
    </p>
    {#if !across.total}
      <p class="hint">No plan of the courses in the list is in force in {year}.</p>
    {:else}
      <div class="cmpcols across">
        {#each across.columns as col (col.institution)}
          <section class="cmpcol">
            <header><strong title={meta.institutions.find((i) => i.code === col.institution)?.institution_name}>{col.institution.toUpperCase()}</strong> <span class="note">{col.plans.length} {col.plans.length === 1 ? 'plan' : 'plans'}</span></header>
            <div class="cbox">
              {#each col.plans as p (p.version.key)}
                {@const t = shown(p.version.key)}
                <article>
                  <h4>{@render planHead(p.course, p.version)}</h4>
                  {#if !docs.get(p.version.key)}<p class="note">loading…</p>
                  {:else if !t}<p class="note">{secLabel.get(s.csec)}: not in this plan.</p>
                  {:else}<div class="plantext">{@render marked(t.text, t.marks)}</div>{/if}
                </article>
              {/each}
            </div>
          </section>
        {/each}
      </div>
    {/if}
  {/if}
</div>

<style>
  .cmpcols { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 12px; height: max(360px, calc(100vh - 210px)); }
  .cmpcols.across { grid-template-columns: none; grid-auto-flow: column; grid-auto-columns: minmax(320px, 1fr); overflow-x: auto; }
  .cmpcol { display: flex; flex-direction: column; min-height: 0; border: 1px solid var(--line); border-radius: 4px; }
  .cmpcol header { flex: none; padding: 6px 10px; border-bottom: 1px solid var(--line); background: var(--bg-2); font-size: 13px; }
  .pick { display: flex; gap: 6px; align-items: center; flex-wrap: wrap; }
  .pick input { flex: 1; min-width: 12em; font-size: 12px; }
  .pick select { font-size: 12px; max-width: 14em; }
  .phead { margin-top: 4px; }
  .inst { color: var(--fg-2); font-size: 12px; }
  .code { font: 12px var(--mono); }
  .cbox { flex: 1; overflow: auto; padding: 8px 12px 24px; min-height: 0; }
  /* the plan with its section margin: the legend above, the text scrolling below */
  .cbox.planbox { display: flex; flex-direction: column; overflow: hidden; padding: 0; }
  .cbox.planbox :global(.legend) { flex: none; padding: 6px 12px 4px 18px; border-bottom: 1px solid var(--line); }
  .cbox.planbox :global(.ptext) { padding: 8px 12px 24px 18px; }
  .cbox .plantext { font-size: 14px; }
  article + article { border-top: 1px solid var(--line); margin-top: 10px; }
  h4 { font-size: 13px; font-weight: 600; margin: 8px 0 4px; }
  @media (max-width: 900px) {
    .cmpcols:not(.across) { grid-template-columns: none; grid-auto-flow: column; grid-auto-columns: minmax(300px, 85%); overflow-x: auto; }
  }
</style>

<script>
  // Word-level comparison of two versions of a course's plan: insertions and
  // deletions marked inline, the matches of the query kept. The other
  // version's text comes from the worker.
  import { versionId } from '../data/keys.js'
  import { diffStats, wordDiff } from './diff.js'
  import { segmentsIn } from './format.js'

  let { course, version, doc, worker, q, excl, cmp = $bindable(), onversion } = $props()

  const years = (v) => (v.year_from === v.year_to ? `${v.year_from}` : `${v.year_from}–${v.year_to}`)
  // default: the version before the shown one (versions are latest first)
  const other = $derived.by(() => {
    const vs = course.versions
    const i = vs.indexOf(version)
    return vs.find((v) => v !== version && versionId(v) === cmp) ?? vs[i + 1] ?? vs[i - 1] ?? null
  })
  // older on the left
  const swap = $derived(other && other.year_from > version.year_from)

  let res = $state.raw(null) // {key, parts, a, b, stats}
  let box = $state()
  let reqId = 0
  $effect(() => {
    if (!other || !doc) return
    const id = ++reqId
    const key = `${other.key}>${version.key}`
    worker.call('doc', { key: other.key, q, excl }).then(async (o) => {
      if (!o || id !== reqId) return
      const [a, b] = swap ? [doc, o] : [o, doc]
      const parts = await wordDiff(a.text, b.text)
      if (id !== reqId) return
      res = { key, parts, a, b, stats: diffStats(parts) }
      queueMicrotask(() => box?.querySelector('ins, del')?.scrollIntoView({ block: 'center' }))
    })
  })
  const ready = $derived(res && other && res.key === `${other.key}>${version.key}`)
  const label = (v) => `${years(v)} (${versionId(v).slice(0, 6)})`
</script>

{#snippet marked(segs)}{#each segs as g, j (j)}{#if g.term < 0}{g.text}{:else}<mark class="t{g.term % 6}">{g.text}</mark>{/if}{/each}{/snippet}

{#if course.versions.length < 2}
  <p class="hint">Only one version of this plan was harvested.</p>
{:else}
  <div class="diffhead">
    <label
      >Compare <select value={versionId(other)} onchange={(e) => (cmp = e.currentTarget.value)}>
        {#each course.versions as v (v.key)}{#if v !== version}<option value={versionId(v)}>{label(v)}</option>{/if}{/each}
      </select></label
    >
    <label
      >with the shown <select value={versionId(version)} onchange={(e) => onversion(course.versions.find((v) => versionId(v) === e.currentTarget.value))}>
        {#each course.versions as v (v.key)}<option value={versionId(v)}>{label(v)}</option>{/each}
      </select></label
    >
    {#if ready}
      <span class="note"><ins>+{res.stats.ins}</ins> <del>−{res.stats.del}</del> words from {years(swap ? version : other)} to {years(swap ? other : version)}</span>
    {/if}
  </div>
  {#if !ready}
    <p class="hint">Comparing…</p>
  {:else if !res.stats.ins && !res.stats.del}
    <p class="hint">The two texts differ only in spacing.</p>
  {:else}
    <div class="plantext diff" bind:this={box}>{#each res.parts as p, i (i)}{@const src = p.op === 'del' ? res.a : res.b}{@const segs = segmentsIn(src.text, p.start, p.start + p.text.length, src.marks)}{#if p.op === 'eq'}{@render marked(segs)}{:else if p.op === 'ins'}<ins>{@render marked(segs)}</ins>{:else}<del>{@render marked(segs)}</del>{/if}{/each}</div>
  {/if}
{/if}

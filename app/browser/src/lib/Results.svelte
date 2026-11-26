<script>
  // Middle pane: one row per course, rendered in pages of PAGE rows, with a
  // year strip and, with a query, a keyword-in-context line of its latest
  // matching version.
  import { yearStrip } from '../data/model.js'
  import { num } from './format.js'
  import { t } from './i18n.js'

  let { rows, y0, y1, matches, active, selectedKey, worker, q, excl, onopen } = $props()

  const PAGE = 100
  let limit = $state(PAGE)
  let snippets = $state.raw(new Map()) // plan key -> kwic lines
  let list = $state()
  let sentinel = $state()

  $effect(() => {
    rows
    limit = PAGE
  })
  $effect(() => {
    if (active >= limit) limit = active + PAGE
  })
  $effect(() => {
    list?.querySelector(`[data-i="${active}"]`)?.scrollIntoView({ block: 'nearest' })
  })
  $effect(() => {
    if (!sentinel) return
    limit // a new observer reports at once whether the sentinel is still in view
    const io = new IntersectionObserver((es) => es.some((e) => e.isIntersecting) && (limit += PAGE))
    io.observe(sentinel)
    return () => io.disconnect()
  })

  // a new query clears the snippets; visible rows fetch theirs
  $effect(() => {
    q
    excl
    snippets = new Map()
  })
  $effect(() => {
    if (!q || !worker) return
    const keys = rows
      .slice(0, limit)
      .map((r) => r.versions[0]?.key)
      .filter((k) => k && !snippets.has(k))
    if (!keys.length) return
    const forQ = q
    worker.call('kwic', { q, excl, keys, max: 1 }).then((res) => {
      if (forQ !== q) return
      const m = new Map(snippets)
      for (const k of keys) m.set(k, res[k] ?? [])
      snippets = m
    })
  })

  // the end of the left context, from a word start, so that the match shows
  const tail = (s) => (s.length > 30 ? s.slice(s.indexOf(' ', s.length - 30) + 1) : s)
  const matchedKeys = (r) => (matches ? new Set(r.course.versions.filter((v) => matches.has(v.key)).map((v) => v.key)) : null)
</script>

{#if !rows.length}<p class="hint">{t('r.none')}</p>{/if}
<ul class="list" bind:this={list} aria-label={t('r.label')}>
  {#each rows.slice(0, limit) as r, i (r.course.key)}
    {@const c = r.course}
    {@const k = snippets.get(r.versions[0]?.key)?.[0]}
    <li><button
      class="row"
      class:active={i === active}
      class:selected={c.key === selectedKey}
      data-i={i}
      data-key={c.key}
      aria-current={c.key === selectedKey ? 'true' : undefined}
      onclick={() => onopen(c)}
    >
      <span class="inst">{c.institution}</span>
      <span class="code">{c.course_code}</span>
      <span class="name">{c.course_name}</span>
      <span class="hits">{#if matches}{t('r.hits', { n: num(r.hits) })}{#if r.versions.length > 1} · {t('r.versions', { n: r.versions.length })}{/if}{/if}</span>
      <span class="strip" aria-hidden="true">
        {#each yearStrip(c, y0, y1, matchedKeys(r)) as cell (cell.year)}<i
            class={cell.state}
            title={cell.state === 'none' ? t('r.noPlan', { y: cell.year, r: cell.reason ?? t('r.noReason') }) : cell.year}
          ></i>{/each}
      </span>
      {#if k}<span class="kwic">…{tail(k.before)}<mark class="t{k.term % 6}">{k.match}</mark>{k.after}…</span>{/if}
    </button></li>
  {/each}
</ul>
{#if rows.length > limit}
  <div bind:this={sentinel} class="more">
    <button class="link" onclick={() => (limit += PAGE)}>{t('r.more', { n: num(rows.length - limit) })}</button>
  </div>
{/if}

<script>
  // Middle pane, trend view: by year, the share of course-years in force (in
  // the filter) whose plan matches the query, out of those with a plan; one
  // line overall, per institution or per study type. Bars under it show the
  // coverage. Every point, bar and table cell lists its courses (`onpick`).
  import { share, trend } from '../data/trend.js'
  import { num, pct } from './format.js'
  import { local, t as tr } from './i18n.js'

  let { model, f, matches, queryOn, q, meta, by = $bindable(), onpick } = $props()

  const BYS = ['all', 'inst', 'st']
  const current = $derived(new Set(meta.institutions.filter((i) => i.plan_year_basis === 'current').map((i) => i.code)))
  const stName = $derived(new Map(meta.study_types.map((x) => [x.code, local(x, 'name') ?? x.name])))
  const t = $derived(trend(model, f, matches, by))
  const series = $derived(
    by === 'all'
      ? [{ key: null, label: tr('t.allLabel'), points: t.total }]
      : t.series.map((s) => ({
          ...s,
          label: s.other ? tr('t.others') : by === 'inst' ? s.key.toUpperCase() + (current.has(s.key) ? '*' : '') : s.key,
          title: s.other ? tr('t.othersTitle') : by === 'st' ? stName.get(s.key) : meta.institutions.find((i) => i.code === s.key)?.institution_name,
        })),
  )
  const value = (p) => (queryOn ? share(p.matching, p.withPlan) : share(p.withPlan, p.n))
  const colour = (s, i) => (s.other ? 'var(--c-other)' : by === 'all' ? 'var(--c1)' : `var(--c${i + 1})`)

  // geometry
  let width = $state(520)
  const H = 230
  const CH = $derived(queryOn ? 54 : 0) // coverage bars (without a query the lines are the coverage)
  const M = { l: 44, r: 12, t: 10, b: 22 }
  const bw = $derived((width - M.l - M.r) / t.years.length)
  const x = (i) => M.l + (i + 0.5) * bw
  const yMax = $derived(Math.max(0.005, ...series.flatMap((s) => s.points.map(value).filter((v) => v != null))))
  const STEPS = [0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.25]
  const step = $derived(STEPS.find((s) => yMax / s <= 5) ?? 0.25)
  const top = $derived(Math.min(1, Math.ceil(yMax / step - 1e-9) * step))
  const ticks = $derived(Array.from({ length: Math.round(top / step) + 1 }, (_, i) => i * step))
  const y = (v) => M.t + (1 - v / top) * (H - M.t - M.b)
  const tickLabel = (v) => `${(100 * v).toFixed(step < 0.01 ? 1 : 0)}%`
  const yearTicks = $derived(t.years.filter((yr, i) => i % Math.ceil(36 / bw) === 0))
  function path(points) {
    let d = ''
    let pen = false
    points.forEach((p, i) => {
      const v = value(p)
      if (v == null) return void (pen = false)
      d += `${pen ? 'L' : 'M'}${x(i).toFixed(1)},${y(v).toFixed(1)}`
      pen = true
    })
    return d
  }
  const barW = $derived(Math.max(2, Math.min(24, bw - 2)))
  const barH = (p) => (p.n ? (p.withPlan / p.n) * (CH - 14) : 0)

  let hover = $state(null) // index of the hovered year
  // beside the crosshair, on the side with more room
  const tipFlip = $derived(hover != null && x(hover) > width / 2)
  const tipLeft = $derived(hover == null ? 0 : x(hover) + (tipFlip ? -12 : 12))
  const year = (i) => t.years[i]
  const pickYear = (i) => onpick({ y0: year(i), y1: year(i) })
  const pickSeries = (s, i) =>
    onpick({ y0: year(i), y1: year(i), ...(s.key && !s.other ? { [by]: s.key } : {}) })
  const counts = (p) => (queryOn ? `${num(p.matching)} / ${num(p.withPlan)} / ${num(p.n)}` : `${num(p.withPlan)} / ${num(p.n)}`)
  const countsHead = $derived(queryOn ? tr('t.countsQ') : tr('t.counts'))
</script>

<div class="trend">
  <div class="trendbar">
    <span class="note">{tr('t.lines')}</span>
    {#each BYS as v (v)}
      <label><input type="radio" bind:group={by} value={v} /> {tr(`t.${v}`)}</label>
    {/each}
  </div>
  {#if queryOn && !matches}
    <p class="hint">{tr('t.wait')}</p>
  {:else if !t.total.some((p) => p.n)}
    <p class="hint">{tr('t.noYears')}</p>
  {:else}
    <h3>
      {#if queryOn}{tr('t.headQ')} <q>{q}</q>{:else}{tr('t.headCov')}{/if}
    </h3>
    <p class="note sub">
      {tr('t.sub')} {#if queryOn}{tr('t.subQ')}{:else}{tr('t.subNoQ')}{/if} {tr('t.click')}
    </p>
    {#if series.length > 1}
      <ul class="legend">
        {#each series as s, i (s.key)}
          <li title={s.title}><i style:background={colour(s, i)}></i>{s.label}</li>
        {/each}
      </ul>
    {/if}
    <div class="chart" bind:clientWidth={width} role="presentation" onpointerleave={() => (hover = null)}>
      <svg {width} height={H + CH} role="img" aria-label={tr('t.chart')}>
        {#each ticks as v (v)}
          <line class="grid" x1={M.l} x2={width - M.r} y1={y(v)} y2={y(v)} />
          <text class="axis" x={M.l - 6} y={y(v) + 4} text-anchor="end">{tickLabel(v)}</text>
        {/each}
        {#each yearTicks as yr (yr)}
          <text class="axis" x={x(yr - t.years[0])} y={H - 6} text-anchor="middle">{yr}</text>
        {/each}
        {#if hover != null}<line class="cross" x1={x(hover)} x2={x(hover)} y1={M.t} y2={H + CH - 2} />{/if}
        <!-- year bands: hover and click (the year alone) -->
        {#each t.years as yr, i (yr)}
          <rect
            class="band"
            x={M.l + i * bw}
            y={0}
            width={bw}
            height={H + CH}
            role="button"
            tabindex="-1"
            aria-label={tr('t.listYear', { y: yr })}
            onpointerenter={() => (hover = i)}
            onclick={() => pickYear(i)}
            onkeydown={(e) => e.key === 'Enter' && pickYear(i)}
          />
        {/each}
        {#each series as s, si (s.key)}
          <path class="line" d={path(s.points)} stroke={colour(s, si)} />
        {/each}
        {#each series as s, si (s.key)}
          {#each s.points as p, i (p.year)}
            {@const v = value(p)}
            {#if v != null}
              <circle class="dot" cx={x(i)} cy={y(v)} r={hover === i ? 5 : 4} fill={colour(s, si)} />
              <circle
                class="hit"
                cx={x(i)}
                cy={y(v)}
                r="9"
                role="button"
                tabindex="-1"
                aria-label="{s.label} {p.year}: {pct(queryOn ? p.matching : p.withPlan, queryOn ? p.withPlan : p.n)}"
                onpointerenter={() => (hover = i)}
                onclick={() => pickSeries(s, i)}
                onkeydown={(e) => e.key === 'Enter' && pickSeries(s, i)}
              />
            {/if}
          {/each}
        {/each}
        <!-- coverage -->
        {#if CH}<text class="axis" x={M.l - 6} y={H + 12} text-anchor="end">{tr('t.coverage')}</text>{/if}
        {#each t.total as p, i (p.year)}
          {#if p.n && CH}
            <rect class="track" x={x(i) - barW / 2} y={H + 2} width={barW} height={CH - 14} rx="2" />
            <rect class="cov" x={x(i) - barW / 2} y={H + 2 + CH - 14 - barH(p)} width={barW} height={barH(p)} rx="2" />
          {/if}
        {/each}
      </svg>
      {#if hover != null}
        {@const tot = t.total[hover]}
        <div class="tip" style:left="{tipLeft}px" style:transform={tipFlip ? 'translateX(-100%)' : null}>
          <strong>{year(hover)}</strong>
          <table>
            <thead><tr><th></th><th></th><th class="note">{countsHead}</th></tr></thead>
            <tbody>
              {#each series as s, si (s.key)}
                {@const p = s.points[hover]}
                <tr>
                  <td><i style:background={colour(s, si)}></i>{by === 'all' ? tr('t.allShort') : s.label}</td>
                  <td class="num">{pct(queryOn ? p.matching : p.withPlan, queryOn ? p.withPlan : p.n)}</td>
                  <td class="note">{counts(p)}</td>
                </tr>
              {/each}
            </tbody>
          </table>
          <div class="note">
            {tr('t.tip', { n: num(tot.n), w: num(tot.withPlan), c: pct(tot.withPlan, tot.n) })}{#if queryOn}{tr('t.tipMatch', { m: num(tot.matching) })}{/if}
          </div>
        </div>
      {/if}
    </div>
    {#if by === 'inst' && series.some((s) => current.has(s.key))}
      <p class="note sub">{tr('t.foot')}</p>
    {/if}
    <details class="ttable">
      <summary>{tr('t.table')}</summary>
      <table class="progs">
        <thead>
          <tr>
            <th>{tr('t.year')}</th>
            {#each series as s (s.key)}<th title={s.title}>{by === 'all' ? (queryOn ? tr('t.matching') : tr('t.withPlan')) : s.label}</th>{/each}
            <th>{tr('t.coverage')}</th>
          </tr>
        </thead>
        <tbody>
          {#each t.years as yr, i (yr)}
            {@const tot = t.total[i]}
            <tr>
              <td>{yr}</td>
              {#each series as s (s.key)}
                {@const p = s.points[i]}
                <td><button class="link" title="{countsHead}: {counts(p)}" onclick={() => pickSeries(s, i)}>{pct(queryOn ? p.matching : p.withPlan, queryOn ? p.withPlan : p.n)}</button></td>
              {/each}
              <td><button class="link" title={tr('t.of', { a: num(tot.withPlan), b: num(tot.n) })} onclick={() => pickYear(i)}>{pct(tot.withPlan, tot.n)}</button></td>
            </tr>
          {/each}
        </tbody>
      </table>
    </details>
  {/if}
</div>

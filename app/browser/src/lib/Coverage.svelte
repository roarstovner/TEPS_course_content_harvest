<script>
  // Middle pane, coverage view: per institution in the filter, the
  // course-years, those with a plan, how the plan years were assigned, and
  // the most common reasons for a missing plan; or the share of the plan
  // versions that have each section (what a section search can see). Counts
  // list their courses.
  import { coverage, sectionCoverage } from '../data/trend.js'
  import { num, pct } from './format.js'

  let { model, f = $bindable(), meta, planSections, onpick } = $props()

  const bySection = $derived(f.cov === 'sections')
  // once per change of the filters (not of the query), when shown
  const secs = $derived(bySection && planSections ? sectionCoverage(model, f, planSections) : null)
  // a section share: its courses (institution and the sections filter) in the list
  function pickSection(institution, code) {
    f.sec = [code]
    onpick({ inst: institution })
  }

  const inst = $derived(new Map(meta.institutions.map((i) => [i.code, i])))
  const rows = $derived(coverage(model, f))
  const total = $derived(rows.reduce((t, r) => ({ n: t.n + r.n, withPlan: t.withPlan + r.withPlan }), { n: 0, withPlan: 0 }))
  const BASIS = { url: 'URL year', current: 'current only*' }
</script>

<div class="trend">
  <h3>Coverage</h3>
  <p class="note sub">
    Course-years in force in the filter and how many have a plan. Click a count to list the courses.
    <label><input type="radio" bind:group={f.cov} value="plans" /> reasons for no plan</label>
    <label><input type="radio" bind:group={f.cov} value="sections" /> sections in the plans</label>
  </p>
  {#if bySection}
    <p class="note sub">
      Of the plan versions in force in the filter, the share that has each section. A section search
      ({#if f.in}<em>in {meta.sections.find((x) => x.code === f.in)?.label_en.toLowerCase()}</em> above, or a prefix such as
      <code>outcomes:</code>{:else}<em>in</em> above, or a prefix such as <code>outcomes:</code>{/if}) cannot find a plan
      without that section. Click a share to list those courses.{#if !planSections} Loading the sections…{/if}
    </p>
  {/if}
  <table class="progs cov">
    <thead>
      <tr>
        <th>Institution</th>
        <th class="num">Course-years</th>
        <th class="num">With a plan</th>
        <th class="num">Share</th>
        {#if bySection}
          <th class="num" title="Plan versions in force in the filter, each once">Plan versions</th>
          {#each meta.sections as x (x.code)}
            <th class="num" class:cur={f.in === x.code} title="{x.label_en} ({x.query}:)">{x.query}</th>
          {/each}
        {:else}
          <th title="How the years of a plan were found">Plan years</th>
          <th>Most common reasons for no plan</th>
        {/if}
      </tr>
    </thead>
    <tbody>
      {#each rows as r (r.institution)}
        {@const i = inst.get(r.institution)}
        <tr>
          <td title={i?.institution_name}>{r.institution.toUpperCase()}</td>
          <td class="num"><button class="link" onclick={() => onpick({ inst: r.institution })}>{num(r.n)}</button></td>
          <td class="num"><button class="link" onclick={() => onpick({ inst: r.institution })}>{num(r.withPlan)}</button></td>
          <td class="num">{pct(r.withPlan, r.n)}</td>
          {#if bySection}
            {@const sc = secs?.rows.get(r.institution)}
            <td class="num">{sc ? num(sc.plans) : ''}</td>
            {#each meta.sections as x (x.code)}
              {@const k = sc?.has.get(x.code) ?? 0}
              <td class="num" class:cur={f.in === x.code} class:low={sc?.plans && k < sc.plans / 2}>
                {#if sc?.plans}<button
                    class="link"
                    title="{num(k)} of {num(sc.plans)} plan versions have {x.label_en.toLowerCase()}"
                    onclick={() => pickSection(r.institution, x.code)}>{pct(k, sc.plans)}</button
                  >{/if}
              </td>
            {/each}
          {:else}
            <td class:flag={i?.plan_year_basis === 'current'}>{BASIS[i?.plan_year_basis] ?? i?.plan_year_basis ?? ''}</td>
            <td class="reasons">
              {#each r.reasons.slice(0, 3) as [reason, n], k (reason)}{k ? ' · ' : ''}<span
                  >{reason ?? 'not recorded'} <span class="note">{num(n)}</span></span
                >{/each}{#if r.reasons.length > 3}{' · …'}{/if}
            </td>
          {/if}
        </tr>
      {/each}
    </tbody>
    <tfoot>
      <tr>
        <th>All</th>
        <th class="num">{num(total.n)}</th>
        <th class="num">{num(total.withPlan)}</th>
        <th class="num">{pct(total.withPlan, total.n)}</th>
        {#if bySection}
          <th class="num">{secs ? num(secs.total.plans) : ''}</th>
          {#each meta.sections as x (x.code)}
            <th class="num" class:cur={f.in === x.code}
              >{secs ? pct(secs.total.has.get(x.code) ?? 0, secs.total.plans) : ''}</th
            >
          {/each}
        {:else}
          <th colspan="2"></th>
        {/if}
      </tr>
    </tfoot>
  </table>
  {#if !bySection && rows.some((r) => inst.get(r.institution)?.plan_year_basis === 'current')}
    <p class="note sub">* The site shows only the current plan; earlier years come from earlier harvests.</p>
  {/if}
</div>

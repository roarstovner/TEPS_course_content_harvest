<script>
  // Middle pane, coverage view: per institution in the filter, the
  // course-years, those with a plan, how the plan years were assigned, and
  // the most common reasons for a missing plan. Counts list their courses.
  import { coverage } from '../data/trend.js'
  import { num, pct } from './format.js'

  let { model, f, meta, onpick } = $props()

  const inst = $derived(new Map(meta.institutions.map((i) => [i.code, i])))
  const rows = $derived(coverage(model, f))
  const total = $derived(rows.reduce((t, r) => ({ n: t.n + r.n, withPlan: t.withPlan + r.withPlan }), { n: 0, withPlan: 0 }))
  const BASIS = { url: 'URL year', current: 'current only*' }
</script>

<div class="trend">
  <h3>Coverage</h3>
  <p class="note sub">Course-years in force in the filter and how many have a plan. Click a count to list the courses.</p>
  <table class="progs cov">
    <thead>
      <tr>
        <th>Institution</th>
        <th class="num">Course-years</th>
        <th class="num">With a plan</th>
        <th class="num">Share</th>
        <th title="How the years of a plan were found">Plan years</th>
        <th>Most common reasons for no plan</th>
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
          <td class:flag={i?.plan_year_basis === 'current'}>{BASIS[i?.plan_year_basis] ?? i?.plan_year_basis ?? ''}</td>
          <td class="reasons">
            {#each r.reasons.slice(0, 3) as [reason, n], k (reason)}{k ? ' · ' : ''}<span
                >{reason ?? 'not recorded'} <span class="note">{num(n)}</span></span
              >{/each}{#if r.reasons.length > 3}{' · …'}{/if}
          </td>
        </tr>
      {/each}
    </tbody>
    <tfoot>
      <tr>
        <th>All</th>
        <th class="num">{num(total.n)}</th>
        <th class="num">{num(total.withPlan)}</th>
        <th class="num">{pct(total.withPlan, total.n)}</th>
        <th colspan="2"></th>
      </tr>
    </tfoot>
  </table>
  {#if rows.some((r) => inst.get(r.institution)?.plan_year_basis === 'current')}
    <p class="note sub">* The site shows only the current plan; earlier years come from earlier harvests.</p>
  {/if}
</div>

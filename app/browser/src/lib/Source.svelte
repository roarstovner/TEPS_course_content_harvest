<script>
  // Where a plan version comes from and who uses it: its URL and harvest
  // date, the years and programmes that use it, and the other courses of the
  // institution with the same text.
  import { planUse, sameText, yearRanges } from '../data/related.js'
  import { local, t } from './i18n.js'

  let { course, version, model, studyTypes, onopen } = $props()

  const use = $derived(planUse(course, version.key, model.programmes))
  const same = $derived(sameText(model, version))
  const typeName = $derived(new Map(studyTypes.map((x) => [x.code, local(x, 'name') ?? x.name])))
  const day = (d) => (d instanceof Date ? d.toISOString().slice(0, 10) : d)
</script>

<dl class="source">
  <dt>{t('src.source')}</dt>
  <dd><a href={version.source_url} target="_blank" rel="noopener noreferrer">{version.source_url}</a></dd>
  <dt>{t('src.harvested')}</dt>
  <dd>{version.harvested_at ? day(version.harvested_at) : t('src.notRecorded')}</dd>
  <dt>{t('src.inForce')}</dt>
  <dd>
    {version.year_from === version.year_to ? version.year_from : `${version.year_from}–${version.year_to}`}
    <small>{t('src.years', { y: yearRanges(use.years) || t('src.none') })}</small>
  </dd>
  {#if same.length}
    <dt>{t('src.same')}</dt>
    <dd>
      {#each same as r (r.course.key + r.version.key)}
        <button class="link same" onclick={() => onopen(r.course, r.version)} title={r.course.course_name}
          >{t('src.sameAs', { c: r.course.course_code })}</button
        >
        <small>{r.course.course_name}</small><br />
      {/each}
    </dd>
  {/if}
</dl>

<h3>{t('src.progs')}</h3>
{#if use.programmes.length}
  <table class="progs">
    <thead><tr><th>{t('src.prog')}</th><th>{t('src.type')}</th><th>{t('src.yearsHead')}</th></tr></thead>
    <tbody>
      {#each use.programmes as p (p.programme_code)}
        <tr>
          <td>{p.programme_name} <small>{p.programme_code}</small></td>
          <td title={p.study_type}>{typeName.get(p.study_type) ?? p.study_type ?? '–'}</td>
          <td>{yearRanges(p.years)}</td>
        </tr>
      {/each}
    </tbody>
  </table>
{:else}
  <p class="note">{t('src.noProg')}</p>
{/if}

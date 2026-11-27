<script>
  // Left pane: years, study type, institution, subject, NUS field, level,
  // credits, language, sections; each count ignores its own filter (model.js
  // facets).
  import StudyTypes from './StudyTypes.svelte'
  import CheckList from './CheckList.svelte'
  import { filterValues, valueLabel } from '../data/filters.js'
  import { getLang, local, NB_VALUES, other, t } from './i18n.js'

  let { state = $bindable(), meta, model, facets, sectionsReady } = $props()

  // value names in the UI language (filters.js has the English ones)
  const label = (key, v) => (getLang() === 'nb' ? (v === '' ? t('f.notGiven') : (NB_VALUES[key]?.[v] ?? valueLabel(key, v))) : valueLabel(key, v))
  const values = (key) => filterValues(model.courses, key).map((v) => ({ value: v, label: label(key, v), title: v || undefined }))
  const institutions = $derived(meta.institutions.map((i) => ({ value: i.code, label: i.code.toUpperCase(), title: i.institution_name })))
  // a subject name with its DBH subject codes (Fagkode; codes are per
  // institution, so several share a name)
  const subjectCodes = $derived.by(() => {
    const m = new Map()
    for (const c of model.courses) if (c.subject_code) (m.get(c.subject_name ?? '') ?? m.set(c.subject_name ?? '', new Set()).get(c.subject_name ?? '')).add(c.subject_code)
    return m
  })
  const subjects = $derived(values('subj').map((i) => ({ ...i, title: [i.label, [...(subjectCodes.get(i.value) ?? [])].sort().join(', ')].filter(Boolean).join(': ') })))
  // NUS subject groups under their field (meta.nus_fields), in code order
  const nusGroup = $derived(new Map((meta.nus_fields ?? []).map((g) => [g.code, g])))
  const nus = $derived(
    filterValues(model.courses, 'nus')
      .sort((a, b) => (!a) - (!b) || a.localeCompare(b))
      .map((v) => {
        const g = nusGroup.get(v)
        if (!g) return { value: v, label: v ? v : label('nus', v), title: v || undefined }
        return { value: v, label: local(g, 'name'), title: `${other(g, 'name')} (NUS x${v}xxx)`, group: g.field, groupLabel: local(g, 'field_name'), groupTitle: `${other(g, 'field_name')} (NUS x${g.field}xxxx)` }
      }),
  )
  const levels = $derived(values('lvl'))
  const credits = $derived(values('cr'))
  const languages = $derived(values('lang'))
  const sections = $derived(meta.sections.map((x) => ({ value: x.code, label: local(x, 'label'), title: `${other(x, 'label')} (${x.query}:)` })))

  const years = $derived(Array.from({ length: model.yearMax - model.yearMin + 1 }, (_, i) => model.yearMin + i))
  const setYear = (which, v) => {
    const y = Number(v)
    state[which] = y === (which === 'y0' ? model.yearFrom : model.yearMax) ? null : y
  }
</script>

<fieldset>
  <legend>{t('f.years')}</legend>
  <div class="years">
    <select aria-label={t('f.from')} value={state.y0 ?? model.yearFrom} onchange={(e) => setYear('y0', e.currentTarget.value)}>
      {#each years as y (y)}<option value={y} disabled={y > (state.y1 ?? model.yearMax)}>{y}</option>{/each}
    </select>
    –
    <select aria-label={t('f.to')} value={state.y1 ?? model.yearMax} onchange={(e) => setYear('y1', e.currentTarget.value)}>
      {#each years as y (y)}<option value={y} disabled={y < (state.y0 ?? model.yearFrom)}>{y}</option>{/each}
    </select>
  </div>
</fieldset>

<StudyTypes types={meta.study_types} bind:selected={state.st} counts={facets.st} uncovered={meta.uncovered} />
<CheckList title={t('f.inst')} items={institutions} bind:selected={state.inst} counts={facets.inst} open />
<CheckList title={t('f.subj')} items={subjects} bind:selected={state.subj} counts={facets.subj} />
<CheckList title={t('f.nus')} items={nus} bind:selected={state.nus} counts={facets.nus} />
<CheckList title={t('f.lvl')} items={levels} bind:selected={state.lvl} counts={facets.lvl} />
<CheckList title={t('f.cr')} items={credits} bind:selected={state.cr} counts={facets.cr} />
<CheckList title={t('f.lang')} items={languages} bind:selected={state.lang} counts={facets.lang} />
<CheckList
  title={t('f.sec')}
  note={sectionsReady ? t('f.secNote') : t('f.loading')}
  items={sections}
  bind:selected={state.sec}
  counts={sectionsReady ? facets.sec : null}
/>

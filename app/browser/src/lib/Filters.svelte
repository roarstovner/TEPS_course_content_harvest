<script>
  // Left pane: years, study type, institution, subject, level, credits,
  // language, sections; each count ignores its own filter (model.js facets).
  import StudyTypes from './StudyTypes.svelte'
  import CheckList from './CheckList.svelte'
  import { filterValues, valueLabel } from '../data/filters.js'

  let { state = $bindable(), meta, model, facets, sectionsReady } = $props()

  const values = (key) => filterValues(model.courses, key).map((v) => ({ value: v, label: valueLabel(key, v), title: v || undefined }))
  const institutions = $derived(meta.institutions.map((i) => ({ value: i.code, label: i.code.toUpperCase(), title: i.institution_name })))
  const subjects = $derived(values('subj'))
  const levels = $derived(values('lvl'))
  const credits = $derived(values('cr'))
  const languages = $derived(values('lang'))
  const sections = $derived(meta.sections.map((x) => ({ value: x.code, label: x.label_en, title: `${x.label_no} (${x.query}:)` })))

  const years = $derived(Array.from({ length: model.yearMax - model.yearMin + 1 }, (_, i) => model.yearMin + i))
  const setYear = (which, v) => {
    const y = Number(v)
    state[which] = y === (which === 'y0' ? model.yearFrom : model.yearMax) ? null : y
  }
</script>

<fieldset>
  <legend>Years</legend>
  <div class="years">
    <select aria-label="From year" value={state.y0 ?? model.yearFrom} onchange={(e) => setYear('y0', e.currentTarget.value)}>
      {#each years as y (y)}<option value={y} disabled={y > (state.y1 ?? model.yearMax)}>{y}</option>{/each}
    </select>
    –
    <select aria-label="To year" value={state.y1 ?? model.yearMax} onchange={(e) => setYear('y1', e.currentTarget.value)}>
      {#each years as y (y)}<option value={y} disabled={y < (state.y0 ?? model.yearFrom)}>{y}</option>{/each}
    </select>
  </div>
</fieldset>

<StudyTypes types={meta.study_types} bind:selected={state.st} counts={facets.st} />
<CheckList title="Institution" items={institutions} bind:selected={state.inst} counts={facets.inst} open />
<CheckList title="Subject" items={subjects} bind:selected={state.subj} counts={facets.subj} />
<CheckList title="Level" items={levels} bind:selected={state.lvl} counts={facets.lvl} />
<CheckList title="Credits" items={credits} bind:selected={state.cr} counts={facets.cr} />
<CheckList title="Language" items={languages} bind:selected={state.lang} counts={facets.lang} />
<CheckList
  title="Sections"
  note={sectionsReady ? 'a plan has all ticked' : 'loading…'}
  items={sections}
  bind:selected={state.sec}
  counts={sectionsReady ? facets.sec : null}
/>

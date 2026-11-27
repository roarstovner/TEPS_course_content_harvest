// The UI strings in English (en) and Norwegian Bokmål (nb), and the active
// language. The course plans themselves are not translated. `t(key, params)`
// reads the language at render time: App.svelte re-renders the page under a
// {#key} block when it changes, so plain module state suffices here.
import { setNumberLocale } from './format.js'

export const LANGS = ['en', 'nb']
const KEY = 'ui-lang'
let lang = 'en'

/** The language at start: the one chosen before, else the browser's
 *  (nb, nn or no: nb), else English. */
export function initialLang() {
  try {
    const l = localStorage.getItem(KEY)
    if (LANGS.includes(l)) return l
  } catch {
    // storage blocked: the browser's language
  }
  const nav = (typeof navigator !== 'undefined' && (navigator.languages?.[0] ?? navigator.language)) || ''
  return /^(nb|nn|no)\b/i.test(nav) ? 'nb' : 'en'
}

/** Make `l` the active language: strings, numbers, <html lang>, the title;
 *  remembered in the browser when `remember` is set. */
export function setLang(l, remember = false) {
  lang = LANGS.includes(l) ? l : 'en'
  setNumberLocale(lang)
  if (typeof document !== 'undefined') {
    document.documentElement.lang = lang
    document.title = t('app.title')
  }
  if (remember) {
    try {
      localStorage.setItem(KEY, lang)
    } catch {
      // storage blocked: the choice lasts until the page is left
    }
  }
}
export const getLang = () => lang

/** A UI string with `{name}` replaced from `params`; the key when missing. */
export function t(key, params) {
  const e = D[key]
  const s = e ? (e[lang === 'nb' ? 1 : 0] ?? e[0]) : key
  return params ? s.replace(/\{(\w+)\}/g, (m, k) => (k in params ? String(params[k]) : m)) : s
}

/** A field of the data in the active language: `label_no` or `label_en`
 *  (meta.json's sections, study types and groups); `other` the other one,
 *  for hover titles. */
export const local = (o, field) => (lang === 'nb' ? (o[`${field}_no`] ?? o[`${field}_en`]) : (o[`${field}_en`] ?? o[`${field}_no`]))
export const other = (o, field) => (lang === 'nb' ? o[`${field}_en`] : o[`${field}_no`])

/** The Norwegian names of filter values (data/filters.js has the English). */
export const NB_VALUES = {
  lvl: { LN: 'Lavere grad', HN: 'Høyere grad', AR: 'Årsstudium', VS: 'Videregående', 50: 'Eldre kode (50)' },
  lang: {
    NOR: 'Norsk', NOB: 'Norsk (bokmål)', NNO: 'Norsk (nynorsk)', ENG: 'Engelsk', SME: 'Nordsamisk', SMA: 'Sørsamisk',
    SMJ: 'Lulesamisk', SWE: 'Svensk', DAN: 'Dansk', FIN: 'Finsk', FRE: 'Fransk', GER: 'Tysk', SPA: 'Spansk',
    ITA: 'Italiensk', RUS: 'Russisk',
  },
}

/** The keyboard shortcuts, for the overlay: [keys, dictionary key]. */
export const SHORTCUTS = {
  general: [
    [['/'], 'key.search'],
    [['j', 'k'], 'key.move'],
    [['↓', '↑'], 'key.arrows'],
    [['Enter'], 'key.open'],
    [['Esc'], 'key.esc'],
    [['?'], 'key.help'],
  ],
  screen: [
    [['j', 'k'], 'key.hit'],
    [['i', 'e', 'm'], 'key.tag'],
    [['n'], 'key.note'],
    [['o'], 'key.openplan'],
  ],
}

// key: [English, Norwegian Bokmål]
const D = {
  'app.title': ['Course Plan Browser', 'Emneplanleser'],
  'app.skip': ['Skip to the results', 'Hopp til resultatene'],
  'app.views': ['View', 'Visning'],
  'app.loading': ['Loading courses…', 'Laster emner…'],
  'app.hint': ['Select a course to read its plan. Press ? for the keyboard shortcuts.', 'Velg et emne for å lese planen. Trykk ? for hurtigtastene.'],
  'view.list': ['List', 'Liste'],
  'view.trend': ['Trend', 'Trend'],
  'view.coverage': ['Coverage', 'Dekning'],
  'view.screen': ['Screen', 'Gjennomgang'],
  'view.compare': ['Compare', 'Sammenlign'],
  'view.terms': ['Term lists', 'Termlister'],

  // header
  'h.search': ['Search', 'Søk'],
  'h.placeholder': ['Search the plans, e.g. fremmedspråk* morsmål*  (press / to focus)', 'Søk i planene, f.eks. fremmedspråk* morsmål*  (trykk / for å gå hit)'],
  'h.scope': ['all terms in the same', 'alle termer i samme'],
  'h.scopeAria': ['Scope of AND', 'Rekkevidde for AND'],
  'scope.plan': ['plan', 'plan'],
  'scope.section': ['section', 'seksjon'],
  'scope.paragraph': ['line', 'linje'],
  'scope.sentence': ['sentence', 'setning'],
  'scope.words': ['within N words', 'innenfor N ord'],
  'h.in': ['in', 'i'],
  'h.inAria': ['Search in', 'Søk i'],
  'h.inTitle': [
    'Search the whole plan or one section: a term without a section prefix searches the section chosen here; a term with its own prefix (content:urfolk) keeps it. The same as putting the section\'s prefix on every term.',
    'Søk i hele planen eller i én seksjon: en term uten seksjonsprefiks søker i seksjonen som er valgt her; en term med eget prefiks (content:urfolk) beholder det. Det samme som å sette seksjonens prefiks på hver term.',
  ],
  'h.full': ['full plan', 'hele planen'],
  'h.download': ['Download', 'Last ned'],
  'h.downloadTitle': ['Download the selection', 'Last ned utvalget'],
  'h.help': ['Help', 'Hjelp'],
  'h.helpTitle': ['User guide', 'Brukerveiledning'],
  'h.keys': ['Keys', 'Taster'],
  'h.keysTitle': ['Keyboard shortcuts (?)', 'Hurtigtaster (?)'],
  'h.lang': ['Norsk', 'English'],
  'h.langTitle': ['Bytt språk til norsk', 'Switch the language to English'],
  'c.courses': ['courses', 'emner'],
  'c.versions': ['plan versions', 'planversjoner'],
  'c.match': ['{m} of {w} course-years with a plan match ({p})', '{m} av {w} emneår med plan treffer ({p})'],
  'c.latest': ['{p} of course-years in force in {y} (coverage {c})', '{p} av emneårene i {y} (dekning {c})'],
  'c.coverage': ['coverage {p} of {n} course-years', 'dekning {p} av {n} emneår'],
  'c.waiting': ['search runs when the plan text has loaded', 'søket kjøres når planteksten er lastet'],
  'c.searching': ['searching…', 'søker…'],
  'c.loading': ['loading {f} {a} / {b}', 'laster {f} {a} / {b}'],
  'c.preparing': ['preparing text…', 'klargjør teksten…'],
  'c.sections': ['loading sections…', 'laster seksjonene…'],
  'wf.none': ['no match', 'ingen treff'],
  'wf.title': ['{c} matches in {p} plans', '{c} treff i {p} planer'],
  'wf.fewer': ['fewer', 'færre'],
  'wf.more': ['+{n} more', '+{n} til'],
  'wf.label': ['Word forms of {t}: untick one to leave it out', 'Ordformer av {t}: fjern krysset for å utelate en'],

  // results
  'r.label': ['Courses', 'Emner'],
  'r.none': ['No course matches.', 'Ingen emner treffer.'],
  'r.hits': ['{n} hits', '{n} treff'],
  'r.versions': ['{n} versions', '{n} versjoner'],
  'r.noPlan': ['{y}: taught, no plan ({r})', '{y}: undervist, ingen plan ({r})'],
  'r.noReason': ['reason not recorded', 'årsak ikke registrert'],
  'r.more': ['Show more ({n} left)', 'Vis flere ({n} igjen)'],

  // filters
  'f.label': ['Filters', 'Filtre'],
  'f.years': ['Years', 'År'],
  'f.from': ['From year', 'Fra år'],
  'f.to': ['To year', 'Til år'],
  'f.inst': ['Institution', 'Institusjon'],
  'f.subj': ['Subject', 'Fag'],
  'f.lvl': ['Level', 'Nivå'],
  'f.cr': ['Credits', 'Studiepoeng'],
  'f.lang': ['Language', 'Språk'],
  'f.sec': ['Sections', 'Seksjoner'],
  'f.secNote': ['a plan has all ticked', 'planen har alle avkryssede'],
  'f.loading': ['loading…', 'laster…'],
  'f.notGiven': ['(not given)', '(ikke oppgitt)'],
  'f.clear': ['Clear', 'Nullstill'],
  'f.filterValues': ['Filter {n} values', 'Filtrer {n} verdier'],
  'f.filterAria': ['Filter {t}', 'Filtrer {t}'],
  'st.title': ['Study type', 'Studietype'],
  'st.te': ['Teacher education (TE frame)', 'Lærerutdanning (LU-rammen)'],
  'st.filter': ['Filter {n} study types', 'Filtrer {n} studietyper'],
  'st.filterAria': ['Filter study types', 'Filtrer studietyper'],
  'st.collapse': ['Collapse {g}', 'Lukk {g}'],
  'st.expand': ['Expand {g}', 'Åpne {g}'],
  'st.count': [
    '{c} courses match the query and the other filters; coverage: {w} of {n} course-years have a plan',
    '{c} emner treffer søket og de andre filtrene; dekning: {w} av {n} emneår har plan',
  ],
  'st.absent': ['{n} not in this data', '{n} ikke i disse dataene'],
  'st.absentTitle': ['{name} ({code}): not in this data; the all-courses data adds it', '{name} ({code}): ikke i disse dataene; dataene med alle emner tar den med'],
  'st.none': ['No study type matches.', 'Ingen studietype treffer.'],
  'st.uncAria': ['Institutions not harvested', 'Institusjoner som ikke er høstet'],
  'st.unc': [
    'Not harvested: institutions with courses of the ticked study types in DBH, with their courses in {y}',
    'Ikke høstet: institusjoner med emner av de avkryssede studietypene i DBH, med emnene deres i {y}',
  ],
  'st.uncInst': ['{name} ({n} courses)', '{name} ({n} emner)'],
  'st.uncNone': ['none with courses in {y}', 'ingen med emner i {y}'],
  'st.uncFormer': ['{n} former, without courses in {y}', '{n} tidligere, uten emner i {y}'],
  'st.uncFormerInst': ['{name}: {n} courses, last in {last}', '{name}: {n} emner, sist i {last}'],
  'st.uncAll': [
    'Every institution with courses of the ticked study types in DBH is harvested.',
    'Alle institusjoner med emner av de avkryssede studietypene i DBH er høstet.',
  ],

  // help
  'help.toc': ['Guide contents', 'Innhold i veiledningen'],
  'help.enOnly': ['', 'Brukerveiledningen finnes foreløpig bare på engelsk.'],

  // course pane
  'cv.back': ['← list', '← liste'],
  'cv.backTitle': ['Back to the list (Esc)', 'Tilbake til listen (Esc)'],
  'cv.credits': ['{n} credits', '{n} studiepoeng'],
  'cv.level': ['level {l}', 'nivå {l}'],
  'cv.taught': ['taught {a}–{b}', 'undervist {a}–{b}'],
  'cv.versions': ['Plan versions', 'Planversjoner'],
  'cv.tabs': ['Plan pane', 'Planpanel'],
  'cv.plan': ['Plan', 'Plan'],
  'cv.sections': ['Sections', 'Seksjoner'],
  'cv.versionsTab': ['Versions', 'Versjoner'],
  'cv.source': ['Source', 'Kilde'],
  'cv.compare': ['compare', 'sammenlign'],
  'cv.compareTitle': ['Show this plan in the compare view (side A, or B when A holds another plan)', 'Vis planen i sammenligningsvisningen (side A, eller B når A har en annen plan)'],
  'cv.sourceLink': ['source ↗', 'kilde ↗'],
  'cv.newTab': ['(opens in a new tab)', '(åpnes i ny fane)'],
  'cv.noPlan': ['No plan was found for this course.', 'Det ble ikke funnet noen plan for dette emnet.'],
  'cv.loadingText': ['Loading the plan text…', 'Laster planteksten…'],
  'cv.loadingSections': ['Loading the sections…', 'Laster seksjonene…'],
  'cv.notIn': ['not in this plan', 'ikke i denne planen'],
  'pt.legend': ['Sections in the margin', 'Seksjoner i margen'],
  'pt.toggle': ['sections in the margin', 'seksjoner i margen'],
  'pt.notIn': ['{s}: not in this plan', '{s}: ikke i denne planen'],
  'pt.none': ['in no section', 'i ingen seksjon'],
  'v.one': ['Only one version of this plan was harvested.', 'Bare én versjon av denne planen ble høstet.'],
  'v.compare': ['Compare', 'Sammenlign'],
  'v.with': ['with the shown', 'med den viste'],
  'v.words': ['words from {a} to {b}', 'ord fra {a} til {b}'],
  'v.comparing': ['Comparing…', 'Sammenligner…'],
  'v.spacing': ['The two texts differ only in spacing.', 'De to tekstene skiller seg bare i mellomrom.'],
  'src.source': ['Source', 'Kilde'],
  'src.harvested': ['Harvested', 'Høstet'],
  'src.notRecorded': ['not recorded', 'ikke registrert'],
  'src.inForce': ['In force', 'Gjelder'],
  'src.years': ["(the plan's years); course-years using it: {y}", '(planens år); emneår som bruker den: {y}'],
  'src.none': ['none', 'ingen'],
  'src.same': ['Same text', 'Samme tekst'],
  'src.sameAs': ['same text as {c}', 'samme tekst som {c}'],
  'src.progs': ['Programmes using the plan', 'Studieprogrammer som bruker planen'],
  'src.prog': ['Programme', 'Studieprogram'],
  'src.type': ['Study type', 'Studietype'],
  'src.yearsHead': ['Years', 'År'],
  'src.noProg': ['No programme in the frame lists the course in the years of this plan.', 'Ingen studieprogrammer i rammen har emnet i årene til denne planen.'],

  // coverage
  'cov.title': ['Coverage', 'Dekning'],
  'cov.intro': ['Course-years in force in the filter and how many have a plan. Click a count to list the courses.', 'Emneår i filteret og hvor mange som har plan. Klikk et tall for å liste emnene.'],
  'cov.reasons': ['reasons for no plan', 'årsaker til manglende plan'],
  'cov.bySection': ['sections in the plans', 'seksjoner i planene'],
  'cov.sec1': ['Of the plan versions in force in the filter, the share that has each section. A section search', 'Av planversjonene i filteret, andelen som har hver seksjon. Et seksjonssøk'],
  'cov.in': ['in', 'i'],
  'cov.above': ['above', 'over'],
  'cov.orPrefix': ['or a prefix such as', 'eller et prefiks som'],
  'cov.sec2': ['cannot find a plan without that section. Click a share to list those courses.', 'finner ikke en plan uten den seksjonen. Klikk en andel for å liste emnene.'],
  'cov.inst': ['Institution', 'Institusjon'],
  'cov.cy': ['Course-years', 'Emneår'],
  'cov.withPlan': ['With a plan', 'Med plan'],
  'cov.share': ['Share', 'Andel'],
  'cov.versions': ['Plan versions', 'Planversjoner'],
  'cov.versionsTitle': ['Plan versions in force in the filter, each once', 'Planversjoner i filteret, hver én gang'],
  'cov.planYears': ['Plan years', 'Planår'],
  'cov.planYearsTitle': ['How the years of a plan were found', 'Hvordan årene til en plan ble funnet'],
  'cov.common': ['Most common reasons for no plan', 'Vanligste årsaker til manglende plan'],
  'cov.cellTitle': ['{k} of {n} plan versions have {s}', '{k} av {n} planversjoner har {s}'],
  'cov.url': ['URL year', 'år i URL'],
  'cov.current': ['current only*', 'bare gjeldende*'],
  'cov.all': ['All', 'Alle'],
  'cov.foot': ['* The site shows only the current plan; earlier years come from earlier harvests.', '* Nettstedet viser bare gjeldende plan; tidligere år kommer fra tidligere høstinger.'],

  // trend
  't.lines': ['Lines', 'Linjer'],
  't.all': ['overall', 'samlet'],
  't.inst': ['per institution', 'per institusjon'],
  't.st': ['per study type', 'per studietype'],
  't.allLabel': ['All course-years in the filter', 'Alle emneår i filteret'],
  't.others': ['others', 'andre'],
  't.othersTitle': ['the course-years of every other group', 'emneårene i alle andre grupper'],
  't.wait': ['The trend shows when the search has run.', 'Trenden vises når søket har kjørt.'],
  't.noYears': ['No course-years in the filter.', 'Ingen emneår i filteret.'],
  't.headQ': ['Share of course-years with a plan whose plan matches', 'Andel emneår med plan der planen treffer'],
  't.headCov': ['Coverage: share of course-years with a plan', 'Dekning: andel emneår med plan'],
  't.sub': ['Course-years in force in the filter, by year.', 'Emneår i filteret, per år.'],
  't.subQ': [
    'A course-year matches when any of its plans matches; the bars below are the coverage, the share of course-years that have a plan, so missing plans are not read as absence.',
    'Et emneår treffer når en av planene treffer; søylene under er dekningen, andelen emneår som har plan, slik at manglende planer ikke leses som fravær.',
  ],
  't.subNoQ': ['Type a query to see the share of plans that match it.', 'Skriv et søk for å se andelen planer som treffer.'],
  't.click': ['Click a point or bar to list its courses.', 'Klikk et punkt eller en søyle for å liste emnene.'],
  't.chart': ['Trend chart; the table below has the numbers', 'Trenddiagram; tabellen under har tallene'],
  't.listYear': ['{y}: list the courses', '{y}: list emnene'],
  't.coverage': ['coverage', 'dekning'],
  't.countsQ': ['matching / with a plan / all', 'treff / med plan / alle'],
  't.counts': ['with a plan / all', 'med plan / alle'],
  't.allShort': ['all', 'alle'],
  't.tip': ['{n} course-years, {w} with a plan (coverage {c})', '{n} emneår, {w} med plan (dekning {c})'],
  't.tipMatch': [', {m} matching', ', {m} treff'],
  't.table': ['Table', 'Tabell'],
  't.year': ['Year', 'År'],
  't.matching': ['matching', 'treff'],
  't.withPlan': ['with a plan', 'med plan'],
  't.of': ['{a} of {b}', '{a} av {b}'],
  't.foot': ['* The site shows only the current plan; earlier years come from earlier harvests.', '* Nettstedet viser bare gjeldende plan; tidligere år kommer fra tidligere høstinger.'],

  // download
  'dl.title': ['Download the selection', 'Last ned utvalget'],
  'dl.what': ['What the list shows: {c} courses, {p} plan versions', 'Det listen viser: {c} emner, {p} planversjoner'],
  'dl.matching': [', matching', ', som treffer'],
  'dl.filters': [', with the filters', ', med filtrene'],
  'dl.excl': [' and unticked word forms', ' og utelatte ordformer'],
  'dl.unit': ['One row per', 'Én rad per'],
  'dl.format': ['Format', 'Format'],
  'unit.course': ['course', 'emne'],
  'unit.plan': ['plan version', 'planversjon'],
  'unit.course_year': ['course-year', 'emneår'],
  'unit.section': ['section', 'seksjon'],
  'unit.hit': ['hit (keyword in context)', 'treff (nøkkelord i kontekst)'],
  'fmt.qda': ['QDA: text files + classification sheet (NVivo, MAXQDA, ATLAS.ti)', 'QDA: tekstfiler + klassifiseringsark (NVivo, MAXQDA, ATLAS.ti)'],
  'dl.needsQ': [' (needs a query)', ' (krever et søk)'],
  'dl.rows': ['rows', 'rader'],
  'dl.counting': ['counting…', 'teller…'],
  'dl.excel1': ['Excel holds at most {n} characters in a cell: longer texts are cut, and the column', 'Excel tar høyst {n} tegn i en celle: lengre tekster kuttes, og kolonnen'],
  'dl.excel2': ['flags the row', 'markerer raden'],
  'dl.excelHere': ['({n} here)', '({n} her)'],
  'dl.excel3': ['. CSV and Parquet keep the whole text.', '. CSV og Parquet beholder hele teksten.'],
  'dl.zipQda': ['A ZIP with the text files and the classification sheet,', 'En ZIP med tekstfilene og klassifiseringsarket,'],
  'dl.zipTable': ['A ZIP with the table,', 'En ZIP med tabellen,'],
  'dl.zip2': ['(query, filters, data version, counts, the link to this view),', '(søk, filtre, dataversjon, antall, lenken til denne visningen),'],
  'dl.zip3': [', the keys of the rows and', ', nøklene til radene og'],
  'dl.zip4': [', which take the same rows from the published release.', ', som henter de samme radene fra den publiserte utgaven.'],
  'dl.cancel': ['Cancel', 'Avbryt'],
  'dl.building': ['Building…', 'Bygger…'],
  'dl.download': ['Download', 'Last ned'],

  // screening
  'sc.title': ['Screening', 'Gjennomgang'],
  'sc.needQ': ['Screening needs a query: it draws a random sample of its hits (keyword in context).', 'Gjennomgangen krever et søk: den trekker et tilfeldig utvalg av treffene (nøkkelord i kontekst).'],
  'sc.sample': ['Sample', 'Utvalg'],
  'sc.hits': ['hits', 'treff'],
  'sc.seed': ['seed', 'frø'],
  'sc.new': ['new sample', 'nytt utvalg'],
  'sc.of': ['{a} of {b} hits', '{a} av {b} treff'],
  'sc.drawing': ['drawing…', 'trekker…'],
  'sc.export': ['Export CSV', 'Eksporter CSV'],
  'sc.note': ['The sample and the seed are in the link. Tags are kept in this browser for this query and data frame', 'Utvalget og frøet står i lenken. Merkingene lagres i denne nettleseren for dette søket og denne datarammen'],
  'sc.notStored': [' — this browser does not store them: export before leaving', ' — denne nettleseren lagrer dem ikke: eksporter før du går'],
  'sc.keys': ['Keys: j/k next, previous, i include, e exclude, m maybe, n note, o open the plan.', 'Taster: j/k neste, forrige, i ta med, e utelat, m kanskje, n notat, o åpne planen.'],
  'sc.untagged': ['untagged', 'umerket'],
  'sc.precision': ['precision', 'presisjon'],
  'sc.precisionNote': ['(include of include + exclude)', '(ta med av ta med + utelat)'],
  'tag.include': ['include', 'ta med'],
  'tag.exclude': ['exclude', 'utelat'],
  'tag.maybe': ['maybe', 'kanskje'],
  'sc.open': ['open the plan', 'åpne planen'],
  'sc.notePh': ['Note', 'Notat'],
  'sc.noHits': ['No hits in the selection.', 'Ingen treff i utvalget.'],
  'sc.list': ['Hits in the sample', 'Treff i utvalget'],

  // compare
  'cmp.pair': ['two plans', 'to planer'],
  'cmp.across': ['one year across institutions', 'ett år på tvers av institusjoner'],
  'cmp.text': ['Text', 'Tekst'],
  'cmp.whole': ['whole plan', 'hele planen'],
  'cmp.diff': ['mark differences', 'marker forskjeller'],
  'cmp.swap': ['swap A and B', 'bytt A og B'],
  'cmp.onlyA': ['words only in A', 'ord bare i A'],
  'cmp.onlyB': ['only in B', 'bare i B'],
  'cmp.year': ['Year', 'År'],
  'cmp.loading': ['loading…', 'laster…'],
  'cmp.intro1': ['Any two plan versions: pick a course on each side, or use', 'To vilkårlige planversjoner: velg et emne på hver side, eller bruk'],
  'cmp.intro2': [
    "in a course's plan pane. B is by default the version before A. Words only in A are struck through, words only in B underlined; matches of the query stay marked.",
    'i planpanelet til et emne. B er som standard versjonen før A. Ord bare i A er gjennomstreket, ord bare i B understreket; treff på søket forblir markert.',
  ],
  'cmp.another': ['another course…', 'et annet emne…'],
  'cmp.find': ['find a course: institution, code or name', 'finn et emne: institusjon, kode eller navn'],
  'cmp.pickAria': ['Course for side {k}', 'Emne for side {k}'],
  'cmp.version': ['Version', 'Versjon'],
  'cmp.oneVersion': ['This course has one version: pick another plan.', 'Dette emnet har én versjon: velg en annen plan.'],
  'cmp.noPlan': ['No plan chosen.', 'Ingen plan valgt.'],
  'cmp.notIn': ['{s}: not in this plan.', '{s}: ikke i denne planen.'],
  'cmp.open': ['open', 'åpne'],
  'cmp.openTitle': ['Open in the list view', 'Åpne i listevisningen'],
  'cmp.acrossIntro': [
    'The plans in force in {y} of the courses in the list (the filters and the query; set the subject or study type to line up one subject), a column per institution',
    'Planene som gjelder i {y} for emnene i listen (filtrene og søket; velg fag eller studietype for å stille opp ett fag), en kolonne per institusjon',
  ],
  'cmp.section': [', section {s}', ', seksjon {s}'],
  'cmp.first': ['Showing the first {m} of {n} plans: narrow the filters.', 'Viser de første {m} av {n} planer: snevre inn filtrene.'],
  'cmp.plansN': ['{n} plans.', '{n} planer.'],
  'cmp.noneYear': ['No plan of the courses in the list is in force in {y}.', 'Ingen plan for emnene i listen gjelder i {y}.'],
  'cmp.plan1': ['plan', 'plan'],
  'cmp.plans': ['plans', 'planer'],

  // term lists
  'tl.list': ['List', 'Liste'],
  'tl.name': ['Name of the list', 'Navn på listen'],
  'tl.new': ['new list', 'ny liste'],
  'tl.newName': ['New list', 'Ny liste'],
  'tl.import': ['import YAML', 'importer YAML'],
  'tl.exportYaml': ['export YAML', 'eksporter YAML'],
  'tl.delete': ['delete list', 'slett listen'],
  'tl.confirm': ['Delete the term list "{n}" from this browser?', 'Slette termlisten «{n}» fra denne nettleseren?'],
  'tl.intro1': ['A term is a query (Norwegian and English variants joined by OR, as', 'En term er et søk (norske og engelske varianter forbundet med OR, som'],
  'tl.intro2': ['), run with the scope of AND and the section set above. Lists are kept in this browser', '), kjørt med rekkevidden for AND og seksjonen valgt over. Listene lagres i denne nettleseren'],
  'tl.intro3': ['; YAML in the format of', '; YAML i formatet til'],
  'tl.none': ['No term list yet: make a new one or import a YAML file.', 'Ingen termliste ennå: lag en ny eller importer en YAML-fil.'],
  'tl.head': ['Share of course-years with a plan whose plan matches each term', 'Andel emneår med plan der planen treffer hver term'],
  'tl.rows': ['Rows', 'Rader'],
  'tl.term': ['term', 'term'],
  'tl.termInst': ['term per institution', 'term per institusjon'],
  'tl.cells': ['Cells', 'Celler'],
  'tl.share': ['share (%)', 'andel (%)'],
  'tl.cy': ['course-years', 'emneår'],
  'tl.running': ['running {n} terms…', 'kjører {n} termer…'],
  'tl.heatIntro': [
    'Course-years in force in the filter, as in the trend view: a course-year matches when any of its plans matches the term; the share is of the course-years with a plan (empty: none). Darkest:',
    'Emneår i filteret, som i trendvisningen: et emneår treffer når en av planene treffer termen; andelen er av emneårene med plan (tom: ingen). Mørkest:',
  ],
  'tl.click': ['Click a cell to list its courses.', 'Klikk en celle for å liste emnene.'],
  'tl.termHead': ['Term', 'Term'],
  'tl.inst': ['Institution', 'Institusjon'],
  'tl.all': ['all', 'alle'],
  'tl.cell': ['{t} · {i} · {y}: {m} matching of {w} with a plan ({p}); {n} course-years', '{t} · {i} · {y}: {m} treff av {w} med plan ({p}); {n} emneår'],
  'tl.terms': ['Terms ({n})', 'Termer ({n})'],
  'tl.label': ['Label', 'Etikett'],
  'tl.query': ['Query', 'Søk'],
  'tl.remove': ['remove', 'fjern'],
  'tl.removeAria': ['Remove {t}', 'Fjern {t}'],
  'tl.add': ['add a term', 'legg til en term'],
  'tl.addQ': ['add the current query', 'legg til gjeldende søk'],

  // shortcuts overlay
  'key.title': ['Keyboard shortcuts', 'Hurtigtaster'],
  'key.general': ['Everywhere', 'Overalt'],
  'key.screenHead': ['In the Screen view', 'I gjennomgangsvisningen'],
  'key.search': ['go to the search box', 'gå til søkefeltet'],
  'key.move': ['next / previous course in the list', 'neste / forrige emne i listen'],
  'key.arrows': ['the same, on a course in the list', 'det samme, på et emne i listen'],
  'key.open': ['open the course', 'åpne emnet'],
  'key.esc': ['leave the search box; close a dialog, the course or the guide', 'forlat søkefeltet; lukk en dialog, emnet eller veiledningen'],
  'key.help': ['show these shortcuts', 'vis disse hurtigtastene'],
  'key.hit': ['next / previous hit (also the arrow keys)', 'neste / forrige treff (også piltastene)'],
  'key.tag': ['include / exclude / maybe', 'ta med / utelat / kanskje'],
  'key.note': ['write a note', 'skriv et notat'],
  'key.openplan': ['open the plan', 'åpne planen'],
  'key.typing': ['The keys do nothing while you type in a field; Esc leaves it.', 'Tastene gjør ingenting mens du skriver i et felt; Esc forlater feltet.'],
  'key.guide': ['User guide', 'Brukerveiledning'],
  'key.close': ['Close', 'Lukk'],
}

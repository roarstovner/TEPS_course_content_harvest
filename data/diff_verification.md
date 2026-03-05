# OsloMet

**FIXED** (2026-02-26): Added `.pre_oslomet()` normalization in `R/normalize_plan_text.R`:
- Replaced semicolons (`;` → ` `) — OsloMet 2022 pages used semicolons as section separators, causing false diffs with other years. Replace (not remove) to avoid concatenating words that had no surrounding space.
- Stripped uppercase `\bVÅR\b` and `\bHØST\b` — appear as column headers in pensum tables and semester tags in course listings, not substantive plan content.

Verified fixes: M1GBO4100 (2021=2022 ✓), M1GEN2100 (2021=2022 ✓), M5GPE5100 (2021-2025 all same ✓), MGVM4100 (2020-2022 same, 2023-2025 same ✓).

Original issues:
- M1GBO4100: the diff between 2021 and 2022 is mainly (only?) semicolons appearing in 2022.
- M1GEN2100 differs only with semicolons between 2022 and 2023.
- M1GEN2100 differs by semicolons between 2021 and 2022 AND by changing VÅR to HØST. Is the latter a real change? I'd say no.
- M5GPE5100 is special, as it has v1 spanning 2021-2025 and v2 only for 2022. What gives? It's the semicolons again. Seems to be something happening in 2022.
- MAMUS4100 shouldn't be in the data at all. Is it reported wrong in FS, that is, courses.RDS has it in the wrong study program? (OPEN: appears in courses.RDS under two OsloMet institutes)
- MGNA4100 has the semicolon problem before and after 2022, 2021 -> 2022 -> 2023 (remaining diffs appear to be real content changes)
- MGVM4100 differs only in VÅR and HØST between v4 2023-2025 and v5 2024. The 2022 semicolon problems are present here as well.

# NTNU

- IDR3027, 2024, Vår: Lots of javascript or similar after "Vis detaljert timeplan i TP". Also at the bottom, which starts with "function toggleRooms(target) { const examRoomsDiv"
- The patterns from IDR3027 seems to hold for many courses. I haven't checked whether they cause diff problems yet, but I still don't want irrelevant code in the course plan fulltext.
- No issues found with the diffs

# INN

- 2EXPHIL51, 2022, Høst: There is no course plan in the fulltext?
- 2PEDL51-2-1, 2022, Høst: No course plan in fulltext?
- 2ENL512-3, 2022, Vår: no course plan in fulltext
- 2ENL512-1, 2022, Vår: The course plan states "Startsemestre\n2027 Vår" and "Undervisningssemestre\n2027 Vår"
- 2NOL51-5, 2022, Vår: Same issue, the Undervisningssemestre is not 2022 but 2026.
- 2PRALU2-4-1, 2022, Vår: No course plan. It seems that "Emnesøket" should be a keyword that signals that no course plan has been found.
- 2SFL51-1, 2022, Vår: Undervisningssemestre has the wrong year.
- 2ENLBA, 2024, Vår: Wrong Undervisningssemestre and Startsemestre
- 2ENL51-12, 2022-2024 to 2023-2024: Some of the keywords has been translated from Norwegian to English and does not indicate a real course plan change, like "Obligatoriske aktiviteter" to "Compulsory activities" and "Individuell" to "Individual". Wonder whether this is systemic across more courses? Especially the english courses with EN-code?
- 2ENL512-2: This also has the Norwegian to English keyword change. (Note that it also has other changes, so the course plan does change!)
- other diffs look legit.

# hivolda

- There's a table at the bottom of many pages. It shows the cells squashed rowwise "VurderingsformGrupperingVarighetKarakterskalaAndelKommentarHjelpemidlerOmfangOppgaveIndividuellA" Is it possible to retrieve the table structure in the fulltext?
- In MGL1-7PRO-1, you have both "Emneansvarlig:" and "Godkjent av:" that counts towards the diffs. Changes in Emneansvarlig is not a change in the course plan. 

# HiOF

- fulltexts have "Sist hentet fra FS (Felles studentsystem) DD. mmm. YYYY" at the end of all course texts. This should be deleted. I think this is a problem with many of the institutions? Inventigate.
- Litteraturlister shows in fulltext. This should be part of data_notes.md, and should possibly be noted for every institution.
- Diff LMBKHV10217 2023-2024 vs 2025: The diff is whether each section has a space after or not. "Kunnskap Studenten har kunnskap om [...]" vs "KunnskapStudenten har kunnskap om [...]"
- Diff LMBKHV10217 2020 vs 2021: Same. The diff is whether each section has a space after or not. "Kunnskap Kandidaten har kunnskap om [...]" vs "KunnskapKandidaten har kunnskap om [...]"
- LMUFRA11221, 2023 vs 2024: "Emneansvarlig: Anje Müller Gjesdal" is part of the diff.
- LMUKRLE10321: The space after each section "Kunnskap"/"Ferdigheter"/"Kompetanse" is here as well.

# HVL

- fulltext only has 2025 courses. Is this correct? Should it not have *any* 2024 courses? I think only the last version of the course plan is available on the website, but this still seems almost strange.
- What? There seems to be an easy URL pattern for older courses. See here: `https://www.hvl.no/studier/studieprogram/emne/2023/lupeki302` We probably should harvest a lot more HVL-plans!
- Could not compare Diffs. Fulltexts look OK.

# MF

- fulltexts look good
- diffs NA, because only 2025 is harvestable.

# NLA

- fulltexts look good
- But there is a simple html pattern to retrieve older years! `https://www.nla.no/for-studenter/Studie-%20og%20emneplaner/emneplan/4MGL1ENG101/2023-2024`. We should harvest them!

# NORD

- fulltexts look good.
- There's a simple URL pattern to harvest earlier years! `https://www.nord.no/studier/emner/smi5004?year=2023&semester=H%C3%98ST`

# NIH

- fulltexts look good
- diffs look good

# UIB

- fulltexts look good
- UiB only had data from 2025. Now using `?start_semester={YEAR}{H|V}` parameter for historical plans.
  - https://www4.uib.no/studier/emner/realdi110?start_semester=2024H
  - https://www4.uib.no/studier/emner/realdi110?start_semester=2025V
- RESOLVED: URL builder updated to include year+semester, harvest script no longer filters to latest year only.

# UIO

- fulltexts look good
- diffs nonexsistent because it's a "2025 only" course.

# UIS

- old course descriptions are available as pdf `https://www6.uis.no/Stine/EMNE/2024/H%C3%98ST/Bokm%C3%A5l/MHI330_1.pdf` using the following URL format. I'm not sure what the suffix "_1" means. We should try to harvest these course plans. Must use a pdf to text converter and temporarily store pdfs, probably.

# USN

- fulltexts looks good, but they do contain all the literature assigned in the course 
- HI-MOD4000, 2018 V 2019: "høst" and "vår" shows up as diff and probably should not? 
- LDKH102, 2024 vs 2025: "Kunnskap" and "KUNNSKAP" shows up as diff and probably should not. Same with other headings "Ferdigheter"/"FERDIGHETER", "Generell kompetanse:"/"GENERELL KOMPETANSE"
- LH-HID4000-1 2021 VS 2022: Diff "Vår21" and "Vår22". Also "Deltakelse/Obligatoriske arbeidskrav"/"Obligatorisk aktivitet og krav til tilstedeværelse" seems to be a heading that changes between many course plans at this time?
- MG1KR3 2024 VS 2025: Vurderingsformer"/"Eksamensformer" seems to be a change that's unrelated to the course plan.
- VG1MA6 2023 VS 2024: "Høst23"/"Høst24" probably should not be a diff. It's in the assigned readings.

# UIT

- fulltexts look good.
- only 2025 years. But there is a "gammeltemne" url pattern: https://uit.no/utdanning/emner/gammeltemne?p_document_id=619810&ar=2020&semester=V. This works when I type 2020, because it says "VÅR 2020" in the heading. If I change the URL to ar=2019 or ar=2021, it doesn't update. Other URLs to test with:
  - https://uit.no/utdanning/emner/gammeltemne?p_document_id=330720&ar=2013&semester=H
  - the web page for accessing older urls. Just change the year. (This works in the browser.) https://uit.no/utdanning/emner?ar=2014&semester=H&p_fagkode=36488,37903 The fagkode just filters the school and kindergarten teacher courses.

# NMBU

- fulltexts look good.
- NMBU is genuinely a one year only institution
- Note that the "emneansvarlig" is mentioned in the text, so this must be handled during deduplication

# SAMAS
# Steiner
# UiA

- the fulltext is kind of unreadable. Is everything on one line? Without newlines? If this can be fixed, it should be fixed!
- EN-157 2020 and 2021 differ only by "Emneansvarlige: Erik Mustad Charles Ivan Armstrong"
- NO-503 2023 and 2024-2025 differ by "Haust" and "Sist henta frå FS (Felles studentsystem) 23. juli". These are not real changes.
- PED160 2021 and 2022 differ only by "Emneansvarlige: Adina Marie Nydahl Alexandra Lazareva"
- REL421 differs only by "Vår" and "Vår, Høst"

# OsloMet

The diff is bad. Hash picks up non-changes.

- M1GBO4100: the diff between 2021 and 2022 is mainly (only?) semicolons appearing in 2022.
- M1GEN2100 differs only with semicolons between 2022 and 2023.
- M1GEN2100 differs by semicolons between 2021 and 2022 AND by changing VÅR to HØST. Is the latter a real change? I'd say no.
- M5GPE5100 is special, as it has v1 spanning 2021-2025 and v2 only for 2022. What gives? It's the semicolons again. Seems to be something happening in 2022.
- MAMUS4100 shouldn't be in the data at all. Is it reported wrong in FS, that is, courses.RDS has it in the wrong study program?
- MGNA4100 has the semicolon problem before and after 2022, 2021 -> 2022 -> 2023
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

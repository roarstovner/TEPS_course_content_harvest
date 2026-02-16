# Deduplication Validation Summary

**Date:** 2026-02-13
**Scope:** All 12 harvested institutions
**Pipeline:** `normalize_plan_text()` + `deduplicate_plans()`

## Overall Summary

| Metric | Value |
|--------|-------|
| Total course-year rows | 32,494 |
| Rows with fulltext | 23,386 (71.9%) |
| Rows without fulltext (NA) | 9,108 (28.0%) |
| Unique plans after dedup | 5,376 |
| Overall dedup ratio | 77.0% |
| Compression factor | 4.4x |

## Per-Institution Dedup Ratios

| Institution | Course Rows | Unique Plans | Dedup Ratio | Assessment |
|-------------|-------------|--------------|-------------|------------|
| uib | 2,613 | 106 | 95.9% | Excellent |
| mf | 603 | 28 | 95.4% | Excellent |
| uio | 1,767 | 93 | 94.7% | Excellent |
| hvl | 3,599 | 297 | 91.7% | Excellent |
| uit | 6,693 | 555 | 91.7% | Excellent |
| hivolda | 1,182 | 99 | 91.6% | Excellent |
| nord | 3,664 | 441 | 88.0% | Good |
| inn | 2,368 | 585 | 75.3% | Moderate - language switching |
| ntnu | 5,735 | 1,532 | 73.3% | Moderate - genuine changes |
| uia | 2,374 | 702 | 70.4% | Moderate - genuine changes |
| nih | 184 | 82 | 55.4% | Low - small dataset, annual edits |
| hiof | 1,712 | 827 | 51.7% | Low - genuine changes |

## NA Fulltext by Institution

| Institution | Total | NA Fulltext | % NA |
|-------------|-------|-------------|------|
| uio | 1,767 | 1,674 | 94.7% |
| nih | 184 | 103 | 56.0% |
| uia | 2,374 | 1,244 | 52.4% |
| hiof | 1,712 | 866 | 50.6% |
| mf | 603 | 294 | 48.8% |
| inn | 2,368 | 1,086 | 45.9% |
| uib | 2,613 | 997 | 38.2% |
| ntnu | 5,735 | 1,944 | 33.9% |
| hvl | 3,599 | 716 | 19.9% |
| nord | 3,664 | 10 | 0.3% |
| hivolda | 1,182 | 0 | 0.0% |
| uit | 6,693 | 0 | 0.0% |

High NA rates for UiO (94.7%) and several others reflect 404s, discontinued courses, or URL issues rather than dedup problems.

## Plan Version Distribution

Courses with single plan version (perfect dedup) dominate at 6 of 12 institutions (hivolda, hvl, mf, nord, uib, uit). These institutions show essentially static course descriptions across years.

Institutions with many multi-version courses:
- **NTNU**: 169 single-version, but many courses have 8-18 versions
- **UiA**: 237 courses with 4+ versions
- **HiOF**: Spread across 1-10 versions per course
- **NIH**: Most courses have 2-5 versions despite small dataset

## Spot-Check Findings

### NTNU (73.3% dedup) - Genuine Content Evolution
Course BI3950 (Masteroppgave i biologi) has 11 plan versions spanning 2007-2025. Changes include:
- Assessment method changed ("Avhandling" -> "Masteroppgave" -> "Masteroppgave med muntlig forsvar")
- Content description revised substantially between versions
- Prerequisites and conditions updated over time

**Verdict:** Genuine, substantive changes. Low dedup is correct behavior.

### HiOF (51.7% dedup) - Genuine Annual Edits
Course LMAGENT21 has 4 versions across 2021-2024. Differences include:
- Exam section changes (e.g., 2024 adds "Arbeidskrav vurderes til godkjent" where 2023 has exam details)
- Lengths differ (7,558-10,342 chars)
- Each year brings minor but meaningful structural changes

**Verdict:** Genuine changes. Normalization is working correctly.

### INN (75.3% dedup) - Language Switching Issue
Course 2ENL51-10 (Shakespeare) appears in both Norwegian and English versions:
- Norwegian labels: "Studiepoeng", "Undervisningssemestre", "Undervisnings- og eksamenssprak"
- English labels: "Number of credits", "Teaching semester", "Language of instruction and examination"
- The actual course content is identical; only the UI labels differ

**Verdict:** Normalization could be improved. Stripping field labels before hashing would increase dedup for INN. However, some versions also have genuinely different content, so the impact would be partial.

### NIH (55.4% dedup) - Small Annual Edits
Course LKI100 has 5 versions across 2021-2024. Text lengths vary (2,588-2,802 chars) with each year bringing minor wording changes.

**Verdict:** Genuine changes. Small dataset amplifies the effect of minor edits.

### UiA (70.4% dedup) - Genuine Content Evolution
Course EN-153 has 5 versions with growing text lengths (5,013-5,235 chars). Content is updated annually.

**Verdict:** Genuine changes.

## Edge Cases

### Courses in Multiple Institutions
14 course codes appear at 2 institutions (e.g., MUS1001-MUS2002 at both INN and Nord; PED/PRA courses at MF and Nord). These are likely collaborative programs or transferred courses. The dedup pipeline handles them correctly by keeping institution as part of the grouping.

### Shared plan_content_id Across Course Codes
16 plan IDs are shared across different Emnekode values. Investigation reveals these are:
- **INN error pages**: 9 course codes share the same "Emnesoket gjelder kun fra..." placeholder text (478 chars). These are empty/error pages where the course plan was not found.
- **UiT near-duplicates**: Course code variants (e.g., LRU-3901 / LRU-3901F, LER-3901 / LER-3911) share identical content.
- **NTNU grouped courses**: Several course codes (MGBEN501-MGBEN508) share a single plan page.

**Recommendation:** The INN error pages should ideally be filtered out as non-content before dedup. The UiT and NTNU cases are legitimate shared content.

## Assessment: Is Dedup Quality Sufficient for Research Use?

**Yes, with caveats.**

### Strengths
1. **High-dedup institutions (7 of 12)** achieve 88-96% dedup, correctly identifying that most course plans are reused across years.
2. **Low-dedup institutions** show genuine content changes, not normalization failures. The pipeline correctly distinguishes versions.
3. **4.4x compression** overall reduces storage and analysis burden substantially.
4. **plan_content_id** correctly links course-year rows to unique text versions.

### Caveats
1. **INN language switching** (Norwegian vs English field labels) inflates version count. Stripping common field labels during normalization would improve dedup from 75% to an estimated 85-90%.
2. **INN error pages** (9 course codes sharing placeholder text) should be filtered as NA rather than treated as valid plans.
3. **28% of rows have NA fulltext** -- this is a harvesting/URL issue, not a dedup issue, but it limits overall coverage.

### Recommendations for Normalization Improvements
1. **Strip common field labels** in `normalize_plan_text()`: Remove Norwegian and English variants of standard labels ("Studiepoeng"/"Number of credits", "Undervisningssemester"/"Teaching semester", etc.) before hashing. This would primarily benefit INN.
2. **Filter error/placeholder pages**: Detect and mark pages with only navigation/template text (e.g., INN's "Emnesoket gjelder kun fra" pages) as NA before dedup.
3. **Consider fuzzy matching**: For institutions with minor annual edits (NIH, HiOF), a similarity threshold (e.g., >95% identical) could further reduce version count. However, this trades accuracy for compression and may not be appropriate for all research questions.

### Final Verdict
The deduplication pipeline is **production-ready for research use**. The 77% overall dedup ratio correctly reflects a mix of stable plans (most institutions) and genuinely evolving content (NTNU, HiOF, UiA, NIH). The INN language switching issue is the only clear normalization gap, and it affects a single institution.

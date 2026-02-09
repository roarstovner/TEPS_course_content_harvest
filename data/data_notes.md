
# UiA

**Verified 2026-02-09**: Pipeline working correctly with updated CSS selector.

## Course Availability
- **Year range**: UiA website only has courses from **2020 onwards** (years 2020-2028 available as of 2026-02)
  - Courses from 2013-2019 exist in DBH but return 404 on website
  - Filter applied in `R/run_harvest_uia.R` to only process 2020+ courses

## Success Rate Analysis
- **Overall**: ~48% (1130/2374 courses from 2020-2025)
- **By semester**:
  - **Autumn (Høst)**: 77.5% success (934/1205) ← Primary teaching semester
  - **Spring (Vår)**: 16.8% success (196/1169) ← Mostly duplicates from DBH
- **Low spring success rate is NOT a bug** - see "Whole-year courses" below

## Whole-Year Courses Pattern
**Similar to USN**: DBH registers courses for both semesters, but UiA typically publishes only one page.

Out of **1,150 courses** registered in BOTH semesters in DBH:
- **877 (76.3%)**: Autumn page exists, spring page missing ← **Most common pattern**
- **193 (16.8%)**: Spring page exists, autumn page missing
- **3 (0.3%)**: Both pages exist (e.g., MA-506 master thesis explicitly offered both semesters)
- **77 (6.7%)**: Neither page exists (discontinued or never published)

**Verified example**: EN-148 (2020) exists in DBH for both semesters, but only autumn page exists on website. The course page states "Undervisningssemester: Høst" (autumn only).

**Conclusion**: Courses are typically offered in ONE semester but registered in DBH for both. This is institutional practice, not a data error.

## URL vs Content Semester Mismatch
**Important finding**: The URL semester doesn't always match the actual teaching semester!

Analysis of 1,130 successfully fetched pages:
- **Autumn (Høst) URLs** (934 total):
  - 529 (57%): Content says "Undervisningssemester: Høst" ✓ (matches URL)
  - 346 (37%): Content says "Undervisningssemester: Vår" ✗ (spring course at autumn URL!)
  - 13 (1%): Content says "Vår, Høst" (offered both)
  - 46 (5%): Semester not found in content
- **Spring (Vår) URLs** (196 total):
  - 192 (98%): Content says "Undervisningssemester: Vår" ✓ (matches URL)
  - 4 (2%): Content says "Vår, Høst" (offered both)
  - 0: No mismatches to autumn-only

**Implication**: UiA publishes some spring-only courses under autumn URLs. When fetching autumn URLs, we capture both autumn courses AND ~37% spring courses. Testing showed most failed spring URLs also fail under autumn URLs (courses truly don't exist), confirming our success rate is accurate.

**Example**: `https://www.uia.no/studier/emner/2022/host/en-156.html` - URL says "host" (autumn), but content says "Undervisningssemester: Vår" (spring).

## Technical Details
- **URL pattern**: `https://www.uia.no/studier/emner/{year}/{semester}/{course_code}.html`
  - Semester mapping: "Vår" → "var", "Høst" → "host"
  - Course code: lowercase, with hyphens preserved (e.g., `bio104`, `sv-155`, `en-221`)
- **CSS selector**: `#right-main` (fixed from initial `.main-text`)
  - Captures 405-7225 characters (median: 4405) for successful pages
  - Includes breadcrumbs, course code, title, description, learning outcomes, teaching methods, assessment
- **Harvest script**: `R/run_harvest_uia.R` (includes validation and summary statistics)

# UiO

UiO does not offer the Emneplan by year, only the newest version of the Emneplan is available. Therefore, Emneplan was only downloaded for 2025 or, if the course had been discontinued, the last year the course was offered.

# USN

- **Expected ~30% success rate**: Not a bug. Breakdown of 318 unique active course codes:
  - 39% autumn-start (only autumn pages)
  - 11% spring-start (only spring pages)
  - 25% have pages in both semesters
  - 25% have no USN course pages at all
- **"New" courses (status=2)**: ~5% success. Pages created when courses become "Active" (status=1), where success jumps to ~81%.
- **Whole-year courses**: Autumn page only; spring semester entries return NA (expected).
- **Checkpoint can have stale failures**: If resolver had timing/network issues, valid pages may be marked as failures. Fix by removing NA entries from checkpoint and re-running.
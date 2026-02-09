
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
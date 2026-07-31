## Test environments
- macOS Tahoe 26.5.2 (arm64), R 4.6.1, local

## R CMD check results
0 errors | 0 warnings | 1 note

* NOTE: Local HTML validation was skipped because the installed `tidy` binary
  is not recent enough and the optional `V8` package is unavailable. This is
  environment-specific; all package tests and vignettes completed successfully.

## Changes since 1.1.2
- Added an explicit choice-label-to-profile mapping option to
  `reshape_projoint()`.
- Added fail-fast validation for invalid, ambiguous, whitespace-altered, and
  missing choices; missing choices can still be retained explicitly.
- Added respondent-ID, argument, and post-reshape choice-integrity checks.
- Improved `read_Qualtrics()` handling of current and legacy metadata rows.
- Added regression tests for physically reordered Qualtrics response columns,
  explicit and reversed profile mappings, malformed choices, missing choices,
  and duplicate respondent IDs.
- Updated documentation and vignettes to emphasize that profile identity must
  be verified from the survey instrument or QSF file and cannot be inferred
  from the CSV alone.

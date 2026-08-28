## Test environments
- macOS Tahoe 26.6.2 (arm64), R 4.6.1, local

## R CMD check results
0 errors | 0 warnings | 1 note

* NOTE: Local HTML validation was skipped because the installed `tidy` binary
  is not recent enough and the optional `V8` package is unavailable. This is
  environment-specific; all package tests and vignettes completed successfully.

## Changes since 1.1.3
- Updated `predict_tau()` for estimatr 2.0.0 by excluding groups with fewer
  than two respondent clusters before requesting a cluster-robust variance.
- Added regression coverage confirming that the bundled example retains the
  same complete predicted-IRR sequence.

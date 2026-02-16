## Test environments
- macOS (R 4.5.2), local
- Windows (win-builder: R-release, R-devel)
- Ubuntu 22.04 (GitHub Actions, R-release)

## R CMD check results
0 errors | 0 warnings | 1 note

* NOTE: installed size is ~7 MB.
  The package includes example data and vignettes used in documentation and runnable examples.
  Data files are compressed; no unnecessary large files are shipped.

## Changes since 1.0.5
- Fixed a bug in ``reshape_projoint()'' related to repeated-task reshaping / task-outcome alignment for some inputs.
- Added additional validation checks in ``reshape_projoint()'' to fail fast with clearer messages when the supplied design/outcomes are incomplete.
- Internal refactor only: moved implementation into ``R/reshape_projoint.R''; no changes to exported API.

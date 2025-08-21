## Test environments
- Local: macOS (Apple Silicon), R 4.4.x
- Ubuntu 22.04 (GitHub Actions), R release
- Windows (win-builder), R devel and R release

## R CMD check results
0 errors | 0 warnings | 2 notes

* This is a resubmission (1.0.3).

### NOTE 1: Package size
Some checks report:
- Installed size ≈ 6–8 MB  
- Large subdirectories: `extdata` (~4 MB), `doc` (~2–3 MB)

**Explanation:**  
- `inst/extdata/` contains example datasets used in vignettes and tests for reproducibility.  
- `doc/` contains built vignettes.  

We have compressed data with `xz`, minimized file count, and pruned vignette artifacts. If requested, we can further slim the package (e.g., move large files to a companion data package).

### NOTE 2: DESCRIPTION Author field
A NOTE was raised in v1.0.2 that the `Author:` field differed from the derived `Authors@R`.  
We have corrected this in v1.0.3 by using ORCID URLs in `Authors@R`, which now matches the automatically generated `Author:` field.

## Reverse dependencies
None.

## Misc
- URLs verified (200 or appropriate redirects).  
- `LICENSE` and `DESCRIPTION` fields comply with CRAN policies.

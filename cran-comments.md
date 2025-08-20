## Test environments
- Local: macOS (Apple Silicon), R 4.4.x
- Ubuntu 22.04 (GitHub Actions), R release
- Windows (win-builder), R devel and R release

## R CMD check results
0 errors | 0 warnings | 2 notes

* This is a resubmission.  
* Changes since 1.0.0 (per CRAN pre-test feedback):
  - Replaced old-style `citEntry()` with `bibentry()` in `inst/CITATION`.
  - Fixed relative README links (now absolute URLs to pkgdown site; removed invalid URIs).
  - Added `Depends: R (>= 4.1.0)` to account for `|>` and `\(…)` syntax.
  - Removed redundant `Author:` field; kept only `Authors@R`.
  - Added `cran-comments.md` to `.Rbuildignore`.

### NOTE 1: Package size
Some checks report:
- Installed size ≈ 6–8 MB  
- Large subdirectories: `extdata` (~4 MB), `doc` (~2–3 MB)

**Explanation:**  
- `inst/extdata/` contains small example datasets used in vignettes and tests to ensure reproducibility.  
- `doc/` contains built vignettes.  

We have compressed data with `xz`, minimized file count, and pruned vignette artifacts. If requested, we can further slim the package (e.g., move large files to a companion data package).

### NOTE 2: Spelling
Domain terms such as “AMCEs”, “Qualtrics”, “pkgdown”, and “IRR” are correctly spelled.

## Reverse dependencies
None.

## Misc
- URLs verified (200 or appropriate redirects).  
- `LICENSE` and `DESCRIPTION` fields comply with CRAN policies.

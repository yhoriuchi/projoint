# projoint 0.1.0 (2025-02-05)

- Initial release.

# projoint 0.2.0 (2025-04-20)

- Minor bug fixes and documentation improvements.
- Major redesign of the website, including improved vignettes and a clearer step-by-step tutorial structure.
- Fixed input validation for specifying inter-rater reliability (IRR), ensuring that IRR values must be between 0.5 and 1.

# projoint 0.3.0 (2025-08-17)

- Robust SEs & clustering: fixes to variance options; users can now pass arguments supported by `estimatr::lm_robust()` for clustering and standard errors.
- `reshape_projoint()` overhaul: now robust to arbitrary base-task orders, infers the repeated base task from the first base outcome, and requires the repeated outcome to be the last element of `.outcomes`.

# projoint 0.3.1 (2025-08-18)

- Bug fix for minor issue.  
- Vignettes extended to illustrate additional choice-level analyses.  

# projoint 1.0.0 (2025-08-19)

- Initial CRAN release

# projoint 1.0.1 (2025-08-19)

- Fix CITATION: switched from citEntry() to bibentry().
- Fixed README links (absolute URLs).
- Added Depends: R (>= 4.1.0).
- Removed duplicate Author field.
- Added cran-comments.md to .Rbuildignore.

# projoint 1.0.2 (2025-08-21)

- CRAN resubmission: fix CITATION (use `bibentry()`, avoid install-time lookups).
- README: replace relative URLs; fix broken BibTeX link (use GitHub raw URL).
- DESCRIPTION: add aligned `Author:` alongside `Authors@R`.
- Bug fix: correct error message when `.irr` is user-specified.
- Robustness: skip IRR estimation (sim/boot) when `.irr` is fixed.
- Docs: new vignette “Explore” and corresponding README/pkgdown links.

# projoint 1.0.3 (2025-08-21)

- DESCRIPTION: aligned `Author:` exactly with CRAN’s derived `Authors@R` format (names and ORCID link formatting) to remove NOTE.

# projoint 1.0.4 (2025-08-21)

- Fixed DESCRIPTION metadata mismatch between `Author` and `Authors@R` fields, as requested by CRAN.

# projoint 1.0.5 (2025-09-10)

## Improvements
- Revised `pj_estimate()` to automatically set the appropriate standard-error option (`se_type`) depending on clustering and estimation method.
- Expanded documentation across functions:
  - All exported methods now include `\value{}` sections describing return values.
  - Clearer examples for choice-level analysis; removed commented-out code.
  - Improved description in `DESCRIPTION` to highlight both profile-level and choice-level estimators, IRR correction, and visualization.
- Updated README and pkgdown site to emphasize **choice-level analysis** as the default and recommended framework.

## Bug fixes
- Fixed `reshape_projoint()` docs (`\textrightarrow` macro removed).
- Standardized return values for `summary()`, `plot()`, and `print()` methods to match CRAN policies.

# projoint 1.0.6 (2026-10-24)

## CRAN maintenance update

This release addresses all issues raised in the CRAN review and includes several small improvements and documentation updates.

### Changes and fixes

- **DESCRIPTION:** References now follow the required format `authors (year) <https://...>`.  
- **Examples:** Removed commented-out code and replaced examples of unexported functions (`projoint_data()`) with runnable toy examples.  
- **Internal helpers:** Suppressed documentation for unexported internal functions (`projoint_tau()`, `plot_projoint_profile_level()`) using `@noRd`.  
- **Cross-references:** Fixed a missing link warning in `plot.projoint_results.Rd`.  
- **make_projoint_data():** Fixed a minor error affecting input validation.  
- **Vignettes and website:** Revised and expanded for clarity, examples, and reproducibility.  

### Notes

- `R CMD check --as-cran` runs cleanly with 0 errors, 0 warnings, and 2 expected NOTES (package size and timestamps).


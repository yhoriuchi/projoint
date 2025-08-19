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


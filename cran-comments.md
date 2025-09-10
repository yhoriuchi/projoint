## Test environments
- macOS (R 4.4.x), local
- Windows (win-builder devel/release)
- Ubuntu 22.04 (GitHub Actions)

## R CMD check results
0 errors | 0 warnings | 1 note

* NOTE (installed size ~7 MB): The package includes small example data and vignettes. 
  Data files are xz-compressed and we avoid shipping prebuilt docs. 
  Remaining files are minimal and needed for examples and documentation.

## Changes since 1.0.4
This resubmission addresses all issues raised by CRAN:

- **DESCRIPTION**: expanded to provide a full paragraph, including details of implemented functionality and methods. Added a reference in the requested format.
- **Documentation**: all exported methods now include `\value{}` sections describing the structure and meaning of return values (or noting when none is returned).
- **Examples**: removed commented-out example code. Added lightweight runnable examples, with longer ones wrapped in `\donttest{}`.
- **CITATION**: updated to show the correct package version (1.0.5).
- **Minor fixes**: vignette examples cleaned, plotting examples simplified to avoid use of unexported helpers.

No other issues remain; all CRAN checks pass locally and on win-builder.

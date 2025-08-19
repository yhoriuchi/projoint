# This is a manual test script for interactive checking of projoint()
# It is NOT run automatically during R CMD check.

devtools::document()
devtools::build()
devtools::install()
# devtools::test()

unlink("docs", recursive = TRUE)  # Delete docs folder
pkgdown::build_site(new_process = TRUE, install = TRUE)


# Final check before releasing it at CRAN ---------------------------------

devtools::document()
unlink("docs", recursive = TRUE)  # Delete docs folder
devtools::build_vignettes()
devtools::check(args = "--as-cran")
devtools::check_win_devel()   # Windows devel
devtools::check_win_release() # Windows release
# rhub::check_for_cran()        # extra Linux/Windows checks
devtools::build()

devtools::install()


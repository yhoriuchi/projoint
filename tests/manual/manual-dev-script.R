# This is a manual test script for interactive checking of projoint()
# It is NOT run automatically during R CMD check.

devtools::document()
devtools::build()
devtools::install()
devtools::check()

# devtools::test()

unlink("docs", recursive = TRUE)  # Delete docs folder
pkgdown::build_site(new_process = TRUE, install = TRUE)


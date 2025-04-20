# This is a manual test script for interactive checking of projoint()
# It is NOT run automatically during R CMD check.

devtools::document()
devtools::build()
devtools::install()
devtools::check()

# devtools::test()

pkgdown::build_site()

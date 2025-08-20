# This is a manual test script for interactive checking of projoint()
# It is NOT run automatically during R CMD check.

devtools::document()
devtools::build()
devtools::check()
devtools::install()
# devtools::test()

unlink("docs", recursive = TRUE)  # Delete docs folder
pkgdown::build_site(new_process = TRUE, install = TRUE)


# Final check before releasing it at CRAN ---------------------------------

# Step 1. Rebuild tarball
devtools::document()
devtools::build_vignettes()
devtools::check(args = "--as-cran")
devtools::check_win_devel()   # Windows devel
devtools::check_win_release() # Windows release
# rhub::check_for_cran()        # extra Linux/Windows checks
devtools::build()

# Step 2. Inspect the tarball
untar("projoint_1.0.1.tar.gz", list = TRUE)

# Step 3. Test the tarball directly
devtools::check_built("projoint_1.0.1.tar.gz")
# This is a manual test script for interactive checking of projoint()
# It is NOT run automatically during R CMD check.

devtools::document()
devtools::clean_dll() # if compiled code; safe otherwise
devtools::check() # confirm examples pass locally

devtools::build()
devtools::install()

options(pkgdown.internet = FALSE)
unlink("docs", recursive = TRUE)  # Delete docs folder
pkgdown::build_site(new_process = TRUE, install = TRUE)


# Final check before releasing it at CRAN ---------------------------------

# Step 1. Rebuild tarball
devtools::document()
devtools::build_vignettes()
devtools::check(args = "--as-cran") # CRAN-like check
# devtools::check_win_devel()   # Windows devel
# devtools::check_win_release() # Windows release
# rhub::check_for_cran()        # extra Linux/Windows checks
devtools::build()

# Step 2. Inspect the tarball
untar("../projoint_1.0.6.tar.gz", list = TRUE)

# Step 3. Test the tarball directly
devtools::check_built("../projoint_1.0.5.tar.gz")

# # Step 4. Optional local install test
# install.packages("../projoint_1.0.6.tar.gz", repos = NULL, type = "source")

# # sanity-check inside the tarball
# ct <- utils::untar("../projoint_1.0.5.tar.gz", list = TRUE)
# desc_rel <- ct[grepl("/DESCRIPTION$", ct)][1]
# tmp <- tempfile(); dir.create(tmp)
# utils::untar("../projoint_1.0.5.tar.gz", files = desc_rel, exdir = tmp)
# dcf <- read.dcf(file.path(tmp, desc_rel))
# dcf[,"Author", drop = TRUE]  # should show ORCIDs with <...>

# 0) Clean & rebuild docs
devtools::document()

# 1) Fast local checks
urlchecker::url_check()              # verify URLs
tools::checkRd("man")                # Rd sanity
devtools::build_vignettes()          # make sure all vignettes knit cleanly

# 2) Full CRAN-style check
devtools::check(args = c("--as-cran"))

# 3) Optional size tidy (only if NOTE persists)
# tools::resaveRdaFiles("data", compress = "xz")
# tools::check_packages_in_dir(".", sizes = "installed")

# 4) Make sure that version is updated in Citation 
library(projoint)
citation("projoint")   # check CITATION output

# 4) 
devtools::check_win_release()
devtools::check_win_devel()

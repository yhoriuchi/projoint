test_that("the package citation identifies the current CRAN release", {
  package_citation <- citation("projoint")[[1]]

  expect_match(
    paste(toBibtex(package_citation), collapse = "\n"),
    "@Manual\\{projoint,"
  )
  expect_equal(
    package_citation$title,
    "projoint: Conjoint Analysis with Reliability Correction and Visualization"
  )
  expect_equal(package_citation$year, "2026")
  expect_equal(
    package_citation$note,
    paste("R package version", as.character(packageVersion("projoint")))
  )
  expect_equal(package_citation$organization, "Comprehensive R Archive Network (CRAN)")
  expect_equal(package_citation$doi, "10.32614/CRAN.package.projoint")
  expect_equal(package_citation$url, "https://CRAN.R-project.org/package=projoint")
})

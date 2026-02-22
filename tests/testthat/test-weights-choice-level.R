test_that(".weights_2 works for choice-level MMs (same-attribute and cross-attribute QOIs)", {
  # Load package data
  data("exampleData1", package = "projoint", envir = environment())
  expect_true(exists("exampleData1"))

  # Add a synthetic covariate to be used as weights
  set.seed(123)
  exampleData1$weight <- stats::runif(nrow(exampleData1))

  # Outcomes as used in the reported issue
  outcomes <- c(paste0("choice", 1:8), "choice1_repeated_flipped")

  out1 <- reshape_projoint(
    exampleData1,
    outcomes,
    .covariates = "weight"
  )

  # Sanity: weight must survive reshape into out1$data
  expect_true("weight" %in% names(out1$data))

  # ------------------------------------------------------------
  # 1) Same-attribute QOI (att1 level3 vs att1 level1)
  # ------------------------------------------------------------
  qoi_same <- set_qoi(
    .structure     = "choice_level",
    .att_choose    = "att1",
    .lev_choose    = "level3",
    .att_notchoose = "att1",
    .lev_notchoose = "level1"
  )

  expect_error(
    projoint(out1, qoi_same, .weights_2 = weight),
    NA
  )

  expect_error(
    projoint(out1, qoi_same, .weights_1 = weight),
    NA
  )

  expect_error(
    projoint(out1, qoi_same, .weights_1 = weight, .weights_2 = weight),
    NA
  )

  # ------------------------------------------------------------
  # 2) Cross-attribute QOI (att1 level1 vs att3 level1)
  # ------------------------------------------------------------
  qoi_cross <- set_qoi(
    .structure     = "choice_level",
    .att_choose    = "att1",
    .lev_choose    = "level1",
    .att_notchoose = "att3",
    .lev_notchoose = "level1"
  )

  expect_error(
    projoint(out1, qoi_cross, .weights_2 = weight),
    NA
  )
})

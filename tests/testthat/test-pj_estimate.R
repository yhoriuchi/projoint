test_that("profile-level MM analytical runs and returns sane output (warning not required)", {
  data(exampleData1, package = "projoint")
  pj <- reshape_projoint(exampleData1, c(paste0("choice", 1:8), "choice1_repeated_flipped"))
  
  expect_no_error({
    fit <- projoint(
      pj,
      .structure = "profile_level",
      .estimand  = "mm",
      .se_method = "analytical"
    )
    # swallow printed banner so the test runner stays quiet
    invisible(capture.output(summary(fit)))
  })
  
  sm <- summary(fit)
  expect_true(all(is.finite(sm$estimate)))
  expect_true(all(is.finite(sm$se) | is.na(sm$se)))
  used <- attr(sm, "se_type_used")
  expect_true(used %in% c("CR2","stata","HC1"))
})

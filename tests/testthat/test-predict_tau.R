test_that("predict_tau excludes groups without cluster variation", {
  data(out1_arranged, package = "projoint")

  fit <- predict_tau(out1_arranged)

  expect_s3_class(fit, "projoint_tau")
  expect_equal(fit$irr$x, 0:7)
  expect_equal(
    unname(fit$irr$predicted),
    c(
      0.743428911213145,
      0.709072855179032,
      0.674716799144919,
      0.640360743110806,
      0.606004687076693,
      0.571648631042580,
      0.537292575008467,
      0.502936518974354
    ),
    tolerance = 1e-12
  )
})

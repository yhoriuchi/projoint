test_that("pj_estimate basic functionality works (analytical SE)", {
  data("exampleData1", package = "projoint")
  
  d <- reshape_projoint(
    exampleData1, 
    .outcomes = c(paste0("choice", 1:8), "choice1_repeated_flipped")
  )
  
  attribute_levels <- d$labels$level_id
  
  i <- 1
  
  attribute <- stringr::str_extract(attribute_levels[i], "^.+(?=:)")
  level     <- stringr::str_extract(attribute_levels[i], "(?<=:).+$")
  
  
  out <- pj_estimate(
    .data = d,
    .structure = "profile_level",
    .estimand = "mm",
    .att_choose = "att1",  # Adjust if necessary
    .lev_choose = "level1",      # Adjust if necessary
    .att_notchoose = NULL,
    .lev_notchoose = NULL,
    .att_choose_b = NULL,
    .lev_choose_b = NULL,
    .att_notchoose_b = NULL,
    .lev_notchoose_b = NULL,
    .se_method = "analytical",
    .irr = NULL,
    .remove_ties = TRUE,
    .ignore_position = NULL,
    .n_sims = NULL,
    .n_boot = NULL,
    .weights_1 = NULL,
    .clusters_1 = NULL,
    .se_type_1 = "classical",
    .weights_2 = NULL,
    .clusters_2 = NULL,
    .se_type_2 = "classical"
  )
  
  expect_s3_class(out, "data.frame")
  expect_true(all(c("estimand", "estimate", "se", "conf.low", "conf.high", "tau") %in% names(out)))
  expect_equal(nrow(out), 2)
})

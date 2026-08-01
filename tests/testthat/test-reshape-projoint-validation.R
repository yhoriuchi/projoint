data(exampleData3, package = "projoint")

test_that("reshape_projoint preserves declared task mapping after column permutation", {
  outcomes <- paste0("choice", 1:8)
  permuted_data <- exampleData3[c(
    setdiff(names(exampleData3), outcomes),
    rev(outcomes)
  )]

  canonical <- reshape_projoint(
    exampleData3,
    outcomes,
    .repeated = FALSE
  )
  permuted <- reshape_projoint(
    permuted_data,
    outcomes,
    .repeated = FALSE
  )

  expect_equal(permuted$data, canonical$data)
})

test_that("reshape_projoint supports an explicit choice-to-profile map", {
  outcomes <- paste0("choice", 1:8)

  positional <- reshape_projoint(
    exampleData3,
    outcomes,
    .repeated = FALSE
  )
  explicit <- reshape_projoint(
    exampleData3,
    outcomes,
    .choice_map = c("Community A" = 1, "Community B" = 2),
    .repeated = FALSE
  )
  reversed <- reshape_projoint(
    exampleData3,
    outcomes,
    .choice_map = c("Community A" = 2, "Community B" = 1),
    .repeated = FALSE
  )

  expect_equal(explicit$data, positional$data)
  expect_equal(reversed$data$selected, 1 - positional$data$selected)
})

test_that("reshape_projoint matches full multi-character suffixes", {
  outcomes <- paste0("choice", 1:8)
  labelled_data <- exampleData3
  labelled_data[outcomes] <- lapply(labelled_data[outcomes], function(x) {
    x <- sub("A$", "Profile One", x)
    sub("B$", "Profile Two", x)
  })

  result <- reshape_projoint(
    labelled_data,
    outcomes,
    .choice_map = c("Profile One" = 1, "Profile Two" = 2),
    .repeated = FALSE
  )

  expect_true(all(result$data$selected %in% c(0, 1)))
})

test_that("reshape_projoint rejects unverified or malformed choices", {
  outcomes <- paste0("choice", 1:8)

  invalid <- exampleData3
  invalid$choice1[1] <- "Community Z"
  expect_error(
    reshape_projoint(invalid, outcomes, .repeated = FALSE),
    "Invalid value"
  )

  whitespace <- exampleData3
  whitespace$choice1[1] <- paste0(whitespace$choice1[1], " ")
  expect_error(
    reshape_projoint(whitespace, outcomes, .repeated = FALSE),
    "trailing whitespace"
  )

  expect_error(
    reshape_projoint(
      exampleData3,
      outcomes,
      .choice_labels = c("A", "BA"),
      .repeated = FALSE
    ),
    "suffixes of one another"
  )
})

test_that("reshape_projoint requires explicit approval for missing choices", {
  outcomes <- paste0("choice", 1:8)
  missing <- exampleData3
  affected_id <- missing$ResponseId[1]
  missing$choice1[1] <- NA_character_

  expect_error(
    reshape_projoint(missing, outcomes, .repeated = FALSE),
    "Missing choices"
  )

  result <- reshape_projoint(
    missing,
    outcomes,
    .repeated = FALSE,
    .allow_missing_choices = TRUE
  )
  affected <- result$data[
    result$data$id == affected_id & result$data$task == 1,
  ]

  expect_equal(nrow(affected), 2L)
  expect_true(all(is.na(affected$selected)))
})

test_that("reshape_projoint validates identifiers and mapping arguments", {
  outcomes <- paste0("choice", 1:8)
  duplicated <- dplyr::bind_rows(exampleData3, exampleData3[1, ])

  expect_error(
    reshape_projoint(duplicated, outcomes, .repeated = FALSE),
    "duplicate IDs"
  )
  expect_error(
    reshape_projoint(
      exampleData3,
      outcomes,
      .choice_map = c("A" = 1, "B" = 1),
      .repeated = FALSE
    ),
    "profiles 1 and 2"
  )
  expect_error(
    reshape_projoint(
      exampleData3,
      outcomes,
      .choice_labels = c("A ", "B"),
      .repeated = FALSE
    ),
    "whitespace"
  )
})

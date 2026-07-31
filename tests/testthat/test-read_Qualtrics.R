write_qualtrics_fixture <- function(lines) {
  path <- tempfile(fileext = ".csv")
  writeLines(lines, path)
  path
}

test_that("read_Qualtrics reads current exports and preserves column order", {
  path <- write_qualtrics_fixture(c(
    "Q2,Q1,ResponseId",
    "Second choice,First choice,Response ID",
    '"{""ImportId"":""QID2""}","{""ImportId"":""QID1""}","{""ImportId"":""_recordId""}"',
    "Candidate B,Candidate A,R_1",
    "Candidate A,Candidate B,R_2"
  ))

  result <- read_Qualtrics(path)

  expect_named(result, c("Q2", "Q1", "ResponseId"), ignore.order = FALSE)
  expect_equal(nrow(result), 2L)
  expect_equal(result$Q2, c("Candidate B", "Candidate A"))
})

test_that("read_Qualtrics reads legacy exports", {
  path <- write_qualtrics_fixture(c(
    "ResponseId,Q1",
    "Response ID,First choice",
    "R_1,Candidate A",
    "R_2,Candidate B"
  ))

  result <- read_Qualtrics(path)

  expect_equal(nrow(result), 2L)
  expect_equal(result$ResponseId, c("R_1", "R_2"))
})

test_that("read_Qualtrics permits an explicit metadata-row override", {
  path <- write_qualtrics_fixture(c(
    "ResponseId,Q1",
    "R_1,Candidate A",
    "R_2,Candidate B"
  ))

  result <- read_Qualtrics(path, .metadata_rows = 0)

  expect_equal(nrow(result), 2L)
  expect_equal(result$Q1, c("Candidate A", "Candidate B"))
})

test_that("read_Qualtrics rejects invalid inputs", {
  expect_error(read_Qualtrics(character()), "single, non-missing character")
  expect_error(read_Qualtrics(tempfile()), "cannot be found")

  path <- write_qualtrics_fixture(c("Q1", "Question", "A"))
  expect_error(read_Qualtrics(path, .metadata_rows = -1), "non-negative integer")
  expect_error(read_Qualtrics(path, .metadata_rows = 1.5), "non-negative integer")
})

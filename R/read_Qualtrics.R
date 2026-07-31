#' Read and re-format a Qualtrics CSV (choice text)
#'
#' Reads a CSV file exported from Qualtrics (with "Use choice text" enabled) and
#' returns a data frame formatted for downstream processing with
#' \code{\link{reshape_projoint}}.
#'
#' @param .file A character string giving the path to a Qualtrics CSV file.
#' @param .metadata_rows Number of metadata rows after the CSV header. The
#'   default, \code{NULL}, detects current Qualtrics exports (question text plus
#'   an \code{ImportId} row) and legacy exports (question text only). Supply a
#'   non-negative integer to override detection.
#'
#' @return A tibble containing response rows only. Column names and physical
#'   column order are preserved from the Qualtrics export.
#'
#' @seealso \code{\link{reshape_projoint}}
#'
#' @examples
#' \donttest{
#' # Write a tiny dummy Qualtrics-style CSV to a temp file
#' tmp <- tempfile(fileext = ".csv")
#' readr::write_csv(
#'   data.frame(Q1 = c("Choice Text", "{\"ImportId\":\"QID1\"}", "A", "B")),
#'   tmp
#' )
#' # Read it back in
#' df <- read_Qualtrics(tmp)
#' head(df)
#' }
#'
#' @export
read_Qualtrics <- function(.file, .metadata_rows = NULL) {
  if (!is.character(.file) || length(.file) != 1L || is.na(.file)) {
    stop("`.file` must be a single, non-missing character string.", call. = FALSE)
  }

  if (!file.exists(.file)) {
    stop("The specified file cannot be found.", call. = FALSE)
  }

  if (!is.null(.metadata_rows)) {
    valid_metadata_rows <-
      is.numeric(.metadata_rows) &&
      length(.metadata_rows) == 1L &&
      !is.na(.metadata_rows) &&
      .metadata_rows >= 0 &&
      .metadata_rows == as.integer(.metadata_rows)

    if (!valid_metadata_rows) {
      stop("`.metadata_rows` must be NULL or one non-negative integer.", call. = FALSE)
    }
  }

  column_names <- readr::read_csv(
    .file,
    n_max = 0,
    name_repair = "minimal",
    show_col_types = FALSE
  ) |>
    names()

  if (is.null(.metadata_rows)) {
    metadata <- readr::read_csv(
      .file,
      n_max = 2,
      col_types = readr::cols(.default = readr::col_character()),
      name_repair = "minimal",
      show_col_types = FALSE
    )

    second_row <- if (nrow(metadata) >= 2L) {
      unlist(metadata[2, ], use.names = FALSE)
    } else {
      character()
    }

    has_import_id_row <- any(grepl('"ImportId"', second_row, fixed = TRUE), na.rm = TRUE)
    .metadata_rows <- if (has_import_id_row) 2L else 1L
  }

  readr::read_csv(
    .file,
    skip = 1L + as.integer(.metadata_rows),
    col_names = column_names,
    name_repair = "minimal",
    show_col_types = FALSE
  )
}

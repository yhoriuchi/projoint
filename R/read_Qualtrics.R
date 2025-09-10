#' Read and re-format a Qualtrics CSV (choice text)
#'
#' Reads a CSV file exported from Qualtrics (with "Use choice text" enabled) and
#' returns a data frame formatted for downstream processing with
#' \code{\link{reshape_projoint}}.
#'
#' @param .file A character string giving the path to a Qualtrics CSV file.
#'
#' @return A data frame where column names are preserved from the Qualtrics export.
#'   The first two rows of Qualtrics metadata are skipped automatically.
#'
#' @seealso \code{\link{reshape_projoint}}
#'
#' @examples
#' \donttest{
#' # Write a tiny dummy Qualtrics-style CSV to a temp file
#' tmp <- tempfile(fileext = ".csv")
#' readr::write_csv(
#'   data.frame(Q1 = c("Choice Text", "Choice Text", "A", "B")),
#'   tmp
#' )
#' # Read it back in
#' df <- read_Qualtrics(tmp)
#' head(df)
#' }
#'
#' @export
read_Qualtrics <- function(.file) {
  
  if (!file.exists(.file)) {
    stop("Error: The specified file cannot be found.")
  }
  
  colnames <- readr::read_csv(.file, show_col_types = FALSE) |> names()
  
  qualtrics <- readr::read_csv(.file, skip = 2, show_col_types = FALSE)
  names(qualtrics) <- colnames
  
  return(qualtrics)
}

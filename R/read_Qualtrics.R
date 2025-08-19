#' Read and re-format a Qualtrics csv (choice text)
#'
#' @param .file A file name corresponding to a csv downloaded from Qualtrics.
#' @return A data frame suitable for \code{\link{reshape_projoint}}
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

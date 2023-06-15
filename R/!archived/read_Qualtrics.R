#' Reads and reformats a Qualtrics csv.
#'
#' @param .file A filename corresponding to a csv downloaded from Qualtrics.
#' @return A data frame suitable for \texttt{reshape_conjoint}
#' @export



read_Qualtrics <- function(.file){

  if(!file.exists(.file)){
    stop("Error: The specified file cannot be found.")
  }

  colnames <- .file %>%
    readr::read_csv(show_col_types = FALSE) %>%
    names()

  qualtrics <- readr::read_csv(.file, skip = 2, show_col_types = FALSE)
  colnames(qualtrics) <- colnames

  return(qualtrics)

}

#' Read the Qualtrics data
#'
#' Read the CSV file downloaded from Qualtrics
#'
#' @import readr
#' @param .file The name of a Qualtrics data file (in CSV)
#' @return A data frame (wide format) with column names
#' @export
#'
read_Qualtrics <- function(.file){

  colnames <- .file %>%
    read_csv() %>%
    names()

  qualtrics <- read_csv(.file, skip = 2)
  colnames(qualtrics) <- colnames

  return(qualtrics)

}

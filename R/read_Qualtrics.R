#' Reads and lightly processes a CSV file downloaded from Qualtrics
#'
#' This function reads a Qualtrics-produced CSV file and removes the first two rows, preserving headers.
#' The output data frame is in wide format; to convert to long format, see \dQuote{reshape_conjoint()}
#'
#' @import readr
#' @param .file The file name containing Qualtrics responses, preferably ending in ".csv"
#' @return A data frame containing Qualtrics responses, suitable for re.
#' @export
#'
read_Qualtrics <- function(.file){
  
  if(!exists(.file)){
    stop("Error: The specified file cannot be found.")
  }

  colnames <- .file %>%
    read_csv() %>%
    names()
  
  qualtrics <- read_csv(.file, skip = 2)
  colnames(qualtrics) <- colnames

  return(qualtrics)

}

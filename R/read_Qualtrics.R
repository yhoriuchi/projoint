#' Read the Qualtrics data
#' Reads and lightly processes a CSV file downloaded from Qualtrics
#'
#' Read the CSV file downloaded from Qualtrics
#' This function reads a Qualtrics-produced CSV file and removes the first two rows, preserving headers.
#' The output data frame is in wide format; to convert to long format, see reshape_conjoint()
#'
#' @import readr
#' @param .file The name of a Qualtrics data file (in CSV)
#' @return A data frame (wide format) with column names
#' @param .file The file name containing Qualtrics responses, preferably ending in ".csv"
#' @return A data frame containing Qualtrics responses.
#' @export
#'

read_Qualtrics <- function(.file){

  if(!file.exists(.file)){
    stop("Error: The specified file cannot be found.")
  }

  colnames <- .file %>%
    readr::read_csv() %>%
    names()

  qualtrics <- readr::read_csv(.file, skip = 2)
  colnames(qualtrics) <- colnames

  return(qualtrics)

}

#' Read the Qualtrics data
#'
#' Read the CSV file downloaded from Qualtrics
#'
#' @import readr
#' @param .file The name of a Qualtrics data file (in CSV)
#' @param .folder The name of a sub-folder relative to the working directory
#' @return A data frame (wide format) with column names
#' @export
#'
read_Qualtrics <- function(.file, .folder = "data/"){

  colnames <- paste0(.folder, "/", .file) %>%
    read_csv() %>%
    names()

  qualtrics <- read_csv(paste0(.folder, "/", .file), skip = 2)
  colnames(qualtrics) <- colnames
  colnames(qualtrics) <- colnames

  return(qualtrics)

}

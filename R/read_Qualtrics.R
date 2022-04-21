read_Qualtrics <- function(.file){

  # if(!exists(.file)){
  #   stop("Error: The specified file cannot be found.")
  # }

  colnames <- .file %>%
    readr::read_csv() %>%
    names()

  qualtrics <- readr::read_csv(.file, skip = 2)
  colnames(qualtrics) <- colnames

  return(qualtrics)

}

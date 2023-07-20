#' Read and re-format a Qualtrics csv (choice text)
#'
#' @param .file A file name corresponding to a csv downloaded from Qualtrics.
#' @return A data frame suitable for reshape_conjoint()
#' @export
#' @examples
#' library(projoint)
#' 
#' # Not run:
#' # dat <- read_Qualtrics("mummolo_nall_replication_cleaned.csv")
#' # head(dat)

read_Qualtrics <- function(
    .file
){
  
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

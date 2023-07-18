#' Set the quantities of interest
#'
#' This function ...
#'
#' @import rlang
#' @import stringr
#' @param .structure either "choice_level" or "profile_level
#' @param .attribute The attribute of interest
#' @param .level The level/s of interest: either a character or a character vector with the length of 2
#' @return A `projoint_structure` object
#' @export
#' 
#' 

set_qoi <- function(
    .structure = "choice_level",
    .attribute, 
    .level
){
  
  structure  <- rlang::arg_match0(.structure, c("choice_level", "profile_level"))
  
  # specify the attributes and levels of interest
  att_levels <- stringr::str_c(.attribute, ":", .level)
  
  n_levels <- length(att_levels)
  
  if (structure == "profile_level" & n_levels == 2){
    
    stop("Specify 1 level for profile-level analysis")
    
  } else if (structure == "choice_level" & n_levels != 2){
    
    stop("Specify 2 levels for choice-level analysis")
    
  }
  
  out <- projoint_qoi("attribute_of_interest" = .attribute,
                      "levels_of_interest" = .level)
  
  return(out)
  
}
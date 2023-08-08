#' Set the quantities of interest
#'
#' This function allows users to specify more fine-tuned details of their conjoint analysis. In particular, users can set very specific quantities of interest aside from simple AMCEs or MMs.
#'
#' @import rlang
#' @import stringr
#' @param .structure Either "profile_level" or "choice_level"
#' @param .estimand Either "mm" for marginal mean or "amce" for average marginal component effect
#' @param .attribute A character column name identifying the attribute of interest
#' @param .level  A character vector identifying the levels of interest. Its length should be 1 for profile-level analysis and 2 for choice-level analysis
#' @param .baseline A character vector identifying the baseline level for AMCE. Its length should be 1 for profile-level analysis and 2 for choice-level analysis
#' @return A `projoint_qoi` object
#' @export

set_qoi <- function(
    .structure = "profile_level",
    .estimand = "mm",
    .attribute, 
    .level,
    .baseline = NULL
){
  
  structure  <- rlang::arg_match0(.structure, c("choice_level", "profile_level"))
  estimand  <- rlang::arg_match0(.estimand, c("mm", "amce"))
  
  # specify the attributes and levels of interest
  att_levels          <- stringr::str_c(.attribute, ":", .level)

  n_levels          <- length(att_levels)
  
  if (structure == "profile_level" & n_levels == 2){
    stop("Specify 1 level for profile-level analysis")
  } else if (structure == "choice_level" & n_levels != 2){
    stop("Specify 2 levels for choice-level analysis")
  }
  
  if (estimand == "mm"){
    
    out <- projoint_qoi_mm("attribute_of_interest" = .attribute,
                           "levels_of_interest" = .level)
    
  } 
  
  if (estimand == "amce"){

    # specify the attributes and levels of interest
    att_levels_baseline <- stringr::str_c(.attribute, ":", .baseline)
    
    n_levels_baseline <- length(att_levels_baseline)
    
    if (structure == "profile_level" & n_levels_baseline == 2){
      
      stop("Specify 1 level for profile-level analysis")
      
    } else if (structure == "choice_level" & n_levels_baseline != 2){
      
      stop("Specify 2 levels for choice-level analysis")
      
    }
    
    out <- projoint_qoi_amce("attribute_of_interest" = .attribute,
                             "levels_of_interest" = .level,
                             "baseline" = .baseline)
    
  }
  
  
  return(out)
  
}
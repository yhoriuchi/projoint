#' Analyze a conjoint data set and correct for measurement error
#'
#' This function ...
#'
#' @import dplyr
#' @import rlang
#' @importFrom MASS mvrnorm
#' @param .data A `projoint_data` object
#' @param .qoi A `projoint_qoi` object. If NULL, defaults to producing all MMs and all AMCEs.
#' @param .irr NULL (default) if IRR is to be calculated using the repeated task. Otherwise, a numerical value
#' @param .ignore_position TRUE (default) if you ignore the location of profile (left or right. Relevant only if analyzed at the choice level
#' @param .se_method c("analytic", "simulation", "bootstrap") description
#' @param .n_sims The number of simulations. Relevant only if .se_method == "simulation" 
#' @param .n_boot The number of bootstrapped samples. Relevant only if .se_method == "bootstrap"
#' @return A `projoint_results` object
#' @export
#' 
#' 

projoint <- function(
    .data,
    .qoi = NULL,
    .structure = "profile_level",
    .estimand = "mm",
    .se_method = "analytical",
    .irr = NULL,
    .remove_ties = TRUE,
    .repeated_task = TRUE,
    .ignore_position = NULL,
    .n_sims = NULL,
    .n_boot = NULL
){
  
  # bind variables locally to the function ----------------------------------
  
  # To be added
  
  # check various settings --------------------------------------------------
  # also see: many checks in pj_estimate()
  
  if(class(.data) != "projoint_data"){
    stop("The .data argument must be of class `projoint_data` from the `reshape_projoint` function.")
  }
  
  if(!is.null(.qoi) & class(.qoi) != "projoint_qoi"){
    stop("The .qoi argument must be of class `projoint_qoi` from the `set_qoi` function.")
  }
  
  # estimate all MMs or AMCEs -----------------------------------------------
  
  if (is.null(.qoi)){
    
    attribute_levels <- .data@labels$level_id
    
    out <- NULL
    
    for (i in seq_along(attribute_levels)){
      
      attribute <- str_extract(attribute_levels[i], "^.+(?=:)")
      level     <- str_extract(attribute_levels[i], "(?<=:).+$")
      
      temp <- pj_estimate(.data,
                          attribute, # note: this is NOT .attribute
                          level, # note: this is NOT .level
                          .structure,
                          .estimand,
                          .se_method,
                          .irr,
                          .remove_ties,
                          .repeated_task,
                          .ignore_position,
                          .n_sims,
                          .n_boot) %>% 
        mutate(attribute = attribute, 
               level = level)
      
      out <- bind_rows(out, temp)
      
    }
    
  } else{
    
    attribute <- .qoi@attribute_of_interest
    level <- .qoi@levels_of_interest
    
    out <- pj_estimate(.data,
                       attribute, # note: this is NOT .attribute
                       level, # note: this is NOT .level
                       .structure,
                       .estimand,
                       .se_method,
                       .irr,
                       .remove_ties,
                       .repeated_task,
                       .ignore_position,
                       .n_sims,
                       .n_boot) %>% 
      mutate(attribute = attribute, 
             level = level)
    
  }
  
  # return(out)
  tau <- unique(out$tau)
  estimates <- out %>% dplyr::select(-tau)
  
  # slots inherited from projoint_data and projoint_qoi are NULL. Why?
  projoint_results("tau" = tau,
                   "estimate" = estimates) %>% 
    return()
  
  
  
}

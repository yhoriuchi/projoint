#' Analyze a conjoint data set and correct for measurement error
#'
#' This function ...
#'
#' @import dplyr
#' @import rlang
#' @importFrom MASS mvrnorm
#' @param .data A `projoint.data` object
#' @param .structure A `projoint.structure` object. If missing, defaults to producing all MMs and all AMCEs.
#' @param .irr NULL (default) if IRR is to be calculated using the repeated task. Otherwise, a numerical value
#' @param .ignore_position TRUE (default) if you ignore the location of profile (left or right. Relevant only if analyzed at the choice level
#' @param .se_method c("analytic", "simulation", "bootstrap") description
#' @param .n_sims The number of simulations. Relevant only if .se_method == "simulation" 
#' @param .n_boot The number of bootstrapped samples. Relevant only if .se_method == "bootstrap"
#' @return A `projoint.results` object
#' @export
#' 
#' 

pj_estimate <- function(
    .data,
    .structure,
    .irr,
    .se_method = "analytic",
    .ignore_position = TRUE,
    .n_sims = NULL,
    .n_boot = NULL
){
  
  # Type check the inputs
  if(class(.data) != "projoint.data"){
    stop("The .data argument must be of class `projoint.data` from the `reshape_projoint` function.")
  }
  if(class(.structure) != "projoint.structure"){
    stop("The .structure argument must be of class `projoint.structure` from the `set_qoi` function.")
  }
  if(!is.null(.irr) & (!is.numeric(.irr) & length(.irr)==1){
    stop("The .irr argument must be either a numeric scalar or NULL.")
  }
  if(!.se_method %in% c("analytic", "bootstrap", "simulation")){
    stop("The .se_method argument must be one of: analytic, bootstrap, simulation.")
  }
  if(!is.logical(.igore_position)){
    stop("The .ignore_position argument must be either TRUE or FALSE.")
  }
  if(!is.null(.n_sims) & (!is.numeric(.n_sims) & length(.n_sims)==1){
    stop("The .n_sims argument must be either a numeric scalar or NULL.")
  }
  if(!is.null(.n_boot) & (!is.numeric(.n_boot) & length(.n_boot)==1){
    stop("The .n_boot argument must be either a numeric scalar or NULL.")
  }
  
  # do some work ------------------------------------------------------------
  
  # return the output -------------------------------------------------------
  
  output <- projoint.results(projoint.data = .data,
                             projoint.structure = .structure,
                             irr = irr,
                             mm = mm,
                             amce = amce)
  
  return(output)
  
} 


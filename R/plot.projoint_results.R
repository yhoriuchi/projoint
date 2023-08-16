#' Plot all MMs or AMCEs
#'
#' This method produces MM or AMCE plots given a \code{\link{projoint_results}} object, the output from the \code{\link{projoint}} function.
#'
#' @import ggplot2
#' @import ggthemes
#' @import dplyr
#' @import stringr
#' @param x A \code{\link{projoint_results}} object
#' @param .estimates The estimates to be plotted, either \code{"corrected"} (default), \code{"uncorrected"}, or \code{"both"}
#' @param .by_var (only if the structure is profile-level) \code{TRUE} to plot the difference in estimates between the two subgroups, \code{FALSE} (default) otherwise 
#' @param .labels (only if the structure is choice-level) A chaarcter vector for the x-axis labels
#' @param .base_size base font size, given in pts.
#' @param .base_family base font family
#' @param ... Additional optional arguments
#' @return A \code{ggplot} object
#' @export

plot.projoint_results <- function(
    x, 
    .estimates = "corrected",
    .by_var = FALSE,
    .labels = NULL,
    .base_size = 12,
    .base_family = "",
    ...) {
  
  if(!is(x, "projoint_results")){
    stop("The x argument must be of class `projoint_results` from the `projoint` function.")
  }
  
  .estimand <- x@estimand
  .structure <- x@structure 
  
  if (.structure == "profile_level"){
    
    out <- plot_projoint_profile_level(x, .estimates, .by_var, .base_size, .base_family, ...)
    
  } else if (.structure == "choice_level" & .estimand == "mm"){
    
    out <- plot_projoint_choice_level_mm(x, .estimates, .labels, .base_size, .base_family, ...)
    
  } else{
    
    stop("The current verion does not support plot() for the results of choice-level AMCEs. Stay tuned!")
  }
  
  out
}

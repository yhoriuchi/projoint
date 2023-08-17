#' Analyze a conjoint data set and correct for measurement error
#'
#' This main function analyzes a conjoint data set and produces measurement error-corrected estimates of either marginal means or average marginal component effects, ready for plotting.
#' It  accepts a `projoint_data` object, and optionally a `projoint_qoi` object for users who wish to specify more complex quantities of interest.
#'
#' @import dplyr
#' @import rlang
#' @import estimatr
#' @importFrom MASS mvrnorm
#' @importFrom methods is
#' @importFrom methods new
#' @param .data A \code{\link{projoint_data}} object
#' @param .qoi A \code{\link{projoint_qoi}} object. If \code{NULL}, defaults to producing all MMs and all AMCEs.
#' @param .by_var A dichotomous variable (character) used for subgroup analysis
#' @param .structure Either \code{"profile_level"} (default) or \code{"choice_level"}. If \code{.qoi} is set, the value of \code{structure} from \code{.qoi} overrides this value.
#' @param .estimand Either \code{"mm"} for marginal mean or \code{"amce"} for average marginal component effect. If \code{.qoi} is set, the value of \code{estimand} from \code{.qoi} overrides this value.
#' @param .se_method By default, \code{c("analytic", "simulation", "bootstrap")} description
#' @param .irr \code{NULL} (default) if IRR is to be calculated using the repeated task. Otherwise, a numerical value
#' @param .remove_ties Logical: should ties be removed before estimation? Defaults to \code{TRUE}.
#' @param .ignore_position NULL (default) if \code{.structure = "choice_level"}. Set to TRUE if you ignore the position of profile (left or right); FALSE if the relative positioning of profiles matters for analysis. If  \code{.structure = "choice_level"} and this argument is \code{NULL}, it is automatically reset to \code{TRUE}.
#' @param .n_sims The number of simulations. Relevant only if \code{.se_method == "simulation"} 
#' @param .n_boot The number of bootstrapped samples. Relevant only if \code{.se_method == "bootstrap"}
#' @param .weights_1 the weight to estimate IRR (see \code{\link[estimatr]{lm_robust}}): \code{NULL} (default)
#' @param .clusters_1 the clusters to estimate IRR (see \code{\link[estimatr]{lm_robust}}): \code{NULL} (default)
#' @param .se_type_1 the standard error type to estimate IRR (see \code{\link[estimatr]{lm_robust}}): \code{"classical"} (default)
#' @param .weights_2 the weight to estimate MM or AMCE (see \code{\link[estimatr]{lm_robust}}): \code{NULL} (default)
#' @param .clusters_2 the clusters to estimate MM or AMCE (see \code{\link[estimatr]{lm_robust}}): \code{NULL} (default)
#' @param .se_type_2 the standard error type to estimate MM or AMCE (see \code{\link[estimatr]{lm_robust}}): \code{"classical"} (default)
#' @return A \code{\link{projoint_results}} object
#' @export
#' @examples
#' 
#' library(projoint)
#' 
#' data("exampleData1")
#' head(exampleData1)
#'
#' outcomes <- paste0("choice", seq(from = 1, to = 8, by = 1))
#' outcomes <- c(outcomes, "choice1_repeated_flipped")
#' 
#' reshaped_data <- reshape_projoint(
#'   .dataframe = exampleData1, 
#'   .outcomes = outcomes)
#'
#' projoint(reshaped_data) 

projoint <- function(
    .data,
    .qoi = NULL,
    .by_var = NULL,
    .structure = "profile_level",
    .estimand = "mm",
    .se_method = "analytical",
    .irr = NULL,
    .remove_ties = TRUE,
    .ignore_position = NULL,
    .n_sims = NULL,
    .n_boot = NULL,
    .weights_1 = NULL,
    .clusters_1 = NULL,
    .se_type_1 = "classical",
    .weights_2 = NULL,
    .clusters_2 = NULL,
    .se_type_2 = "classical"
){
  
  if(!is.null(.structure) & !is.null(.qoi)){
    .structure = .qoi@structure
    warning("Both .qoi and .structure are specified; using the value from .qoi.")
  }

  if(!is.null(.estimand) & !is.null(.qoi)){
    .estimand = .qoi@estimand
    warning("Both .qoi and .estimand are specified; using the value from .qoi.")
  }
  
  if(.structure == "choice_level" & is.null(.ignore_position)){
    .ignore_position = TRUE
  }
  
  if (is.null(.by_var)){
    
    projoint_level(.data,
                   .qoi,
                   .structure,
                   .estimand,
                   .se_method,
                   .irr,
                   .remove_ties,
                   .ignore_position,
                   .n_sims,
                   .n_boot,
                   .weights_1,
                   .clusters_1,
                   .se_type_1,
                   .weights_2,
                   .clusters_2,
                   .se_type_2)
    
  } else{
    
    projoint_diff(.data,
                  .qoi,
                  .by_var,
                  .structure,
                  .estimand,
                  .se_method,
                  .irr,
                  .remove_ties,
                  .ignore_position,
                  .n_sims,
                  .n_boot,
                  .weights_1,
                  .clusters_1,
                  .se_type_1,
                  .weights_2,
                  .clusters_2,
                  .se_type_2)
    
  }
  
}

#' @param x A \code{\link{projoint_results}} object
#' @param ... Optional arguments; currently none accepted
#' @export
#' @rdname projoint

print.projoint_results <- function(x, ...) {
  cat("[A projoint output]\n", 
      "Estimand:", x@estimand, "\n",
      "Structure:", x@structure, "\n",
      "IRR:", x@irr, "\n",
      "Tau:", x@tau, "\n",
      "Remove ties:", x@remove_ties, "\n",
      "SE methods:", x@se_method)
}

#' @param object A \code{\link{projoint_results}} object
#' @param ... Optional arguments; currently none accepted
#' @export
#' @rdname projoint

summary.projoint_results <- function(object, ...) {
  object@estimates
}




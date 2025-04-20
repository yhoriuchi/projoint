#' Analyze a Conjoint Data Set with Measurement Error Correction
#'
#' This function analyzes a conjoint data set and produces corrected estimates
#' of either marginal means (MMs) or average marginal component effects (AMCEs),
#' ready for plotting. It corrects for intra-respondent reliability (IRR) bias
#' and accounts for repeated tasks if available.
#'
#' Most users simply provide a \code{\link{projoint_data}} object.
#' Advanced users can also specify quantities of interest using \code{\link{projoint_qoi}}.
#'
#' @import dplyr
#' @import rlang
#' @import estimatr
#' @importFrom MASS mvrnorm
#' @importFrom methods is
#'
#' @param .data A \code{\link{projoint_data}} object created by \code{reshape_projoint()} or \code{make_projoint_data()}.
#' @param .qoi Optional. A \code{\link{projoint_qoi}} object if targeting custom quantities of interest.
#' @param .by_var Character. Optional. A dichotomous variable for subgroup analysis (e.g., Republican vs Democrat).
#' Only available for **profile-level** analysis. Ignored for choice-level analysis.
#' @param .structure Character. Either \code{"profile_level"} (default) or \code{"choice_level"}.
#' If \code{.qoi} is supplied, the structure specified there overrides this.
#' @param .estimand Character. Either \code{"mm"} (marginal mean) or \code{"amce"} (average marginal component effect).
#' If \code{.qoi} is supplied, the estimand specified there overrides this.
#' @param .se_method Character. Standard error method: \code{"analytical"} (default), \code{"simulation"}, or \code{"bootstrap"}.
#' @param .irr Numeric or \code{NULL} (default). If \code{NULL}, IRR is estimated from repeated tasks; otherwise, user-supplied IRR.
#' @param .remove_ties Logical. Should ties in choice data be removed before estimation? Defaults to \code{TRUE}.
#' @param .ignore_position Logical. Only for choice-level analysis. Should the position of profiles (left/right) be ignored? Default is \code{TRUE}.
#' @param .n_sims Integer. Required if \code{.se_method = "simulation"}. Number of simulation draws.
#' @param .n_boot Integer. Required if \code{.se_method = "bootstrap"}. Number of bootstrap samples.
#' @param .weights_1, .clusters_1, .se_type_1 Arguments passed to \code{\link[estimatr]{lm_robust}} when estimating IRR.
#' @param .weights_2, .clusters_2, .se_type_2 Arguments passed to \code{\link[estimatr]{lm_robust}} when estimating MMs or AMCEs.
#' @param .clusters_1 Cluster ID variable for IRR estimation. Passed to \code{lm_robust()}. Default is \code{NULL}.
#' @param .se_type_1 Standard error type for IRR estimation. Passed to \code{lm_robust()}. Default is \code{"classical"}.
#' @param .clusters_2 Cluster ID variable for MM/AMCE estimation. Passed to \code{lm_robust()}. Default is \code{NULL}.
#' @param .se_type_2 Standard error type for MM/AMCE estimation. Passed to \code{lm_robust()}. Default is \code{"classical"}.
#'
#' @return A \code{\link{projoint_results}} object containing estimated quantities, ready for plotting or further analysis.
#'
#' @seealso \code{\link{reshape_projoint}}, \code{\link{projoint_qoi}}, \code{\link{plot.projoint_results}}
#' @export
projoint <- function(
    .data,
    .qoi = NULL,
    .by_var = NULL,
    .structure = "choice_level",
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
) {
  
  if (!is.null(.structure) & !is.null(.qoi)) {
    .structure <- .qoi$structure
  }
  if (!is.null(.estimand) & !is.null(.qoi)) {
    .estimand <- .qoi$estimand
  }
  if (.structure == "choice_level" & is.null(.ignore_position)) {
    .ignore_position <- TRUE
  }
  
  if (is.null(.by_var)) {
    out <- projoint_level(
      .data, .qoi, .structure, .estimand, .se_method, .irr, .remove_ties,
      .ignore_position, .n_sims, .n_boot,
      .weights_1, .clusters_1, .se_type_1,
      .weights_2, .clusters_2, .se_type_2
    )
    
  } else {
    out <- projoint_diff(
      .data, .qoi, .by_var, .structure, .estimand, .se_method, .irr, .remove_ties,
      .ignore_position, .n_sims, .n_boot,
      .weights_1, .clusters_1, .se_type_1,
      .weights_2, .clusters_2, .se_type_2
    )
  }
  
  return(out)
}

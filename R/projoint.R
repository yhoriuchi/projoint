#' Analyze a Conjoint Data Set with Measurement Error Correction
#'
#' Computes marginal means (MMs) or average marginal component effects (AMCEs)
#' with correction for intra-respondent reliability (IRR), using repeated tasks
#' when available. Returns estimates ready for plotting.
#'
#' Most users provide a \code{\link{projoint_data}} object. Advanced users may
#' target custom quantities with \code{\link{projoint_qoi}}.
#'
#' @import dplyr
#' @import rlang
#' @import estimatr
#' @importFrom MASS mvrnorm
#' @importFrom methods is
#'
#' @param .data A \code{\link{projoint_data}} created by \code{\link{reshape_projoint}} or \code{\link{make_projoint_data}}.
#' @param .qoi Optional \code{\link{projoint_qoi}} specifying custom quantities of interest.
#'   If supplied, its \code{structure} and \code{estimand} override \code{.structure} and \code{.estimand}.
#' @param .by_var Optional column name (character) for subgroup analysis; must be
#'   logical (TRUE/FALSE) or numeric/integer coded as 0/1. Only supported for
#'   \code{.structure = "profile_level"} (ignored for choice-level).
#' @param .structure Either \code{"profile_level"} or \code{"choice_level"} (default \code{"choice_level"}).
#'   If \code{.qoi} is supplied, this is overridden by \code{.qoi$structure}.
#' @param .estimand Either \code{"mm"} (marginal mean) or \code{"amce"} (average marginal component effect);
#'   default \code{"mm"}. If \code{.qoi} is supplied, this is overridden by \code{.qoi$estimand}.
#' @param .se_method Standard-error method: \code{"analytical"} (default), \code{"simulation"}, or \code{"bootstrap"}.
#' @param .irr Numeric or \code{NULL} (default). If \code{NULL}, IRR is estimated from repeated tasks; otherwise IRR is fixed to the supplied value.
#' @param .remove_ties Logical; remove ties in choice data before estimation? Default \code{TRUE}.
#' @param .ignore_position Logical; for choice-level only. Ignore profile position (left/right)? Default \code{TRUE}.
#' @param .n_sims Integer; required when \code{.se_method = "simulation"}.
#' @param .n_boot Integer; required when \code{.se_method = "bootstrap"}.
#' @param .weights_1,.clusters_1,.se_type_1 Arguments passed to \code{\link[estimatr]{lm_robust}} for IRR estimation.
#'   If \code{.se_type_1} is \code{NULL}, \emph{estimatr} defaults are used (HC2 when unclustered; CR2 when clustered).
#' @param .weights_2,.clusters_2,.se_type_2 Arguments passed to \code{\link[estimatr]{lm_robust}} for MM/AMCE estimation.
#'   If \code{.se_type_2} is \code{NULL}, \emph{estimatr} defaults are used (HC2 when unclustered; CR2 when clustered).
#' @param .auto_cluster Logical. If \code{TRUE} (default), automatically cluster on an \code{id}
#'   column when present and no \code{.clusters_*} are supplied. Auto-clustering only
#'   occurs when the corresponding \code{.se_type_*} is \code{NULL}.
#'
#' @details
#' Valid \code{se_type_*} values depend on whether clustering is used:
#' \itemize{
#'   \item Without clusters: \code{"classical"}, \code{"HC0"}, \code{"HC1"}, \code{"HC2"}, \code{"HC3"}
#'   \item With clusters: \code{"CR0"}, \code{"CR1"}, \code{"CR2"}, \code{"stata"}, \code{"none"}
#' }
#' If \code{NULL}, \emph{estimatr} defaults are used (\code{HC2} when unclustered; \code{CR2} when clustered).
#'
#' @return A \code{\link{projoint_results}} object with estimates and metadata, ready for plotting or further analysis.
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
    .se_type_1 = NULL,
    .weights_2 = NULL,
    .clusters_2 = NULL,
    .se_type_2 = NULL,
    .auto_cluster = TRUE
) {
  
  if (!is.null(.structure) && !is.null(.qoi)) {
    .structure <- .qoi$structure
  }
  if (!is.null(.estimand) && !is.null(.qoi)) {
    .estimand <- .qoi$estimand
  }
  if (.structure == "choice_level" && is.null(.ignore_position)) {
    .ignore_position <- TRUE
  }

  if (!is.null(.by_var) && .structure != "profile_level") {
    stop(".by_var is only supported for profile-level analysis.")
  }
  
  weights1_quo  <- rlang::enquo(.weights_1)
  clusters1_quo <- rlang::enquo(.clusters_1)
  weights2_quo  <- rlang::enquo(.weights_2)
  clusters2_quo <- rlang::enquo(.clusters_2)
  
  if (is.null(.by_var)) {
    out <- projoint_level(
      .data = .data, 
      .qoi = .qoi, 
      .structure = .structure, 
      .estimand = .estimand, 
      .se_method = .se_method, 
      .irr = .irr, 
      .remove_ties = .remove_ties,
      .ignore_position = .ignore_position, 
      .n_sims = .n_sims, 
      .n_boot = .n_boot,
      .weights_1 = weights1_quo, 
      .clusters_1 = clusters1_quo, 
      .se_type_1 = .se_type_1,
      .weights_2 = weights2_quo, 
      .clusters_2 = clusters2_quo, 
      .se_type_2 = .se_type_2,
      .auto_cluster = .auto_cluster
    )
    
  } else {
    out <- projoint_diff(
      .data = .data, 
      .qoi = .qoi, 
      .by_var = .by_var, 
      .structure = .structure, 
      .estimand = .estimand, 
      .se_method = .se_method, 
      .irr = .irr, 
      .remove_ties = .remove_ties,
      .ignore_position = .ignore_position, 
      .n_sims = .n_sims, 
      .n_boot = .n_boot,
      .weights_1 = weights1_quo, 
      .clusters_1 = clusters1_quo, 
      .se_type_1 = .se_type_1,
      .weights_2 = weights2_quo, 
      .clusters_2 = clusters2_quo, 
      .se_type_2 = .se_type_2,
      .auto_cluster = .auto_cluster
    )
  }
  
  return(out)
}

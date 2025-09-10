#' Analyze a conjoint dataset with measurement-error correction
#'
#' Computes marginal means (MMs) or average marginal component effects (AMCEs)
#' with correction for intra-respondent reliability (IRR, \eqn{\tau}). When a
#' repeated task is present, IRR is estimated unless a fixed value is supplied.
#' Results are returned in a structured object ready for plotting and summary.
#'
#' Most users will pass a \code{\link{projoint_data}} object (from
#' \code{\link{reshape_projoint}} or \code{\link{make_projoint_data}}). Advanced
#' users may specify custom quantities via \code{\link{projoint_qoi}}; if provided,
#' its \code{structure} and \code{estimand} override \code{.structure} and
#' \code{.estimand}.
#'
#' @import dplyr
#' @import rlang
#' @import estimatr
#' @importFrom MASS mvrnorm
#' @importFrom methods is
#'
#' @param .data A \code{\link{projoint_data}} created by
#'   \code{\link{reshape_projoint}} or \code{\link{make_projoint_data}}.
#' @param .qoi Optional \code{\link{projoint_qoi}} describing the quantity of
#'   interest. If supplied, its fields override \code{.structure} and \code{.estimand}.
#' @param .by_var Optional column name (character) for subgroup analysis; must be
#'   logical (TRUE/FALSE) or numeric/integer coded as 0/1. Only supported for
#'   \code{.structure == "profile_level"} (ignored otherwise).
#' @param .structure Either \code{"profile_level"} or \code{"choice_level"}
#'   (default \code{"choice_level"}). Overridden by \code{.qoi$structure} if present.
#' @param .estimand Either \code{"mm"} (marginal mean) or \code{"amce"} (average
#'   marginal component effect). Default \code{"mm"}. Overridden by \code{.qoi$estimand} if present.
#' @param .se_method Standard-error method: \code{"analytical"} (default),
#'   \code{"simulation"}, or \code{"bootstrap"}.
#' @param .irr Numeric or \code{NULL}. If \code{NULL} (default), IRR is estimated
#'   (when design allows); otherwise a fixed IRR value is used and IRR estimation
#'   is skipped.
#' @param .remove_ties Logical; remove ties in choice data before estimation?
#'   Default \code{TRUE}.
#' @param .ignore_position Logical; choice-level only. Ignore profile position
#'   (left/right)? Default \code{TRUE}.
#' @param .n_sims Integer; required when \code{.se_method = "simulation"}.
#' @param .n_boot Integer; required when \code{.se_method = "bootstrap"}.
#' @param .weights_1,.clusters_1,.se_type_1 Passed to
#'   \code{\link[estimatr]{lm_robust}} when estimating IRR.
#' @param .weights_2,.clusters_2,.se_type_2 Passed to
#'   \code{\link[estimatr]{lm_robust}} when estimating MMs/AMCEs.
#' @param .auto_cluster Logical. If \code{TRUE} (default), automatically cluster on
#'   an \code{id} column when present and no \code{.clusters_*} are supplied. Auto-clustering only
#'   occurs when the corresponding \code{.se_type_*} is \code{NULL}.
#' @param .seed Optional integer. Sets a temporary RNG seed for reproducible
#'   simulation/bootstrap inside the call; restores the previous RNG state on exit.
#'
#' @details
#' Valid \code{se_type_*} values depend on clustering:
#' \itemize{
#'   \item Without clusters: \code{"classical"}, \code{"HC0"}, \code{"HC1"}, \code{"HC2"}, \code{"HC3"}
#'   \item With clusters: \code{"CR0"}, \code{"CR1"}, \code{"CR2"}, \code{"stata"}, \code{"none"}
#' }
#' If \code{NULL}, \emph{estimatr} defaults are used (\code{HC2} when unclustered;
#' \code{CR2} when clustered).
#'
#' @return A \code{projoint_results} object (list-like) with components such as:
#' \itemize{
#'   \item \code{$estimand}, \code{$structure}, \code{$se_method}, \code{$irr}, \code{$tau}
#'   \item \code{$labels}: attribute/level mapping used in estimation
#'   \item \code{$estimates}: a data frame of estimates with columns like
#'         \code{att_level_choose}, \code{att_level_notchoose} (if choice-level),
#'         \code{estimate}, \code{se} / \code{std.error}, \code{conf.low}, \code{conf.high},
#'         and an \code{estimand} label such as \code{"mm_corrected"} or \code{"amce_uncorrected"}.
#' }
#' This object is suitable for downstream use in \code{\link{plot.projoint_results}},
#' \code{\link{summary.projoint_results}}, and related helpers.
#'
#' @seealso \code{\link{reshape_projoint}}, \code{\link{projoint_qoi}},
#'   \code{\link{plot.projoint_results}}, \code{\link{summary.projoint_results}}
#'
#' @examples
#' \donttest{
#' # Prepare example data
#' data(exampleData1)
#' outcomes <- c(paste0("choice", 1:8), "choice1_repeated_flipped")
#' pj <- reshape_projoint(exampleData1, outcomes)
#'
#' # Choice-level QoI based on pj$labels
#' att <- unique(pj$labels$attribute_id)[1]
#' lev_ids   <- pj$labels$level_id[pj$labels$attribute_id == att]
#' lev_names <- sub(".*:", "", lev_ids)
#'
#' q <- set_qoi(
#'     .structure     = "choice_level",
#'     .estimand      = "mm",
#'     .att_choose    = att,
#'     .lev_choose    = lev_names[2],
#'     .att_notchoose = att,
#'     .lev_notchoose = lev_names[1]
#'   )
#'   
#' # Choice-level, marginal means (fast example: fix IRR)
#' fit_choice <- projoint(
#'   pj,
#'   .qoi = q,
#'   .structure = "choice_level",
#'   .estimand  = "mm",
#'   .irr       = 0.80,        # skip IRR estimation for a quick example
#'   .se_method = "analytical"
#' )
#' head(summary(fit_choice))
#'
#' # Profile-level AMCEs
#' fit_profile <- projoint(
#'   pj,
#'   .structure = "profile_level",
#'   .estimand  = "amce",
#'   .se_method = "analytical"
#' )
#' # Plot using the S3 plot method
#' p <- plot(fit_profile, .estimates = "both")
#' print(p)
#' }
#'
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
    .auto_cluster = TRUE,
    .seed = NULL
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
  
  # ---- Optional reproducible RNG (wrapper) -----------------------------------
  if (!is.null(.seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      old_seed <- .Random.seed
      on.exit({
        if (exists("old_seed", inherits = FALSE)) .Random.seed <<- old_seed
      }, add = TRUE)
    }
    set.seed(.seed)
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

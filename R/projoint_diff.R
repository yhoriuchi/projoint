#' Estimate subgroup differences (internal)
#'
#' Worker used by \code{\link{projoint}} to compute subgroup differences
#' (group \code{== 1} minus group \code{== 0}) in marginal means (MMs) or
#' average marginal component effects (AMCEs). Supported only for
#' \code{.structure = "profile_level"}.
#'
#' @param .data A \code{\link{projoint_data}} object.
#' @param .qoi Optional \code{\link{projoint_qoi}}; if \code{NULL}, estimates all MMs/AMCEs.
#' @param .by_var Column name in \code{.data$data} defining subgroups; must be logical,
#'   numeric/integer coded as 0/1, or factor with levels \code{"0"}/\code{"1"}.
#' @param .structure Must be \code{"profile_level"}.
#' @param .estimand Either \code{"mm"} or \code{"amce"}.
#' @param .se_method One of \code{"analytical"}, \code{"simulation"}, or \code{"bootstrap"}.
#' @param .irr \code{NULL} to estimate IRR from repeated tasks, or numeric to fix IRR.
#' @param .remove_ties Logical; drop ties before estimation? Default \code{TRUE}.
#' @param .ignore_position Ignored (subgroup analysis is profile-level only).
#' @param .n_sims Integer; required when \code{.se_method = "simulation"}.
#' @param .n_boot Integer; required when \code{.se_method = "bootstrap"}.
#' @param .weights_1,.clusters_1,.se_type_1 Passed to \code{\link[estimatr]{lm_robust}} for IRR estimation.
#' @param .weights_2,.clusters_2,.se_type_2 Passed to \code{\link[estimatr]{lm_robust}} for MM/AMCE estimation.
#' @param .auto_cluster Logical; if \code{TRUE} (default), auto-cluster on \code{id} when suitable
#'   and no clusters are provided (applies only if the corresponding \code{.se_type_*} is \code{NULL}).
#' @param .seed Optional integer; sets a temporary RNG seed and restores prior state on exit.
#'
#' @return A \code{\link{projoint_results}} object containing subgroup differences with fields:
#' \itemize{
#'   \item \code{estimates}: tibble with one row per attribute/level and columns
#'         like \code{estimate} (group1 minus group0), \code{se}, \code{conf.low},
#'         \code{conf.high}, plus internal columns (\code{estimate_1}, \code{estimate_0},
#'         \code{se_1}, \code{se_0}) used in the diff.
#'   \item \code{estimand}: \code{"mm"} or \code{"amce"}.
#'   \item \code{structure}: \code{"profile_level"}.
#'   \item \code{se_method}: SE method used.
#'   \item \code{irr}: \code{"Estimated"} or \code{"Assumed (<value> )"}.
#'   \item \code{tau}: numeric reliability used (average of subgroup taus when estimated).
#'   \item \code{remove_ties}, \code{ignore_position}: flags echoed from inputs.
#'   \item \code{se_type_used}, \code{cluster_by}: details propagated from fitting calls.
#'   \item \code{labels}, \code{data}: design labels and the analysis data (passed through).
#' }
#'
#' @keywords internal
#' @seealso \code{\link{projoint}}, \code{\link{projoint_level}}, \code{\link{projoint_results}}
projoint_diff <- function(
    .data,
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
    .se_type_2,
    .auto_cluster = TRUE,
    .seed = NULL
){
  
  # ---- Early guards --------------------------------------------------------
  
  .structure <- rlang::arg_match0(.structure, c("profile_level","choice_level"))
  
  # require .by_var and validate it
  if (missing(.by_var) || is.null(.by_var)) {
    stop("`.by_var` must be provided for subgroup analysis.")
  }
  if (.structure != "profile_level") {
    stop("Subgroup analysis via `.by_var` is only supported for `.structure = 'profile_level'`.")
  }
  if (!.by_var %in% names(.data$data)) {
    stop("`.by_var` must be a column in `.data$data`.")
  }
  vals <- unique(stats::na.omit(.data$data[[.by_var]]))
  is_ok <- is.logical(.data$data[[.by_var]]) ||
    (is.numeric(vals) && all(vals %in% c(0, 1))) ||
    (is.factor(.data$data[[.by_var]]) && all(levels(.data$data[[.by_var]]) %in% c("0","1")))
  if (!is_ok) {
    stop("`.by_var` must be logical (TRUE/FALSE) or numeric/integer coded as 0/1 (factor levels '0'/'1' allowed).")
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
  
  # ---- Split data -----------------------------------------------------------
  
  by_col <- .data$data[[.by_var]]
  # allow factor "0"/"1"
  if (is.factor(by_col) && all(levels(by_col) %in% c("0","1"))) {
    by_col <- as.integer(as.character(by_col))
  }
  
  # drop NAs in .by_var (otherwise they go to neither subgroup)
  keep <- !is.na(by_col)
  
  if (is.logical(by_col)) {
    subgroup1 <- .data$data[ keep &  by_col, , drop = FALSE]
    subgroup0 <- .data$data[ keep & !by_col, , drop = FALSE]
  } else {
    # assumes 0/1 per guard above
    subgroup1 <- .data$data[ keep & by_col == 1L, , drop = FALSE]
    subgroup0 <- .data$data[ keep & by_col == 0L, , drop = FALSE]
  }
  
  if (nrow(subgroup1) == 0L || nrow(subgroup0) == 0L) {
    stop("Both subgroups defined by `.by_var` must have at least one observation.")
  }

  data1 <-  projoint_data("labels" = .data$labels, "data" = subgroup1)
  data0 <-  projoint_data("labels" = .data$labels, "data" = subgroup0)
  
  # ---- Run per subgroup -----------------------------------------------------
  
  out1 <- projoint_level(.data = data1,
                         .qoi = .qoi,
                         .structure = .structure,
                         .estimand = .estimand,
                         .se_method = .se_method,
                         .irr = .irr,
                         .remove_ties = .remove_ties,
                         .ignore_position = .ignore_position,
                         .n_sims = .n_sims,
                         .n_boot = .n_boot,
                         .weights_1 = .weights_1,
                         .clusters_1 = .clusters_1,
                         .se_type_1 = .se_type_1,
                         .weights_2 = .weights_2,
                         .clusters_2 = .clusters_2,
                         .se_type_2 = .se_type_2,
                         .auto_cluster = .auto_cluster)
  
  out0 <- projoint_level(.data = data0,
                         .qoi = .qoi,
                         .structure = .structure,
                         .estimand = .estimand,
                         .se_method = .se_method,
                         .irr = .irr,
                         .remove_ties = .remove_ties,
                         .ignore_position = .ignore_position,
                         .n_sims = .n_sims,
                         .n_boot = .n_boot,
                         .weights_1 = .weights_1,
                         .clusters_1 = .clusters_1,
                         .se_type_1 = .se_type_1,
                         .weights_2 = .weights_2,
                         .clusters_2 = .clusters_2,
                         .se_type_2 = .se_type_2,
                         .auto_cluster = .auto_cluster)
  
  # ---- Combine --------------------------------------------------------------
  
  estimate1 <- out1$estimates |> 
    dplyr::select(estimand, att_level_choose,
                  "estimate_1" = estimate,
                  "se_1" = se) |> 
    dplyr::mutate(tau = out1$tau)
  
  estimate0 <- out0$estimates |> 
    dplyr::select(estimand, att_level_choose,
                  "estimate_0" = estimate,
                  "se_0" = se)
  
  estimates <- estimate1 |> 
    dplyr::left_join(estimate0, by = c("estimand", "att_level_choose")) |> 
    dplyr::mutate(estimate = estimate_1 - estimate_0,
           se = sqrt(se_1^2 + se_0^2), 
           conf.low = estimate - 1.96 * se,
           conf.high = estimate + 1.96 * se) 
  
  tau <- mean(c(out1$tau, out0$tau), na.rm = TRUE)

  se_type_used <- out1$se_type_used
  cluster_by   <- out1$cluster_by

  # ---- Labels & return ------------------------------------------------------
  
  if (is.null(.irr)){
    irr <- "Estimated"
  } else {
    irr <- stringr::str_c("Assumed (", .irr, ")")
  }

  if (.estimand == "mm"){
    
    if(is.null(.qoi)){
      return(projoint_results("estimand" = .estimand,
                       "structure" = .structure,
                       "estimates" = estimates, 
                       "se_method" = .se_method,
                       "irr" = irr,
                       "tau" = tau,
                       "remove_ties" = .remove_ties,
                       "ignore_position" = .ignore_position,
                       "attribute_of_interest" = "all",
                       "levels_of_interest" = "all",
                       "attribute_of_interest_0" = NULL,
                       "levels_of_interest_0" = NULL,
                       "attribute_of_interest_baseline" = NULL,
                       "levels_of_interest_baseline" = NULL,
                       "attribute_of_interest_0_baseline" = NULL,
                       "levels_of_interest_0_baseline" = NULL,
                       "se_type_used" = se_type_used,
                       "cluster_by"   = cluster_by,
                       labels = .data$labels,
                       data = .data$data))
    } else {
      return(projoint_results("estimand" = .estimand,
                       "structure" = .structure,
                       "estimates" = estimates, 
                       "se_method" = .se_method,
                       "irr" = irr,
                       "tau" = tau,
                       "remove_ties" = .remove_ties,
                       "ignore_position" = .ignore_position,
                       "attribute_of_interest" = .qoi$attribute_of_interest,
                       "levels_of_interest" = .qoi$levels_of_interest,
                       "attribute_of_interest_0" = .qoi$attribute_of_interest_0,
                       "levels_of_interest_0" = .qoi$levels_of_interest_0,
                       "attribute_of_interest_baseline" = NULL,
                       "levels_of_interest_baseline" = NULL,
                       "attribute_of_interest_0_baseline" = NULL,
                       "levels_of_interest_0_baseline" = NULL,
                       "se_type_used" = se_type_used,
                       "cluster_by"   = cluster_by,
                       labels = .data$labels,
                       data = .data$data))
    }
    
  } else {
    
    if(is.null(.qoi)){
      return(projoint_results("estimand" = .estimand,
                       "structure" = .structure,
                       "estimates" = estimates, 
                       "se_method" = .se_method,
                       "irr" = irr,
                       "tau" = tau,
                       "remove_ties" = .remove_ties,
                       "ignore_position" = .ignore_position,
                       "attribute_of_interest" = "all",
                       "levels_of_interest" = "all except level1",
                       "attribute_of_interest_0" = NULL,
                       "levels_of_interest_0" = NULL,
                       "attribute_of_interest_baseline" = "all",
                       "levels_of_interest_baseline" = "level1",
                       "attribute_of_interest_0_baseline" = NULL,
                       "levels_of_interest_0_baseline" = NULL,
                       "se_type_used" = se_type_used,
                       "cluster_by"   = cluster_by,
                       labels = .data$labels,
                       data = .data$data))

    } else {
      return(projoint_results("estimand" = .estimand,
                       "structure" = .structure,
                       "estimates" = estimates, 
                       "se_method" = .se_method,
                       "irr" = irr,
                       "tau" = tau,
                       "remove_ties" = .remove_ties,
                       "ignore_position" = .ignore_position,
                       "attribute_of_interest" = .qoi$attribute_of_interest,
                       "levels_of_interest" = .qoi$levels_of_interest,
                       "attribute_of_interest_0" = .qoi$attribute_of_interest_0,
                       "levels_of_interest_0" = .qoi$levels_of_interest_0,
                       "attribute_of_interest_baseline" = .qoi$attribute_of_interest_baseline,
                       "levels_of_interest_baseline" = .qoi$levels_of_interest_baseline,
                       "attribute_of_interest_0_baseline" = .qoi$attribute_of_interest_0_baseline,
                       "levels_of_interest_0_baseline" = .qoi$levels_of_interest_0_baseline,
                       "se_type_used" = se_type_used,
                       "cluster_by"   = cluster_by,
                       labels = .data$labels,
                       data = .data$data))
    }
    
  }
}


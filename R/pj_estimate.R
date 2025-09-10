#' Internal Estimation Function
#'
#' Core workhorse for computing marginal means (MMs) or AMCEs from a conjoint design,
#' with optional intra-respondent reliability (IRR) correction.
#'
#' @param .data A \code{\link{projoint_data}} object created by \code{\link{reshape_projoint}} or \code{\link{make_projoint_data}}.
#' @param .structure Either \code{"profile_level"} or \code{"choice_level"}.
#' @param .estimand Either \code{"mm"} (marginal mean) or \code{"amce"} (average marginal component effect).
#' @param .att_choose Attribute for the chosen profile/feature.
#' @param .lev_choose Level(s) for the chosen profile/feature. Length 1 for \code{profile_level}; may be 1+ for \code{choice_level}.
#' @param .att_notchoose Attribute for the not-chosen profile/feature (required for \code{choice_level}).
#' @param .lev_notchoose Level(s) for the not-chosen profile/feature (required for \code{choice_level}).
#' @param .att_choose_b (AMCE only) Baseline attribute for comparison.
#' @param .lev_choose_b (AMCE only) Baseline level(s) for comparison.
#' @param .att_notchoose_b (AMCE only, choice-level only) Baseline attribute for the not-chosen profile.
#' @param .lev_notchoose_b (AMCE only, choice-level only) Baseline level(s) for the not-chosen profile.
#' @param .se_method One of \code{"analytical"}, \code{"simulation"}, or \code{"bootstrap"}.
#' @param .irr \code{NULL} (default) to estimate IRR from repeated tasks; otherwise a numeric IRR value.
#' @param .remove_ties Logical; should ties be removed before estimation? Defaults to \code{TRUE}.
#' @param .ignore_position Logical; only for \code{choice_level}. If \code{TRUE} (default), ignore profile position (left/right).
#' @param .n_sims Integer; required if \code{.se_method == "simulation"}.
#' @param .n_boot Integer; required if \code{.se_method == "bootstrap"}.
#' @param .weights_1 (Optional) Bare (unquoted) column with weights for IRR estimation; passed to \code{\link[estimatr]{lm_robust}}.
#' @param .clusters_1 (Optional) Bare (unquoted) column with clusters for IRR estimation; passed to \code{\link[estimatr]{lm_robust}}.
#' @param .se_type_1 SE type for IRR estimation; passed to \code{\link[estimatr]{lm_robust}}. If \code{NULL}, \emph{estimatr} defaults are used (HC2 when unclustered; CR2 when clustered).
#' @param .weights_2 (Optional) Bare (unquoted) column with weights for MM/AMCE estimation; passed to \code{\link[estimatr]{lm_robust}}.
#' @param .clusters_2 (Optional) Bare (unquoted) column with clusters for MM/AMCE estimation; passed to \code{\link[estimatr]{lm_robust}}.
#' @param .se_type_2 SE type for MM/AMCE estimation; passed to \code{\link[estimatr]{lm_robust}}. If \code{NULL}, \emph{estimatr} defaults are used (HC2 when unclustered; CR2 when clustered).
#' @param .auto_cluster Logical; if \code{TRUE} (default), auto-cluster on \code{id} when present and no \code{.clusters_*} are supplied; auto-clustering only occurs when the corresponding \code{.se_type_*} is \code{NULL}. See \code{\link{projoint}}.
#' @param .seed Optional integer. If supplied, sets a temporary RNG seed for reproducible simulation/bootstrap inside this call 
#'   and restores the previous RNG state on exit.
#' @return A data frame with rows for the requested estimand(s) and columns:
#'   \itemize{
#'     \item \code{estimand}: one of \code{"mm_uncorrected"}, \code{"mm_corrected"},
#'           \code{"amce_uncorrected"}, \code{"amce_corrected"}.
#'     \item \code{estimate}, \code{se}, \code{conf.low}, \code{conf.high}, \code{tau}.
#'   }
#' @seealso \code{\link[estimatr]{lm_robust}}, \code{\link{projoint}},
#'   \code{\link{projoint_level}}, \code{\link{projoint_diff}}
#' @keywords internal
#' @details
#' IRR is clipped to \code{[0.5, 1)} (with a tiny epsilon) to avoid boundary issues.
#' For choice-level MMs, ties must be removed (\code{.remove_ties = TRUE}).
#' When \code{.seed} is supplied, the previous RNG state is restored on exit.
pj_estimate <- function(
    .data,
    .structure,
    .estimand,
    .att_choose, 
    .lev_choose,
    .att_notchoose, 
    .lev_notchoose,
    .att_choose_b, 
    .lev_choose_b,
    .att_notchoose_b, 
    .lev_notchoose_b,
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
  # ---- Match args -----------------------------------------------------------
  structure <- rlang::arg_match0(.structure, c("choice_level", "profile_level"))
  estimand  <- rlang::arg_match0(.estimand,  c("mm", "amce"))
  se_method <- rlang::arg_match0(.se_method, c("analytical", "simulation", "bootstrap"))
  
  if (structure == "choice_level" & is.null(.att_choose)) stop("The .att_choose argument must be specified.")
  if (structure == "choice_level" & is.null(.lev_choose)) stop("The .lev_choose argument must be specified.")
  
  # ---- Argument guards ------------------------------------------------------
  if(!is.null(.irr) & (!is.numeric(.irr) || length(.irr) != 1)) {
    stop("The .irr argument must be either a numeric scalar or NULL.")
  }
  if(!is.logical(.remove_ties)) stop("The .remove_ties argument must be either TRUE or FALSE.")
  
  if (structure == "profile_level" && length(.lev_choose) != 1L)
    stop(".lev_choose must have length 1 for profile-level estimands.")
  
  if (structure == "profile_level" & !is.null(.ignore_position)) {
    stop("The .ignore_position argument can be specified only when .structure = 'choice_level'.")
  }
  if (structure == "choice_level" & is.null(.ignore_position)) {
    stop("Specify the .ignore_position argument (TRUE/FALSE) for choice-level analysis.")
  }
  if (structure == "choice_level" & !is.logical(.ignore_position)) {
    stop("The .ignore_position argument must be either TRUE or FALSE.")
  }
  
  # after the existing .att_choose / .lev_choose checks
  if (structure == "choice_level" & is.null(.att_notchoose))
    stop("The .att_notchoose argument must be specified for choice-level analysis.")
  if (structure == "choice_level" & is.null(.lev_notchoose))
    stop("The .lev_notchoose argument must be specified for choice-level analysis.")
  
  if (estimand == "amce" & (is.null(.att_choose_b) || is.null(.lev_choose_b)))
    stop("For AMCE, .att_choose_b and .lev_choose_b (baseline) must be specified.")
  if (estimand == "amce" && structure == "choice_level" &&
      (is.null(.att_notchoose_b) || is.null(.lev_notchoose_b)))
    stop("For choice-level AMCE, .att_notchoose_b and .lev_notchoose_b must be specified.")
  
  if(!is.null(.n_sims) & (!is.numeric(.n_sims) || length(.n_sims) != 1)) {
    stop("The .n_sims argument must be either a numeric scalar or NULL.")
  }
  if(!is.null(.n_boot) & (!is.numeric(.n_boot) || length(.n_boot) != 1)) {
    stop("The .n_boot argument must be either a numeric scalar or NULL.")
  }
  if(se_method == "simulation" & is.null(.n_sims))  stop("Specify .n_sims for simulation SEs.")
  if(se_method != "simulation" & !is.null(.n_sims)) stop("You cannot specify .n_sims unless .se_method = 'simulation'.")
  if(se_method == "bootstrap"  & is.null(.n_boot))  stop("Specify .n_boot for bootstrap SEs.")
  if(se_method != "bootstrap"  & !is.null(.n_boot)) stop("You cannot specify .n_boot unless .se_method = 'bootstrap'.")
  
  if (structure == "choice_level" & estimand == "mm" & .remove_ties == FALSE) {
    stop(".remove_ties must be TRUE to estimate choice-level MMs.")
  }
  if (identical(.se_type_2, "none") && se_method == "simulation") {
    stop("Simulation SEs require finite standard errors. Choose analytical or bootstrap when se_type = 'none'.")
  }
  
  # ---- Optional reproducible RNG (low-level) --------------------------------
  if (!is.null(.seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      old_seed <- .Random.seed
      on.exit({
        if (exists("old_seed", inherits = FALSE)) .Random.seed <<- old_seed
      }, add = TRUE)
    }
    set.seed(.seed)
  }
  
  # ---- Organize data --------------------------------------------------------
  if (estimand == "mm"){
    if (structure == "choice_level"){
      attlev_choose    <- stringr::str_c(.att_choose,    ":", .lev_choose)
      attlev_notchoose <- stringr::str_c(.att_notchoose, ":", .lev_notchoose)
      
      temp1 <- organize_data(.dataframe = .data$data, .structure = structure, .estimand = estimand,
                             .remove_ties, .att_choose, .lev_choose, .att_notchoose, .lev_notchoose)
      
      if (isTRUE(.ignore_position)){
        if (.att_choose == .att_notchoose){
          temp2 <- temp1$data_for_estimand |>
            dplyr::mutate(
              selected    = ifelse(qoi_1 %in% attlev_choose, selected_1, selected_2),
              qoi_choose  = stringr::str_c(attlev_choose, collapse = ", "),
              qoi_notchoose = stringr::str_c(attlev_notchoose, collapse = ", ")
            ) |>
            dplyr::select(-matches("_\\d$"))
        } else {
          temp2 <- temp1$data_for_estimand |>
            dplyr::mutate(
              selected    = ifelse(qoi_choose_1 %in% attlev_choose, selected_1, selected_2),
              qoi_choose  = stringr::str_c(attlev_choose, collapse = ", "),
              qoi_notchoose = stringr::str_c(attlev_notchoose, collapse = ", ")
            ) |>
            dplyr::select(-matches("_\\d$"))
        }
      } else {
        if (.att_choose == .att_notchoose){
          temp2 <- temp1$data_for_estimand |>
            dplyr::filter(qoi_1 %in% attlev_notchoose, qoi_2 %in% attlev_choose) |>
            dplyr::mutate(
              selected    = selected_2, 
              qoi_choose  = stringr::str_c(attlev_choose, collapse = ", "),
              qoi_notchoose = stringr::str_c(attlev_notchoose, collapse = ", ")
            ) |>
            dplyr::select(-matches("_\\d$"))
        } else {
          temp2 <- temp1$data_for_estimand |>
            dplyr::filter(qoi_notchoose_1 %in% attlev_notchoose, qoi_choose_2 %in% attlev_choose) |>
            dplyr::mutate(
              selected    = selected_2,
              qoi_choose  = stringr::str_c(attlev_choose, collapse = ", "),
              qoi_notchoose = stringr::str_c(attlev_notchoose, collapse = ", ")
            ) |>
            dplyr::select(-matches("_\\d$"))
        }
      }
      
      data_for_irr      <- temp1$data_for_irr |> dplyr::distinct()
      data_for_estimand <- temp2
      
      # Ensure id exists for bootstrap/resampling
      if (!"id" %in% names(data_for_irr))      data_for_irr$id      <- seq_len(nrow(data_for_irr))
      if (!"id" %in% names(data_for_estimand)) data_for_estimand$id <- seq_len(nrow(data_for_estimand))
      
    } else {
      .list <- organize_data(.dataframe = .data$data, .structure = structure, .estimand = estimand,
                             .remove_ties, .att_choose, .lev_choose, .att_notchoose, .lev_notchoose)
      data_for_irr      <- .list$data_for_irr
      data_for_estimand <- .list$data_for_estimand
      
      # Ensure id exists for bootstrap/resampling
      if (!"id" %in% names(data_for_irr))      data_for_irr$id      <- seq_len(nrow(data_for_irr))
      if (!"id" %in% names(data_for_estimand)) data_for_estimand$id <- seq_len(nrow(data_for_estimand))
      
    }
  } else { # estimand == "amce"
    if (structure == "choice_level"){
      # Not baseline
      attlev_choose    <- stringr::str_c(.att_choose,    ":", .lev_choose)
      attlev_notchoose <- stringr::str_c(.att_notchoose, ":", .lev_notchoose)
      temp1 <- organize_data(.dataframe = .data$data, .structure = structure, .estimand = estimand,
                             .remove_ties, .att_choose, .lev_choose, .att_notchoose, .lev_notchoose)
      if (isTRUE(.ignore_position)){
        if (.att_choose == .att_notchoose){
          temp2 <- temp1$data_for_estimand |>
            dplyr::mutate(
              selected    = ifelse(qoi_1 %in% attlev_choose, selected_1, selected_2),
              qoi_choose  = stringr::str_c(attlev_choose, collapse = ", "),
              qoi_notchoose = stringr::str_c(attlev_notchoose, collapse = ", ")
            ) |>
            dplyr::select(-matches("_\\d$"))
        } else {
          temp2 <- temp1$data_for_estimand |>
            dplyr::mutate(
              selected    = ifelse(qoi_choose_1 %in% attlev_choose, selected_1, selected_2),
              qoi_choose  = stringr::str_c(attlev_choose, collapse = ", "),
              qoi_notchoose = stringr::str_c(attlev_notchoose, collapse = ", ")
            ) |>
            dplyr::select(-matches("_\\d$"))
        }
      } else {
        if (.att_choose == .att_notchoose){
          temp2 <- temp1$data_for_estimand |>
            dplyr::filter(qoi_1 %in% attlev_notchoose, qoi_2 %in% attlev_choose) |>
            dplyr::mutate(
              selected    = selected_2,
              qoi_choose  = stringr::str_c(attlev_choose, collapse = ", "),
              qoi_notchoose = stringr::str_c(attlev_notchoose, collapse = ", ")
            ) |>
            dplyr::select(-matches("_\\d$"))
        } else {
          temp2 <- temp1$data_for_estimand |>
            dplyr::filter(qoi_notchoose_1 %in% attlev_notchoose, qoi_choose_2 %in% attlev_choose) |>
            dplyr::mutate(
              selected    = selected_2,
              qoi_choose  = stringr::str_c(attlev_choose, collapse = ", "),
              qoi_notchoose = stringr::str_c(attlev_notchoose, collapse = ", ")
            ) |>
            dplyr::select(-matches("_\\d$"))
        }
      }
      
      # Baseline
      attlev_choose_b    <- stringr::str_c(.att_choose_b,    ":", .lev_choose_b)
      attlev_notchoose_b <- stringr::str_c(.att_notchoose_b, ":", .lev_notchoose_b)
      
      temp1_b <- organize_data(.dataframe = .data$data, .structure = structure, .estimand = estimand,
                               .remove_ties,
                               .att_choose = .att_choose_b, .lev_choose = .lev_choose_b,
                               .att_notchoose = .att_notchoose_b, .lev_notchoose = .lev_notchoose_b)
      if (isTRUE(.ignore_position)){
        if (.att_choose_b == .att_notchoose_b){
          temp2_b <- temp1_b$data_for_estimand |>
            dplyr::mutate(
              selected    = ifelse(qoi_1 %in% attlev_choose_b, selected_1, selected_2),
              qoi_choose  = stringr::str_c(attlev_choose_b, collapse = ", "),
              qoi_notchoose = stringr::str_c(attlev_notchoose_b, collapse = ", ")
            ) |>
            dplyr::select(-matches("_\\d$"))
        } else {
          temp2_b <- temp1_b$data_for_estimand |>
            dplyr::mutate(
              selected    = ifelse(qoi_choose_1 %in% attlev_choose_b, selected_1, selected_2),
              qoi_choose  = stringr::str_c(attlev_choose_b, collapse = ", "),
              qoi_notchoose = stringr::str_c(attlev_notchoose_b, collapse = ", ")
            ) |>
            dplyr::select(-matches("_\\d$"))
        }
      } else {
        if (.att_choose_b == .att_notchoose_b){
          temp2_b <- temp1_b$data_for_estimand |>
            dplyr::filter(qoi_1 %in% attlev_notchoose_b, qoi_2 %in% attlev_choose_b) |>
            dplyr::mutate(
              selected    = selected_2,
              qoi_choose  = stringr::str_c(attlev_choose_b, collapse = ", "),
              qoi_notchoose = stringr::str_c(attlev_notchoose_b, collapse = ", ")
            ) |>
            dplyr::select(-matches("_\\d$"))
        } else {
          temp2_b <- temp1_b$data_for_estimand |>
            dplyr::filter(qoi_notchoose_1 %in% attlev_notchoose_b, qoi_choose_2 %in% attlev_choose_b) |>
            dplyr::mutate(
              selected    = selected_2,
              qoi_choose  = stringr::str_c(attlev_choose_b, collapse = ", "),
              qoi_notchoose = stringr::str_c(attlev_notchoose_b, collapse = ", ")
            ) |>
            dplyr::select(-matches("_\\d$"))
        }
      }
      
      # Merge
      data_for_irr <- dplyr::bind_rows(temp1$data_for_irr, temp1_b$data_for_irr) |> dplyr::distinct()
      data_for_estimand <- dplyr::bind_rows(
        temp2   |> dplyr::mutate(x = 1),
        temp2_b |> dplyr::mutate(x = 0)
      )
      
      # Ensure id exists for bootstrap/resampling
      if (!"id" %in% names(data_for_irr))      data_for_irr$id      <- seq_len(nrow(data_for_irr))
      if (!"id" %in% names(data_for_estimand)) data_for_estimand$id <- seq_len(nrow(data_for_estimand))
      
      
    } else {
      temp1   <- organize_data(.dataframe = .data$data, .structure = structure, .estimand = estimand,
                               .remove_ties, .att_choose, .lev_choose, .att_notchoose, .lev_notchoose)
      temp1_b <- organize_data(.dataframe = .data$data, .structure = structure, .estimand = estimand,
                               .remove_ties,
                               .att_choose = .att_choose_b, .lev_choose = .lev_choose_b,
                               .att_notchoose = .att_notchoose_b, .lev_notchoose = .lev_notchoose_b)
      data_for_irr <- dplyr::bind_rows(temp1$data_for_irr, temp1_b$data_for_irr) |> dplyr::distinct()
      data_for_estimand <- dplyr::bind_rows(
        temp1$data_for_estimand   |> dplyr::mutate(x = 1),
        temp1_b$data_for_estimand |> dplyr::mutate(x = 0)
      )
      
      if (!"id" %in% names(data_for_irr))      data_for_irr$id      <- seq_len(nrow(data_for_irr))
      if (!"id" %in% names(data_for_estimand)) data_for_estimand$id <- seq_len(nrow(data_for_estimand))
      
    }
  }
  
  if (!"id" %in% names(data_for_irr))      data_for_irr$id      <- dplyr::row_number()
  if (!"id" %in% names(data_for_estimand)) data_for_estimand$id <- dplyr::row_number()
  
  # ---- Tidy-eval (quosures) -------------------------------------------------
  # Accept either a bare name (we'll enquo it) or a pre-made quosure
  clusters1_quo <- if (rlang::is_quosure(.clusters_1)) .clusters_1 else rlang::enquo(.clusters_1)
  weights1_quo  <- if (rlang::is_quosure(.weights_1))  .weights_1  else rlang::enquo(.weights_1)
  clusters2_quo <- if (rlang::is_quosure(.clusters_2)) .clusters_2 else rlang::enquo(.clusters_2)
  weights2_quo  <- if (rlang::is_quosure(.weights_2))  .weights_2  else rlang::enquo(.weights_2)
  
  
  # Auto-cluster only if allowed and sensible
  if (is.null(.irr) &&
      .auto_cluster &&
      rlang::quo_is_null(clusters1_quo) &&
      "id" %in% names(data_for_irr) &&
      is.null(.se_type_1)) {
    clusters1_quo <- rlang::quo(id)
  }
  if (.auto_cluster &&
      rlang::quo_is_null(clusters2_quo) &&
      "id" %in% names(data_for_estimand) &&
      is.null(.se_type_2)) {
    clusters2_quo <- rlang::quo(id)
  }
  
  # Figure out whether each stage is clustered
  is_clustered_1 <- if (is.null(.irr)) !rlang::quo_is_null(clusters1_quo) else FALSE
  is_clustered_2 <- !rlang::quo_is_null(clusters2_quo)
  
  # Define allowed se_type values once, before validation
  ok_uncl <- c("classical","HC0","HC1","HC2","HC3","stata","none")
  ok_cl   <- c("CR0","CR2","stata","none")
  
  # And gate se_type_1 validation:
  if (is.null(.irr) && !is.null(.se_type_1)) {
    if (is_clustered_1 && !(.se_type_1 %in% ok_cl)) {
      stop("`.se_type_1` must be one of ", paste(ok_cl, collapse=", "),
           " when clusters are supplied or auto-detected.")
    }
    if (!is_clustered_1 && !(.se_type_1 %in% ok_uncl)) {
      stop("`.se_type_1` must be one of ", paste(ok_uncl, collapse=", "),
           " when no clusters are used.")
    }
  }
  
  
  if (!is.null(.se_type_2)) {
    if (is_clustered_2 && !(.se_type_2 %in% ok_cl)) {
      stop("`.se_type_2` must be one of ", paste(ok_cl, collapse=", "),
           " when clusters are supplied or auto-detected.")
    }
    if (!is_clustered_2 && !(.se_type_2 %in% ok_uncl)) {
      stop("`.se_type_2` must be one of ", paste(ok_uncl, collapse=", "),
           " when no clusters are used.")
    }
  }
  
  # ---- Decide actual se_type passed to lm_robust() ---------------------------
  se_type2_final <- if (is.null(.se_type_2)) {
    if (is_clustered_2) "CR2" else "HC2"
  } else {
    .se_type_2
  }
  
  # Use this name for the label (NULL/NA if no clustering)
  cluster_by2 <- if (is_clustered_2) {
    # try to get a nice name for the quosure
    rlang::as_name(clusters2_quo)
  } else {
    NA_character_
  }
  
  # ---- Estimate IRR / tau ---------------------------------------------------
  
  if (nrow(data_for_estimand) == 0L) {
    stop("No rows match the specified attribute/level combination(s).")
  }
  if (is.null(.irr) && nrow(data_for_irr) == 0L) {
    stop("No rows available to estimate IRR/tau. Consider supplying .irr directly.")
  }
  # if (nrow(data_for_irr) == 0L) {
  #   stop("No rows available to estimate IRR/tau.")
  # }
  
  critical_from_ci <- function(est, se, lo) {
    ct <- abs((lo - est) / se)
    if (!is.finite(ct)) 1.96 else ct
  }
  
  eps <- 1e-8
  
  if (is.null(.irr)) {
    reg_irr <- estimatr::lm_robust(
      agree ~ 1,
      weights  = if (!rlang::quo_is_null(weights1_quo))  rlang::eval_tidy(weights1_quo,  data_for_irr) else NULL,
      clusters = if (!rlang::quo_is_null(clusters1_quo)) rlang::eval_tidy(clusters1_quo, data_for_irr) else NULL,
      se_type = .se_type_1,
      data = data_for_irr
    ) |> estimatr::tidy()
    
    irr_raw <- reg_irr$estimate[1]
    if (is.na(irr_raw)) {
      warning("Estimated IRR is NA. Using fallback IRR = 0.75.")
      irr <- 0.75
      var_irr <- 0   # avoid NA propagation in var_tau
    } else {
      if (irr_raw < 0.5 || irr_raw > 1) {
        warning(sprintf("Estimated IRR was %.3f and clipped to [0.5, 1].", irr_raw))
      }
      irr <- irr_raw
      se_irr  <- reg_irr$std.error[1]
      var_irr <- if (is.na(se_irr)) 0 else se_irr^2
    }
    
    # Nudge off boundaries for stability
    irr <- min(max(irr, 0.5 + eps), 1 - eps)
    
    # With interior IRR, no special-casing needed
    tau     <- (1 - sqrt(1 - 2 * (1 - irr))) / 2
    var_tau <- 0.25 * (2 * irr - 1)^(-1) * var_irr
    
  } else if (!is.numeric(.irr) || length(.irr) != 1L || is.na(.irr) || !is.finite(.irr)) {
    stop(".irr should be a finite numeric scalar.")
    
  } else {
    # user-supplied IRR: warn if outside [0.5,1], then always nudge/clip
    if (.irr < 0.5 || .irr > 1) {
      warning(sprintf("Supplied IRR %.3f clipped to [0.5, 1].", .irr))
    }
    irr <- min(max(.irr, 0.5 + eps), 1 - eps)
    
    tau     <- (1 - sqrt(1 - 2 * (1 - irr))) / 2
    var_tau <- 0   # user-supplied IRR treated as fixed
  }
  
  
  # ---- Uncorrected estimate -------------------------------------------------
  if (estimand == "mm") {
    # reg_mm <- estimatr::lm_robust(
    #   selected ~ 1,
    #   weights  = if (!rlang::quo_is_null(weights2_quo))  rlang::eval_tidy(weights2_quo,  data_for_estimand) else NULL,
    #   clusters = if (!rlang::quo_is_null(clusters2_quo)) rlang::eval_tidy(clusters2_quo, data_for_estimand) else NULL,
    #   se_type  = se_type2_final,
    #   data     = data_for_estimand
    # ) |> estimatr::tidy()
    
    # Fixed on Sep 10, 2025     ###############################################
    
    # cache once
    w2  <- if (!rlang::quo_is_null(weights2_quo))  rlang::eval_tidy(weights2_quo,  data_for_estimand) else NULL
    cl2 <- if (!rlang::quo_is_null(clusters2_quo)) rlang::eval_tidy(clusters2_quo, data_for_estimand) else NULL
    
    # 1) fit with configured se_type2_final (often "CR2")
    reg_mm_fit <- suppressWarnings(
      estimatr::lm_robust(
        selected ~ 1,
        weights  = w2,
        clusters = cl2,
        se_type  = se_type2_final,
        data     = data_for_estimand
      )
    )
    
    V <- reg_mm_fit$vcov
    d <- diag(V)
    bad_vcov <- any(!is.finite(d) | d < -1e-10)
    bad_se   <- any(!is.finite(summary(reg_mm_fit)$coefficients[, "Std. Error"]))
    
    # 2) fallback if NA/Inf or negative diag
    if (bad_vcov || bad_se) {
      reg_mm_fit <- suppressWarnings(
        estimatr::lm_robust(
          selected ~ 1,
          weights  = w2,
          clusters = cl2,
          se_type  = "stata",
          data     = data_for_estimand
        )
      )
      
      V <- reg_mm_fit$vcov
      d <- diag(V)
      bad_vcov <- any(!is.finite(d) | d < -1e-10)
      bad_se   <- any(!is.finite(summary(reg_mm_fit)$coefficients[, "Std. Error"]))
      
      # optional last resort: drop clusters + HC1
      if (bad_vcov || bad_se) {
        reg_mm_fit <- estimatr::lm_robust(
          selected ~ 1,
          weights  = w2,
          clusters = NULL,
          se_type  = "HC1",
          data     = data_for_estimand
        )
        V <- reg_mm_fit$vcov
        d <- diag(V)
      }
      
      warning("MM analytical SEs: CR2 produced non-PSD/NA variances; fell back to se_type='stata' (then HC1 if needed).")
    }
    
    # (optional) record what actually got used
    se_type2_used_mm <- reg_mm_fit$se_type
    # meta$main_se$se_type_used_mm <- se_type2_used_mm
    
    # proceed
    reg_mm <- estimatr::tidy(reg_mm_fit)
    
    ###############################################
    
    critical_t <- critical_from_ci(reg_mm$estimate[1], reg_mm$std.error[1], reg_mm$conf.low[1])
    
    mm_uncorrected     <- reg_mm$estimate[1]
    var_uncorrected_mm <- reg_mm$std.error[1]^2
    
    if (se_method == "simulation" && is.na(reg_mm$std.error[1])) {
      warning("Simulation SEs require finite standard errors; e.g., with se_type = 'none' results may be NA.")
    }
  }
  
  if (estimand == "amce") {
    
    
    # reg_amce <- estimatr::lm_robust(
    #   selected ~ x,
    #   weights  = if (!rlang::quo_is_null(weights2_quo))  rlang::eval_tidy(weights2_quo,  data_for_estimand) else NULL,
    #   clusters = if (!rlang::quo_is_null(clusters2_quo)) rlang::eval_tidy(clusters2_quo, data_for_estimand) else NULL,
    #   se_type  = se_type2_final,
    #   data     = data_for_estimand
    # ) |> estimatr::tidy()
    
    # Fixed on Sep 10, 2025     ###############################################
    
    # cache weights/clusters once
    w2  <- if (!rlang::quo_is_null(weights2_quo))  rlang::eval_tidy(weights2_quo,  data_for_estimand) else NULL
    cl2 <- if (!rlang::quo_is_null(clusters2_quo)) rlang::eval_tidy(clusters2_quo, data_for_estimand) else NULL
    
    # 1) fit with your configured se_type2_final (likely "CR2")
    reg_amce_fit <- suppressWarnings(
      estimatr::lm_robust(
        selected ~ x,
        weights  = w2,
        clusters = cl2,
        se_type  = se_type2_final,
        data     = data_for_estimand
      )
    )
    
    V <- reg_amce_fit$vcov
    d <- diag(V)
    bad_vcov <- any(!is.finite(d) | d < -1e-10)
    bad_se   <- any(!is.finite(summary(reg_amce_fit)$coefficients[, "Std. Error"]))

    # 2) fallback if vcov is NA/Inf or non-PSD (negative diag)
    if (bad_vcov || bad_se) {
      reg_amce_fit <- suppressWarnings(
        estimatr::lm_robust(
          selected ~ x,
          weights  = w2,
          clusters = cl2,
          se_type  = "stata"   # HC1/Stata-style; typically PSD
          ,
          data     = data_for_estimand
        )
      )
      
      V <- reg_amce_fit$vcov
      d <- diag(V)
      bad_vcov <- any(!is.finite(d) | d < -1e-10)
      bad_se   <- any(!is.finite(summary(reg_amce_fit)$coefficients[, "Std. Error"]))
      
      # optional last resort: drop clustering if still bad
      if (bad_vcov || bad_se) {
        reg_amce_fit <- suppressWarnings(
          estimatr::lm_robust(
            selected ~ x,
            weights  = w2,
            clusters = NULL,
            se_type  = "HC1",
            data     = data_for_estimand
          )
        )
        V <- reg_amce_fit$vcov
        d <- diag(V)
      }
      
      warning("AMCE analytical SEs: CR2 produced non-PSD/NA variances; fell back to se_type='stata' (then HC1 if needed).")
    }
    
    # (optional) record what actually got used
    se_type2_used <- reg_amce_fit$se_type
    # e.g., stash in your result/meta if you have one:
    # meta$main_se$se_type_used <- se_type2_used
    
    # proceed as before
    reg_amce <- estimatr::tidy(reg_amce_fit)
    
    ###############################################
    
    
    
    critical_t <- critical_from_ci(reg_amce$estimate[2], reg_amce$std.error[2], reg_amce$conf.low[2])
    
    amce_uncorrected     <- reg_amce$estimate[2]
    var_uncorrected_amce <- reg_amce$std.error[2]^2
    
    if (se_method == "simulation" && is.na(reg_amce$std.error[2])) {
      warning("Simulation SEs require finite standard errors; e.g., with se_type = 'none' results may be NA.")
    }
  }
  
  # ---- Covariance pieces ----------------------------------------------------
  has_agree <- "agree" %in% names(data_for_estimand)
  data_for_cov <- if (has_agree) dplyr::filter(data_for_estimand, !is.na(agree))
  else data_for_estimand[0, , drop = FALSE]
  nz <- has_agree && nrow(data_for_cov) > 0L
  
  if (estimand == "mm"){
    if (is.null(.irr) && nz) {
      cov_mm_irr <- if (nrow(data_for_cov) > 1L)
        stats::cov(data_for_cov$selected, data_for_cov$agree) / nrow(data_for_irr)
      else 0
    } else {
      cov_mm_irr <- 0
    }
    cov_mm_tau <- -0.5 * (2 * irr - 1)^(-1/2) * cov_mm_irr
  }
  
  if (estimand == "amce"){
    d_cov0 <- data_for_cov |> dplyr::filter(x == 0)
    d_cov1 <- data_for_cov |> dplyr::filter(x == 1)
    n0 <- if (nz) nrow(d_cov0) else 0L
    n1 <- if (nz) nrow(d_cov1) else 0L
    
    if (is.null(.irr) && nz) {
      cov_mm0_irr <- if (n0 > 1L) stats::cov(d_cov0$selected, d_cov0$agree) / nrow(data_for_irr) else 0
      cov_mm1_irr <- if (n1 > 1L) stats::cov(d_cov1$selected, d_cov1$agree) / nrow(data_for_irr) else 0
    } else {
      cov_mm0_irr <- 0
      cov_mm1_irr <- 0
    }
    cov_mm0_tau  <- -0.5 * (2 * irr - 1)^(-1/2) * cov_mm0_irr
    cov_mm1_tau  <- -0.5 * (2 * irr - 1)^(-1/2) * cov_mm1_irr
    cov_amce_tau <- cov_mm1_tau - cov_mm0_tau
  }
  
  # ---- Corrected estimates & SEs -------------------------------------------
  if (estimand == "mm"){
    if (se_method == "analytical"){
      mm_corrected <- (mm_uncorrected - tau) / (1 - 2 * tau)
      # var_corrected_mm <- (mm_corrected^2 / (1 - (2 * tau))^2) *
      #   ( ((var_uncorrected_mm + var_tau - 2 * cov_mm_tau) / mm_corrected^2)
      #     + 4 * (cov_mm_tau - var_tau) / mm_corrected
      #     + 4 * var_tau )
      
      # Use delta-method. Avoid blowing up the variance when the corrected estimate is near 0.
      dp  <- 1 / (1 - 2 * tau)
      dt  <- (2 * mm_uncorrected - 1) / (1 - 2 * tau)^2
      var_corrected_mm <- dp^2 * var_uncorrected_mm + dt^2 * var_tau + 2 * dp * dt * cov_mm_tau
      
      output <- data.frame(
        estimand = c("mm_uncorrected", "mm_corrected"),
        estimate = c(mm_uncorrected, mm_corrected),
        se       = c(sqrt(pmax(var_uncorrected_mm, 0)), sqrt(pmax(var_corrected_mm, 0)))
      ) |>
        dplyr::mutate(conf.low  = estimate - critical_t * se,
                      conf.high = estimate + critical_t * se,
                      tau = tau)
    }  
    
    if (se_method == "simulation"){
      if ((var_tau <= 0 || !is.finite(var_tau)) && (cov_mm_tau == 0)) {
        mm_draws  <- stats::rnorm(.n_sims, mean = mm_uncorrected, sd = sqrt(var_uncorrected_mm))
        tau_draws <- rep(tau, .n_sims)
        sim_mm <- data.frame(mm_uncorrected = mm_draws, tau = tau_draws) |>
          dplyr::mutate(mm_corrected = (mm_uncorrected - tau) / (1 - 2 * tau))
      } else {
        vcov_mm_tau <- matrix(c(var_uncorrected_mm, cov_mm_tau,
                                cov_mm_tau,        var_tau), nrow = 2)
        sim_mm <- as.data.frame(MASS::mvrnorm(.n_sims, c(mm_uncorrected, tau), vcov_mm_tau)) |>
          rlang::set_names(c("mm_uncorrected", "tau")) |>
          dplyr::mutate(mm_corrected = (mm_uncorrected - tau) / (1 - 2 * tau))
      }
      
      output <- data.frame(
        estimand = c("mm_uncorrected", "mm_corrected"),
        estimate = c(mm_uncorrected, mean(sim_mm$mm_corrected)),
        se       = c(sqrt(pmax(var_uncorrected_mm, 0)), stats::sd(sim_mm$mm_corrected))
      ) |>
        dplyr::mutate(conf.low  = estimate - critical_t * se,
                      conf.high = estimate + critical_t * se,
                      tau = tau)
    }
    
    if (se_method == "bootstrap"){
      # Only define IDs_1 if we’ll estimate IRR
      if (is.null(.irr)) {
        IDs_1 <- unique(stats::na.omit(data_for_irr$id))
        if (length(IDs_1) == 0L) stop("No rows available to bootstrap IRR/tau. Consider supplying .irr directly.")
      }
      IDs_2 <- unique(stats::na.omit(data_for_estimand$id))
      out <- NULL
      for (i in seq_len(.n_boot)) {
        
        # Always bootstrap the estimand stage
        id_2 <- sample(IDs_2, length(IDs_2), replace = TRUE)
        bs_sample_2 <- data.frame(id = id_2) |>
          dplyr::left_join(data_for_estimand, by = "id", relationship = "many-to-many")
        
        # Bootstrap (and fit) IRR only when needed
        if (is.null(.irr)) {
          id_1 <- sample(IDs_1, length(IDs_1), replace = TRUE)
          bs_sample_1 <- data.frame(id = id_1) |>
            dplyr::left_join(data_for_irr, by = "id", relationship = "many-to-many")
          
          reg_irr <- estimatr::lm_robust(
            agree ~ 1,
            weights  = if (!rlang::quo_is_null(weights1_quo))  rlang::eval_tidy(weights1_quo,  bs_sample_1) else NULL,
            clusters = if (!rlang::quo_is_null(clusters1_quo)) rlang::eval_tidy(clusters1_quo, bs_sample_1) else NULL,
            se_type  = .se_type_1,
            data     = bs_sample_1
          ) |> estimatr::tidy()
          
          irr_bs <- reg_irr$estimate[1]
          tau_bs <- (1 - sqrt(1 - 2 * (1 - irr_bs))) / 2
        } else {
          tau_bs <- (1 - sqrt(1 - 2 * (1 - .irr))) / 2
        }
        
        reg_mm <- estimatr::lm_robust(
          selected ~ 1,
          weights  = if (!rlang::quo_is_null(weights2_quo))  rlang::eval_tidy(weights2_quo,  bs_sample_2) else NULL,
          clusters = if (!rlang::quo_is_null(clusters2_quo)) rlang::eval_tidy(clusters2_quo, bs_sample_2) else NULL,
          se_type = se_type2_final,
          data = bs_sample_2
        ) |> estimatr::tidy()
        
        mm_uncorrected_bs <- reg_mm$estimate[1]
        mm_corrected_bs <- (mm_uncorrected_bs - tau_bs) / (1 - 2 * tau_bs)
        
        out <- dplyr::bind_rows(out,
                                data.frame(estimand = c("mm_corrected","mm_uncorrected"),
                                           estimate = c(mm_corrected_bs, mm_uncorrected_bs)))
      }
      
      output <- data.frame(
        estimand = c("mm_uncorrected","mm_corrected"),
        estimate = c(
          mean(out |> dplyr::filter(estimand == "mm_uncorrected") |> dplyr::pull(estimate)),
          mean(out |> dplyr::filter(estimand == "mm_corrected")   |> dplyr::pull(estimate))
        ),
        se = c(
          stats::sd(out |> dplyr::filter(estimand == "mm_uncorrected") |> dplyr::pull(estimate)),
          stats::sd(out |> dplyr::filter(estimand == "mm_corrected")   |> dplyr::pull(estimate))
        )
      ) |>
        dplyr::mutate(conf.low = estimate - critical_t * se,
                      conf.high = estimate + critical_t * se,
                      tau = tau)
    }
  }
  
  if (estimand == "amce"){
    if (se_method == "analytical"){
      amce_corrected <- amce_uncorrected / (1 - 2 * tau)
      
      # var_corrected_amce <- (amce_corrected^2 / (1 - (2 * tau))^2) *
      #   ( (var_uncorrected_amce / amce_corrected^2)
      #     + 4 * cov_amce_tau / amce_corrected
      #     + 4 * var_tau )
      
      # Use delta-method. Avoid blowing up the variance when the corrected estimate is near 0.
      db  <- 1 / (1 - 2 * tau)
      dt  <- 2 * amce_uncorrected / (1 - 2 * tau)^2
      var_corrected_amce <- db^2 * var_uncorrected_amce + dt^2 * var_tau + 2 * db * dt * cov_amce_tau
      
      output <- data.frame(
        estimand = c("amce_uncorrected","amce_corrected"),
        estimate = c(amce_uncorrected, amce_corrected),
        se       = c(sqrt(pmax(var_uncorrected_amce, 0)), sqrt(pmax(var_corrected_amce, 0)))
      ) |>
        dplyr::mutate(conf.low  = estimate - critical_t * se,
                      conf.high = estimate + critical_t * se,
                      tau = tau)
    }
    
    if (se_method == "simulation"){
      if ((var_tau <= 0 || !is.finite(var_tau)) && (cov_amce_tau == 0)) {
        amce_draws <- stats::rnorm(.n_sims, mean = amce_uncorrected, sd = sqrt(var_uncorrected_amce))
        tau_draws  <- rep(tau, .n_sims)
        sim_amce <- data.frame(amce_uncorrected = amce_draws, tau = tau_draws) |>
          dplyr::mutate(amce_corrected = amce_uncorrected / (1 - 2 * tau))
      } else {
        vcov_amce_tau <- matrix(c(var_uncorrected_amce, cov_amce_tau,
                                  cov_amce_tau,        var_tau), nrow = 2)
        sim_amce <- as.data.frame(MASS::mvrnorm(.n_sims, c(amce_uncorrected, tau), vcov_amce_tau)) |>
          rlang::set_names(c("amce_uncorrected","tau")) |>
          dplyr::mutate(amce_corrected = amce_uncorrected / (1 - 2 * tau))
      }
      
      output <- data.frame(
        estimand = c("amce_uncorrected","amce_corrected"),
        estimate = c(amce_uncorrected, mean(sim_amce$amce_corrected)),
        se       = c(sqrt(pmax(var_uncorrected_amce, 0)), stats::sd(sim_amce$amce_corrected))
      ) |>
        dplyr::mutate(conf.low  = estimate - critical_t * se,
                      conf.high = estimate + critical_t * se,
                      tau = tau)
    }
    
    if (se_method == "bootstrap"){
      
      IDs_2 <- unique(stats::na.omit(data_for_estimand$id))
      # Only define IDs_1 if we’ll estimate IRR
      if (is.null(.irr)) {
        IDs_1 <- unique(stats::na.omit(data_for_irr$id))
        if (length(IDs_1) == 0L) stop("No rows available to bootstrap IRR/tau. Consider supplying .irr directly.")
      }
      
      out <- NULL
      for (i in seq_len(.n_boot)) {
        
        # Always bootstrap the estimand stage
        id_2 <- sample(IDs_2, length(IDs_2), replace = TRUE)
        bs_sample_2 <- data.frame(id = id_2) |>
          dplyr::left_join(data_for_estimand, by = "id", relationship = "many-to-many")
        
        # Bootstrap (and fit) IRR only when needed
        if (is.null(.irr)) {
          id_1 <- sample(IDs_1, length(IDs_1), replace = TRUE)
          bs_sample_1 <- data.frame(id = id_1) |>
            dplyr::left_join(data_for_irr, by = "id", relationship = "many-to-many")
          
          reg_irr <- estimatr::lm_robust(
            agree ~ 1,
            weights  = if (!rlang::quo_is_null(weights1_quo))  rlang::eval_tidy(weights1_quo,  bs_sample_1) else NULL,
            clusters = if (!rlang::quo_is_null(clusters1_quo)) rlang::eval_tidy(clusters1_quo, bs_sample_1) else NULL,
            se_type  = .se_type_1,
            data     = bs_sample_1
          ) |> estimatr::tidy()
          
          irr_bs <- reg_irr$estimate[1]
          tau_bs <- (1 - sqrt(1 - 2 * (1 - irr_bs))) / 2
        } else {
          tau_bs <- (1 - sqrt(1 - 2 * (1 - .irr))) / 2
        }
        
        reg_amce <- estimatr::lm_robust(
          selected ~ x,
          weights  = if (!rlang::quo_is_null(weights2_quo))  rlang::eval_tidy(weights2_quo,  bs_sample_2) else NULL,
          clusters = if (!rlang::quo_is_null(clusters2_quo)) rlang::eval_tidy(clusters2_quo, bs_sample_2) else NULL,
          se_type = se_type2_final,
          data = bs_sample_2
        ) |> estimatr::tidy()
        
        amce_uncorrected_bs <- reg_amce$estimate[2]
        amce_corrected_bs <- amce_uncorrected_bs / (1 - 2 * tau_bs)
        
        out <- dplyr::bind_rows(out,
                                data.frame(estimand = c("amce_corrected","amce_uncorrected"),
                                           estimate = c(amce_corrected_bs, amce_uncorrected_bs)))
      }
      
      output <- data.frame(
        estimand = c("amce_uncorrected","amce_corrected"),
        estimate = c(
          mean(out |> dplyr::filter(estimand == "amce_uncorrected") |> dplyr::pull(estimate)),
          mean(out |> dplyr::filter(estimand == "amce_corrected")   |> dplyr::pull(estimate))
        ),
        se = c(
          stats::sd(out |> dplyr::filter(estimand == "amce_uncorrected") |> dplyr::pull(estimate)),
          stats::sd(out |> dplyr::filter(estimand == "amce_corrected")   |> dplyr::pull(estimate))
        )
      ) |>
        dplyr::mutate(conf.low = estimate - critical_t * se,
                      conf.high = estimate + critical_t * se,
                      tau = tau)
    }
  }
  
  if (any(!is.finite(output$se))) {
    warning("Some standard errors are NA/Inf after corrections. ",
            "Consider se_type='stata' or bootstrap SEs.")
  }
  
  # Prefer the se_type captured from the finalized fit (AMCE first, then MM).
  se_type_used_final <-
    if (exists("se_type2_used", inherits = FALSE)) {
      se_type2_used
    } else if (exists("se_type2_used_mm", inherits = FALSE)) {
      se_type2_used_mm
    } else {
      se_type2_final  # fallback: what was requested
    }
  
  attr(output, "se_type_used") <- se_type_used_final
  attr(output, "cluster_by")   <- cluster_by2
  
  return(output)
}

#' Estimate profile- or choice-level effects (internal)
#'
#' Core worker used by \code{\link{projoint}} to compute marginal means (MMs)
#' or average marginal component effects (AMCEs) under either the profile- or
#' choice-level structure. Handles IRR usage (estimated or fixed) and the
#' requested standard-error method.
#'
#' @param .data A \code{\link{projoint_data}} object.
#' @param .qoi Optional \code{\link{projoint_qoi}}; if \code{NULL}, estimates all
#'   MMs/AMCEs implied by the design. When supplied, overrides \code{.structure}
#'   and \code{.estimand}.
#' @param .structure Either \code{"profile_level"} or \code{"choice_level"}.
#' @param .estimand Either \code{"mm"} (marginal mean) or \code{"amce"} (average
#'   marginal component effect).
#' @param .se_method One of \code{"analytical"}, \code{"simulation"}, or
#'   \code{"bootstrap"}.
#' @param .irr \code{NULL} to estimate IRR from repeated tasks; numeric to fix IRR.
#' @param .remove_ties Logical; whether to drop tied responses (default \code{TRUE}).
#' @param .ignore_position Logical; choice-level only. Ignore left/right position?
#'   Default \code{TRUE}.
#' @param .n_sims Integer; required when \code{.se_method = "simulation"}.
#' @param .n_boot Integer; required when \code{.se_method = "bootstrap"}.
#' @param .weights_1,.clusters_1,.se_type_1 Arguments passed to
#'   \code{\link[estimatr]{lm_robust}} for IRR estimation. If \code{.se_type_1}
#'   is \code{NULL}, \emph{estimatr} defaults are used.
#' @param .weights_2,.clusters_2,.se_type_2 Arguments passed to
#'   \code{\link[estimatr]{lm_robust}} for MM/AMCE estimation. If \code{.se_type_2}
#'   is \code{NULL}, \emph{estimatr} defaults are used.
#' @param .auto_cluster Logical; if \code{TRUE} (default) and an \code{id} column
#'   is present while no clusters are provided, cluster automatically. Only applied
#'   when the corresponding \code{.se_type_*} is \code{NULL}.
#' @param .seed Optional integer; if supplied, sets a temporary RNG seed for
#'   simulation/bootstrap and restores prior state on exit.
#'
#' @return A \code{\link{projoint_results}} object containing:
#' \itemize{
#'   \item \code{estimates}: tibble of estimates (point estimate, SE, CI) with
#'         identifier columns (e.g., \code{att_level_*}).
#'   \item \code{estimand}: \code{"mm"} or \code{"amce"}.
#'   \item \code{structure}: \code{"profile_level"} or \code{"choice_level"}.
#'   \item \code{se_method}: SE computation method used.
#'   \item \code{irr}: character noting IRR usage (e.g., \code{"Estimated"} or
#'         \code{"Assumed (<value> )"}).
#'   \item \code{tau}: numeric \eqn{\tau} used to correct measurement error.
#'   \item \code{remove_ties}, \code{ignore_position}: flags echoed from inputs.
#'   \item \code{se_type_used}, \code{cluster_by}: details propagated from
#'         fitting calls (if available).
#'   \item \code{labels}, \code{data}: design labels and the analysis data
#'         passed through for downstream methods.
#' }
#'
#' @keywords internal
#' @seealso \code{\link{projoint}}, \code{\link{pj_estimate}}, \code{\link{organize_data}},
#'   \code{\link{projoint_results}}
projoint_level <- function(
    .data,
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
    .se_type_2,
    .auto_cluster = TRUE,
    .seed = NULL
){
  
  # check various settings --------------------------------------------------
  # Also see the checking in pj_estimate()
  
  structure <- rlang::arg_match0(.structure, c("choice_level", "profile_level"))
  estimand  <- rlang::arg_match0(.estimand, c("mm", "amce"))
  se_method <- rlang::arg_match0(.se_method, c("analytical", "simulation", "bootstrap"))
  
  if (!is(.data, "projoint_data")){
    stop("The .data argument must be of class `projoint_data` from the `reshape_projoint` function.")
  }
  
  if (!is.null(.qoi) && !is(.qoi, "projoint_qoi")){
    stop("The .qoi argument must be of class `projoint_qoi` from the `set_qoi` function.")
  }
  
  if (.se_method == "simulation" && is.null(.n_sims)) {
    stop("If SEs are calculated by simulation, .n_sims must be specified (not NULL).")
  }
  
  if (.se_method == "bootstrap" && is.null(.n_boot)) {
    stop("If SEs are calculated by bootstrap, .n_boot must be specified (not NULL).")
  }
  
  if (is.null(.qoi) && structure == "choice_level") {
    stop("The .qoi argument must be specified for choice-level analysis.") 
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
  
  # estimate all MMs or AMCEs -----------------------------------------------
  
  if (is.null(.qoi)){
    
    attribute_levels <- .data$labels$level_id
    
    out <- NULL
    se_type_used <- NULL
    cluster_by   <- NULL

    for (i in seq_along(attribute_levels)){
      
      attribute <- stringr::str_extract(attribute_levels[i], "^.+(?=:)")
      level     <- stringr::str_extract(attribute_levels[i], "(?<=:).+$")
      
      if (estimand == "mm"){
        
        temp1 <- pj_estimate(.data = .data,
                             
                             .structure = structure,
                             .estimand = estimand,
                             
                             .att_choose = attribute,
                             .lev_choose = level,
                             .att_notchoose = NULL, 
                             .lev_notchoose = NULL,
                             .att_choose_b = NULL, 
                             .lev_choose_b = NULL,
                             .att_notchoose_b = NULL, 
                             .lev_notchoose_b = NULL,
                             
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
                             .auto_cluster = .auto_cluster) |> 
          dplyr::mutate(att_level_choose = stringr::str_c(stringr::str_c(attribute, level, sep = ":"), collapse = " or "))
        
      } else {
        
        temp1 <- pj_estimate(.data = .data,
                             .structure = structure,
                             .estimand = estimand,
                             
                             .att_choose = attribute,
                             .lev_choose = level,
                             .att_notchoose = NULL, 
                             .lev_notchoose = NULL,
                             .att_choose_b = attribute, 
                             .lev_choose_b = "level1", # The default baseline is "level1"
                             .att_notchoose_b = NULL, 
                             .lev_notchoose_b = NULL,
                             
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
                             .auto_cluster = .auto_cluster) |> 
          dplyr::mutate(att_level_choose = stringr::str_c(stringr::str_c(attribute, level, sep = ":"), collapse = " or "),
                        att_level_choose_baseline = stringr::str_c(stringr::str_c(attribute, "level1", sep = ":"), collapse = " or ")
          )
        
      }
      
      # capture once, from the first call
      if (is.null(se_type_used)) {
        se_type_used <- attr(temp1, "se_type_used", exact = TRUE)
        cluster_by   <- attr(temp1, "cluster_by",   exact = TRUE)
      }
      
      out <- dplyr::bind_rows(out, temp1)
      
    }
    
    if (estimand == "amce"){
      
      out <- out |> 
        dplyr::filter(att_level_choose != att_level_choose_baseline)
      
    }
    
  } else{
    
    attribute_of_interest  <- .qoi$attribute_of_interest
    levels_of_interest     <- .qoi$levels_of_interest
    
    attribute_of_interest_0  <- .qoi$attribute_of_interest_0
    levels_of_interest_0     <- .qoi$levels_of_interest_0
    
    attribute_of_interest_baseline <- .qoi$attribute_of_interest_baseline
    levels_of_interest_baseline     <- .qoi$levels_of_interest_baseline
    
    attribute_of_interest_0_baseline <- .qoi$attribute_of_interest_0_baseline
    levels_of_interest_0_baseline     <- .qoi$levels_of_interest_0_baseline
    
    temp <- pj_estimate(.data = .data,
                        .structure = structure,
                        .estimand = estimand,
                        
                        .att_choose = attribute_of_interest,
                        .lev_choose = levels_of_interest,
                        .att_notchoose = attribute_of_interest_0, 
                        .lev_notchoose = levels_of_interest_0,
                        .att_choose_b = attribute_of_interest_baseline, 
                        .lev_choose_b = levels_of_interest_baseline,
                        .att_notchoose_b = attribute_of_interest_0_baseline, 
                        .lev_notchoose_b = levels_of_interest_0_baseline,
                        
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
    
    se_type_used <- attr(temp, "se_type_used", exact = TRUE)
    cluster_by   <- attr(temp, "cluster_by",   exact = TRUE)
    
    if (estimand == "mm"){
      
      out <- temp |> 
        dplyr::mutate(att_level_choose = stringr::str_c(stringr::str_c(attribute_of_interest, levels_of_interest, sep = ":"), collapse = " or "),
                      att_level_notchoose = stringr::str_c(stringr::str_c(attribute_of_interest_0, levels_of_interest_0, sep = ":"), collapse = " or "))
      
    } else{
      
      out <- temp |> 
        dplyr::mutate(att_level_choose = stringr::str_c(stringr::str_c(attribute_of_interest, levels_of_interest, sep = ":"), collapse = " or "),
                      att_level_notchoose = stringr::str_c(stringr::str_c(attribute_of_interest_0, levels_of_interest_0, sep = ":"), collapse = " or "),
                      att_level_choose_baseline = stringr::str_c(stringr::str_c(attribute_of_interest_baseline, levels_of_interest_baseline, sep = ":"), collapse = " or "),
                      att_level_notchoose_baseline = stringr::str_c(stringr::str_c(attribute_of_interest_0_baseline, levels_of_interest_0_baseline, sep = ":"), collapse = " or "))
      
    }
    
  }
  
  
  # Pull through meta from pj_estimate() result
  # se_type_used <- attr(out, "se_type_used", exact = TRUE)
  # cluster_by   <- attr(out, "cluster_by",   exact = TRUE)
  
  tau <- unique(out$tau)
  if (length(tau) != 1L) tau <- tau[1L]
  
  estimates <- out |> 
    dplyr::select(-tau) |> 
    dplyr::as_tibble()
  
  # return estimates --------------------------------------------------------
  
  if (!is.null(.irr)){
    irr <- stringr::str_c("Assumed (", .irr, ")")
  } else{
    irr <- "Estimated" 
  }
  
  if (.estimand == "mm"){
    
    if(is.null(.qoi)){
      return(
        projoint_results("estimand" = .estimand,
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
                         data = .data$data)
      )
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
                              data = .data$data)
      )
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
                              data = .data$data)
      )
      
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
                              data = .data$data)
      )
    }
    
  }
  
}

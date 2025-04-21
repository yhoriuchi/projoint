#' Estimate and Correct Marginal Means (MMs) or Average Marginal Component Effects (AMCEs)
#'
#' Internal function to estimate and correct MMs or AMCEs from a conjoint experiment, adjusting for measurement error bias. 
#'
#' @keywords internal
#' @import dplyr
#' @import rlang
#' @import estimatr
#' @importFrom MASS mvrnorm
#'
#' @param .data A \code{\link{projoint_data}} object produced by \code{reshape_projoint()} or \code{make_projoint_data()}.
#' @param .structure Either \code{"profile_level"} (default) or \code{"choice_level"}.
#' @param .estimand Either \code{"mm"} (marginal mean) or \code{"amce"} (average marginal component effect).
#' @param .att_choose A character string specifying the attribute of interest (for the level(s) chosen).
#' @param .lev_choose A character vector specifying the level(s) of interest (for the level(s) chosen). Length 1 for profile-level analysis; 1+ for choice-level analysis.
#' @param .att_notchoose A character string specifying the attribute of interest (for the level(s) not chosen). Required only for \code{choice_level}.
#' @param .lev_notchoose A character vector specifying the level(s) not chosen. Required only for \code{choice_level}.
#' @param .att_choose_b (AMCE only) A character string specifying the baseline attribute for comparison.
#' @param .lev_choose_b (AMCE only) A character vector specifying the baseline level(s) for comparison.
#' @param .att_notchoose_b (AMCE only, choice-level only) A character string specifying the baseline attribute for the not-chosen profile.
#' @param .lev_notchoose_b (AMCE only, choice-level only) A character vector specifying the baseline level(s) for the not-chosen profile.
#' @param .se_method Method for standard error estimation: one of \code{"analytical"} (default), \code{"simulation"}, or \code{"bootstrap"}.
#' @param .irr Numeric scalar specifying intra-respondent reliability (IRR). If \code{NULL} (default), it is estimated from repeated tasks.
#' @param .remove_ties Logical; whether to remove ties before estimation (default is \code{TRUE}).
#' @param .ignore_position Logical; if \code{TRUE} (default), profile position (left/right) is ignored. Only relevant for \code{choice_level}.
#' @param .n_sims Number of simulations to run if \code{se_method = "simulation"}.
#' @param .n_boot Number of bootstrap replications if \code{se_method = "bootstrap"}.
#'
#' @param .weights_1 (Optional) Bare (unquoted) name of a column specifying weights for IRR estimation (see \code{\link[estimatr]{lm_robust}}). Defaults to \code{NULL}.
#' @param .clusters_1 (Optional) Bare (unquoted) name of a column specifying clusters for IRR estimation. Defaults to \code{NULL}.
#' @param .se_type_1 Standard error type for IRR estimation; passed to \code{lm_robust()}. Default is \code{"classical"}.
#'
#' @param .weights_2 (Optional) Bare (unquoted) name of a column specifying weights for MM or AMCE estimation. Defaults to \code{NULL}.
#' @param .clusters_2 (Optional) Bare (unquoted) name of a column specifying clusters for MM or AMCE estimation. Defaults to \code{NULL}.
#' @param .se_type_2 Standard error type for MM or AMCE estimation; passed to \code{lm_robust()}. Default is \code{"classical"}.
#'
#' @return A data frame containing corrected and uncorrected estimates, standard errors, confidence intervals, and tau values.
#'
#' @details
#' 
#' For weighting and clustering:
#' \itemize{
#'   \item \strong{Bare names} must be provided (e.g., \code{weight_var} not \code{"weight_var"}).
#'   \item If unspecified, estimation proceeds without weights or clustering.
#' }
#' 
#' For marginal mean (MM) estimation, the outcome is the probability of selecting a profile or profile feature.
#' For AMCE estimation, the outcome is the change in probability associated with a change in feature level.
#'
#' The function automatically applies measurement error correction based on estimated or user-specified intra-respondent reliability (IRR).
#'
#' @seealso \code{\link{reshape_projoint}}, \code{\link{make_projoint_data}}, \code{\link{set_qoi}}, \code{\link{projoint}}

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
    .se_type_2
){
  
  # check various settings --------------------------------------------------
  
  structure <- rlang::arg_match0(.structure, c("choice_level", "profile_level"))
  estimand  <- rlang::arg_match0(.estimand, c("mm", "amce"))
  se_method <- rlang::arg_match0(.se_method, c("analytical", "simulation", "bootstrap"))
  
  if (structure == "choice_level" & is.null(.att_choose)){
    stop("The .att_choose argument must be specified.")
  }
  if (structure == "choice_level" & is.null(.lev_choose)){
    stop("The .att_choose argument must be specified.")
  }
  
  if (.structure == "profile_level"){
    
    if (.estimand == "mm"){
      if (!is.null(.att_notchoose)){
        stop("The .att_notchoose argument must be NULL.") 
      } 
      if (!is.null(.lev_notchoose)){
        stop("The .lev_notchoose argument must be NULL.") 
      } 
      if (!is.null(.att_choose_b)){
        stop("The .att_choose_b argument must be NULL.") 
      } 
      if (!is.null(.lev_choose_b)){
        stop("The .lev_choose_b argument must be NULL.") 
      } 
      if (!is.null(.att_notchoose_b)){
        stop("The .att_notchoose_b argument must be NULL.") 
      } 
      if (!is.null(.lev_notchoose_b)){
        stop("The .lev_notchoose_b argument must be NULL.") 
      } 
    } else {
      if (!is.null(.att_notchoose)){
        stop("The .att_notchoose argument must be NULL.") 
      } 
      if (!is.null(.lev_notchoose)){
        stop("The .lev_notchoose argument must be NULL.") 
      } 
      if (is.null(.att_choose_b)){
        stop("The .att_choose_b argument must be specified") 
      } 
      if (is.null(.lev_choose_b)){
        stop("The .lev_choose_b argument must be specified") 
      } 
      if (!is.null(.att_notchoose_b)){
        stop("The .att_notchoose_b argument must be NULL.") 
      } 
      if (!is.null(.lev_notchoose_b)){
        stop("The .lev_notchoose_b argument must be NULL.") 
      } 
    }
    
    
  } else {
    
    if (.estimand == "mm"){
      if (is.null(.att_notchoose)){
        stop("The .att_notchoose argument must be specified") 
      } 
      if (is.null(.lev_notchoose)){
        stop("The .lev_notchoose argument must be specified") 
      } 
      if (!is.null(.att_choose_b)){
        stop("The .att_choose_b argument must be NULL.") 
      } 
      if (!is.null(.lev_choose_b)){
        stop("The .lev_choose_b argument must be NULL.") 
      } 
      if (!is.null(.att_notchoose_b)){
        stop("The .att_notchoose_b argument must be NULL.") 
      } 
      if (!is.null(.lev_notchoose_b)){
        stop("The .lev_notchoose_b argument must be NULL.") 
      } 
    } else {
      if (is.null(.att_notchoose)){
        stop("The .att_notchoose argument must be specified") 
      } 
      if (is.null(.lev_notchoose)){
        stop("The .lev_notchoose argument must be specified") 
      } 
      if (is.null(.att_choose_b)){
        stop("The .att_choose_b argument must be specified") 
      } 
      if (is.null(.lev_choose_b)){
        stop("The .lev_choose_b argument must be specified") 
      } 
      if (is.null(.att_notchoose_b)){
        stop("The .att_notchoose_b argument must be specified") 
      } 
      if (is.null(.lev_notchoose_b)){
        stop("The .lev_notchoose_b argument must be specified") 
      } 
    }
    
  }
  
  
  if(!is.null(.irr) & !is.numeric(.irr) & length(.irr) == 1){
    stop("The .irr argument must be either a numeric scalar or NULL.")
  }
  
  if(!is.logical(.remove_ties)){
    stop("The .remove_ties argument must be either TRUE or FALSE.")
  }
  
  if (.structure == "profile_level" & !is.null(.ignore_position)){
    stop("The .ignore_position argument can be specified only when the .structure argument is choice_level.")
  }
  
  if (.structure == "choice_level" & is.null(.ignore_position)){
    stop("Specify the .ignore_position argument.")
  }
  
  if (.structure == "choice_level" & !is.null(.ignore_position) & !is.logical(.ignore_position)){
    stop("The .ignore_position argument must be either TRUE or FALSE.")
  }
  
  if(!is.null(.n_sims) & !is.numeric(.n_sims) & length(.n_sims) == 1){
    stop("The .n_sims argument must be either a numeric scalar or NULL.")
  }
  
  if(!is.null(.n_boot) & !is.numeric(.n_boot) & length(.n_boot) == 1){
    stop("The .n_boot argument must be either a numeric scalar or NULL.")
  }
  
  if(.se_method == "simulation" & is.null(.n_sims)){
    stop("Specify the .n_sims argument for simulation")
  }
  
  if(.se_method != "simulation" & !is.null(.n_sims)){
    stop("You cannot specify the .n_sims argument for analytical derivation or bootstrapping")
  }
  
  if(.se_method == "bootstrap" & is.null(.n_boot)){
    stop("Specify the .n_boot argument for bootstrapping")
  }
  
  if(.se_method != "bootstrap" & !is.null(.n_boot)){
    stop("You cannot specify the .n_boot argument for analytical derivation or simulation")
  }
  
  if (.structure == "choice_level" & .estimand == "mm" & .remove_ties == FALSE){
    stop("The .remove_ties argument should be TRUE to estimate choice-level MMs.")
  }
  
  
  # Evaluate weights and clusters -------------------------------------------
  
  clusters1_quo <- rlang::enquo(.clusters_1)
  weights1_quo <- rlang::enquo(.weights_1)
  
  clusters2_quo <- rlang::enquo(.clusters_2)
  weights2_quo <- rlang::enquo(.weights_2)

  # bind variables locally to the function ----------------------------------
  
  id <- NULL
  agree <- NULL
  x <- NULL
  cov <- NULL
  vcov <- NULL
  sd <- NULL
  estimate <- NULL
  cov_mm_tau <- NULL
  reg_tau <- NULL
  se <- NULL
  qoi <- NULL
  qoi_1 <- NULL
  qoi_2 <- NULL
  qoi_choose <- NULL
  qoi_choose_1 <- NULL
  qoi_choose_2 <- NULL
  qoi_notchoose <- NULL
  qoi_notchoose_1 <- NULL
  qoi_notchoose_2 <- NULL
  selected <- NULL
  selected_1 <- NULL
  selected_2 <- NULL
  
  # Organize data -----------------------------------------------------------
  
  if (estimand == "mm"){
    
    if (structure == "choice_level"){
      
      # specify the attributes and levels of interest
      attlev_choose    <- stringr::str_c(.att_choose,  ":", .lev_choose)
      attlev_notchoose <- stringr::str_c(.att_notchoose,  ":", .lev_notchoose)
      
      temp1 <- organize_data(.dataframe = .data$data,
                             .structure = structure,
                             .estimand = estimand,
                             .remove_ties,
                             .att_choose,
                             .lev_choose,
                             .att_notchoose,
                             .lev_notchoose)
      
      if (isTRUE(.ignore_position)){
        
        if (.att_choose == .att_notchoose){
          
          temp2 <- temp1$data_for_estimand |> 
            dplyr::mutate(selected = ifelse(qoi_1 %in% attlev_choose, selected_1, selected_2), 
                          qoi_choose = stringr::str_c(attlev_choose, collapse = ", "),
                          qoi_notchoose = stringr::str_c(attlev_notchoose, collapse = ", ")) |> 
            dplyr::select(-matches("_\\d$"))
          
        } else{
          
          temp2 <- temp1$data_for_estimand |> 
            dplyr::mutate(selected = ifelse(qoi_choose_1 %in% attlev_choose, selected_1, selected_2), 
                          qoi_choose = stringr::str_c(attlev_choose, collapse = ", "),
                          qoi_notchoose = stringr::str_c(attlev_notchoose, collapse = ", ")) |> 
            dplyr::select(-matches("_\\d$"))
          
        }
        
      } else{
        
        if (.att_choose == .att_notchoose){
          
          temp2 <- temp1$data_for_estimand |> 
            dplyr::filter(qoi_1 == attlev_notchoose & qoi_2 == attlev_choose) |> 
            dplyr::mutate(selected = selected_2, # If .ignore_position == FALSE, selected = 1 if the left profile is chosen
                          qoi_choose = stringr::str_c(attlev_choose, collapse = ", "),
                          qoi_notchoose = stringr::str_c(attlev_notchoose, collapse = ", ")) |> 
            dplyr::select(-matches("_\\d$"))
          
        } else{
          
          temp2 <- temp1$data_for_estimand |> 
            dplyr::filter(qoi_notchoose_1 == attlev_notchoose & qoi_choose_2 == attlev_choose) |> 
            
            dplyr::mutate(selected = selected_2, 
                          qoi_choose = stringr::str_c(attlev_choose, collapse = ", "),
                          qoi_notchoose = stringr::str_c(attlev_notchoose, collapse = ", ")) |> 
            dplyr::select(-matches("_\\d$"))
          
        }
      }
      
      # data to estimate irr
      data_for_irr <- temp1$data_for_irr |> 
        dplyr::distinct()
      
      # data to estimate QoI
      data_for_estimand <- temp2
      
    } else {
      
      .list <- organize_data(.dataframe = .data$data,
                             .structure = structure,
                             .estimand = estimand,
                             .remove_ties,
                             .att_choose,
                             .lev_choose,
                             .att_notchoose,
                             .lev_notchoose)
      
      # save two data frames
      
      data_for_irr      <- .list$data_for_irr
      data_for_estimand <- .list$data_for_estimand
      
    }
    
  } else {
    
    if (structure == "choice_level"){
      
      # NOT baseline ------------------------------------------------------------
      
      # specify the attributes and levels of interest
      attlev_choose    <- stringr::str_c(.att_choose,  ":", .lev_choose)
      attlev_notchoose <- stringr::str_c(.att_notchoose,  ":", .lev_notchoose)
      
      temp1 <- organize_data(.dataframe = .data$data,
                             .structure = structure,
                             .estimand = estimand,
                             .remove_ties,
                             .att_choose,
                             .lev_choose,
                             .att_notchoose,
                             .lev_notchoose)
      
      if (isTRUE(.ignore_position)){
        
        if (.att_choose == .att_notchoose){
          
          temp2 <- temp1$data_for_estimand |> 
            dplyr::mutate(selected = ifelse(qoi_1 %in% attlev_choose, selected_1, selected_2), 
                          qoi_choose = str_c(attlev_choose, collapse = ", "),
                          qoi_notchoose = str_c(attlev_notchoose, collapse = ", ")) |> 
            dplyr::select(-matches("_\\d$"))
          
        } else{
          
          temp2 <- temp1$data_for_estimand |> 
            dplyr::mutate(selected = ifelse(qoi_choose_1 %in% attlev_choose, selected_1, selected_2), 
                          qoi_choose = str_c(attlev_choose, collapse = ", "),
                          qoi_notchoose = str_c(attlev_notchoose, collapse = ", ")) |> 
            dplyr::select(-matches("_\\d$"))
          
        }
        
      } else{
        
        if (.att_choose == .att_notchoose){
          
          temp2 <- temp1$data_for_estimand |> 
            dplyr::filter(qoi_1 == attlev_notchoose & qoi_2 == attlev_choose) |> 
            dplyr::mutate(selected = selected_2, # If .ignore_position == FALSE, selected = 1 if the left profile is chosen
                          qoi_choose = str_c(attlev_choose, collapse = ", "),
                          qoi_notchoose = str_c(attlev_notchoose, collapse = ", ")) |> 
            dplyr::select(-matches("_\\d$"))
          
        } else{
          
          temp2 <- temp1$data_for_estimand |> 
            dplyr::filter(qoi_notchoose_1 == attlev_notchoose & qoi_choose_2 == attlev_choose) |> 
            dplyr::mutate(selected = selected_2, 
                          qoi_choose = str_c(attlev_choose, collapse = ", "),
                          qoi_notchoose = str_c(attlev_notchoose, collapse = ", ")) |> 
            dplyr::select(-matches("_\\d$"))
          
        }
      }
      
      
      # Baseline ----------------------------------------------------------------
      
      # specify the attributes and levels of interest
      attlev_choose_b    <- stringr::str_c(.att_choose_b,  ":", .lev_choose_b)
      attlev_notchoose_b <- stringr::str_c(.att_notchoose_b,  ":", .lev_notchoose_b)
      
      temp1_b <- organize_data(.dataframe = .data$data,
                               .structure = structure,
                               .estimand = estimand,
                               .remove_ties,
                               .att_choose = .att_choose_b,
                               .lev_choose = .lev_choose_b,
                               .att_notchoose = .att_notchoose_b,
                               .lev_notchoose = .lev_notchoose_b)
      
      if (isTRUE(.ignore_position)){
        
        if (.att_choose_b == .att_notchoose_b){
          
          temp2_b <- temp1_b$data_for_estimand |> 
            dplyr::mutate(selected = ifelse(qoi_1 %in% attlev_choose, selected_1, selected_2), 
                          qoi_choose = str_c(attlev_choose, collapse = ", "),
                          qoi_notchoose = str_c(attlev_notchoose, collapse = ", ")) |> 
            dplyr::select(-matches("_\\d$"))
          
        } else{
          
          temp2_b <- temp1_b$data_for_estimand |> 
            dplyr::mutate(selected = ifelse(qoi_choose_1 %in% attlev_choose, selected_1, selected_2), 
                          qoi_choose = str_c(attlev_choose, collapse = ", "),
                          qoi_notchoose = str_c(attlev_notchoose, collapse = ", ")) |> 
            dplyr::select(-matches("_\\d$"))
          
        }
        
      } else{
        
        if (.att_choose_b == .att_notchoose_b){
          
          temp2_b <- temp1_b$data_for_estimand |> 
            dplyr::filter(qoi_1 == attlev_notchoose & qoi_2 == attlev_choose) |> 
            dplyr::mutate(selected = selected_2, # If .ignore_position == FALSE, selected = 1 if the left profile is chosen
                          qoi_choose = str_c(attlev_choose, collapse = ", "),
                          qoi_notchoose = str_c(attlev_notchoose, collapse = ", ")) |> 
            dplyr::select(-matches("_\\d$"))
          
        } else{
          
          temp2_b <- temp1_b$data_for_estimand |> 
            dplyr::filter(qoi_notchoose_1 == attlev_notchoose & qoi_choose_2 == attlev_choose) |> 
            
            dplyr::mutate(selected = selected_2, 
                          qoi_choose = str_c(attlev_choose, collapse = ", "),
                          qoi_notchoose = str_c(attlev_notchoose, collapse = ", ")) |> 
            dplyr::select(-matches("_\\d$"))
          
        }
      }
      
      # Merge -------------------------------------------------------------------
      
      # merge data to estimate irr
      data_for_irr <- bind_rows(
        temp1$data_for_irr,
        temp1_b$data_for_irr
      ) |>
        distinct()
      
      # merge data to estimate mm or amce
      data_for_estimand <- bind_rows(
        temp2 |> mutate(x = 1),
        temp2_b |> mutate(x = 0)
      )
      
    } else {
      
      temp1 <- organize_data(.dataframe = .data$data,
                             .structure = structure,
                             .estimand = estimand,
                             .remove_ties,
                             .att_choose,
                             .lev_choose,
                             .att_notchoose,
                             .lev_notchoose)
      temp1_b <- organize_data(.dataframe = .data$data,
                               .structure = structure,
                               .estimand = estimand,
                               .remove_ties,
                               .att_choose = .att_choose_b,
                               .lev_choose = .lev_choose_b,
                               .att_notchoose = .att_notchoose_b,
                               .lev_notchoose = .lev_notchoose_b)
      
      # merge data to estimate irr
      data_for_irr <- bind_rows(
        temp1$data_for_irr,
        temp1_b$data_for_irr
      ) |>
        distinct()
      
      # merge data to estimate mm or amce
      data_for_estimand <- bind_rows(
        temp1$data_for_estimand |> mutate(x = 1),
        temp1_b$data_for_estimand |> mutate(x = 0)
      )
      
    }
    
  }
  
  # Estimate or specify tau -------------------------------------------------
  
  if (is.null(.irr)) {
    
    reg_irr <- estimatr::lm_robust(
      agree ~ 1,
      weights  = if (!rlang::quo_is_null(weights1_quo)) rlang::eval_tidy(weights1_quo, data_for_irr) else NULL,
      clusters = if (!rlang::quo_is_null(clusters1_quo)) rlang::eval_tidy(clusters1_quo, data_for_irr) else NULL,
      se_type = .se_type_1,
      data = data_for_irr
    ) |> estimatr::tidy()
    
    irr_raw <- reg_irr$estimate[1]
    
    if (is.na(irr_raw)) {
      warning("Estimated IRR is NA. Using fallback IRR = 0.75.")
      irr <- 0.75
    } else {
      irr <- irr_raw
      if (irr < 0.5 || irr > 1) {
        warning(glue::glue("Estimated IRR was {round(irr_raw, 3)} and clipped to [0.5, 1]."))
      }
      irr <- min(max(irr, 0.5), 1)
    }
    
    var_irr <- reg_irr$std.error[1]^2
    
    tau     <- if (irr >= 1 - 1e-8) {
      0
    } else if (irr <= 1e-8) {
      0.5
    } else {
      (1 - sqrt(1 - 2 * (1 - irr))) / 2
    }
    
    var_tau <- if (irr >= 1 - 1e-8 || irr <= 1e-8) {
      0
    } else {
      0.25 * (2 * irr - 1)^(-1) * var_irr
    }
    
  } else if (!is.null(.irr) & !is.numeric(.irr)) {
    
    stop(".irr should be numeric.")
    
  } else {
    
    irr <- .irr
    tau <- (1 - sqrt(1 - 2 * (1 - irr))) / 2
    var_tau <- 0
    
  }
  
  
  # Estimate MM or AMCE (uncorrected) ---------------------------------------
  
  if (estimand == "mm"){
    
    reg_mm <- estimatr::lm_robust(
      selected ~ 1,
      weights  = if (!rlang::quo_is_null(weights2_quo)) rlang::eval_tidy(weights2_quo, data_for_estimand) else NULL,
      clusters = if (!rlang::quo_is_null(clusters2_quo)) rlang::eval_tidy(clusters2_quo, data_for_estimand) else NULL,
      se_type = .se_type_2,
      data = data_for_estimand
    ) |> 
      estimatr::tidy()
    
    # the critical t-value
    critical_t <- abs((reg_mm$conf.low[1] - reg_mm$estimate[1]) / reg_mm$std.error[1])
    
    mm_uncorrected <-  reg_mm$estimate[1]
    var_uncorrected_mm <- reg_mm$std.error[1]^2
    
  } 
  
  if (estimand == "amce") {
    
    reg_amce  <- estimatr::lm_robust(
      selected ~ x, 
      weights  = if (!rlang::quo_is_null(weights2_quo)) rlang::eval_tidy(weights2_quo, data_for_estimand) else NULL,
      clusters = if (!rlang::quo_is_null(clusters2_quo)) rlang::eval_tidy(clusters2_quo, data_for_estimand) else NULL,
      se_type = .se_type_2,
      data = data_for_estimand
    ) |> 
      estimatr::tidy()
    
    # the critical t-value
    critical_t <- abs((reg_amce$conf.low[2] - reg_amce$estimate[2]) / reg_amce$std.error[2])
    
    amce_uncorrected <-  reg_amce$estimate[2]
    var_uncorrected_amce <- reg_amce$std.error[2]^2
    
  } 
  
  # Calculate the covariance ------------------------------------------------
  
  # Keep the observations with both "selected" and "agree"
  
  data_for_cov <- data_for_estimand |> 
    dplyr::filter(!is.na(agree))
  
  # if (is.null(irr)){
  #   data_for_cov <- data_for_estimand |> 
  #     dplyr::filter(!is.na(agree))
  # } else{
  #   data_for_cov <- data_for_estimand
  # }
  
  if (estimand == "mm"){
    
    if (is.null(.irr)){
      
      cov_mm_irr  <- cov(data_for_cov$selected, data_for_cov$agree) / nrow(data_for_irr) 
      
    } else {
      
      cov_mm_irr <- 0
      
    }
    
    cov_mm_tau  <- -0.5 * (2 * irr - 1)^(-1/2) * cov_mm_irr
    
  } 
  
  if (estimand == "amce") {
    
    d_cov0 <- data_for_cov |> filter(x == 0)
    d_cov1 <- data_for_cov |> filter(x == 1)
    
    if (is.null(.irr)){
      
      cov_mm0_irr  <- cov(d_cov0$selected, d_cov0$agree) / nrow(data_for_irr)
      cov_mm1_irr  <- cov(d_cov1$selected, d_cov1$agree) / nrow(data_for_irr)
      
    } else {
      
      cov_mm0_irr  <- 0
      cov_mm1_irr  <- 0
      
    }
    
    cov_mm0_tau  <- -0.5 * (2 * irr - 1)^(-1/2) * cov_mm0_irr
    cov_mm1_tau  <- -0.5 * (2 * irr - 1)^(-1/2) * cov_mm1_irr
    cov_amce_tau <- cov_mm1_tau - cov_mm0_tau
    
  }
  
  # estimate and correct MMs ------------------------------------------------
  
  if (estimand == "mm"){
    
    if (se_method == "analytical"){
      
      # corrected the estimate
      mm_corrected   <- (mm_uncorrected - tau) / (1 - 2 * tau) 
      
      # analytically calculate variances
      var_corrected_mm <- 
        (mm_corrected^2 / (1 - (2 * tau))^2) * 
        (
          ((var_uncorrected_mm + var_tau - 2 * cov_mm_tau) / mm_corrected^2)
          + 4 * (cov_mm_tau - var_tau) / mm_corrected
          + 4 * var_tau
        )
      
      # return
      output <- data.frame("estimand" = c("mm_uncorrected", "mm_corrected"),
                           "estimate" = c(mm_uncorrected, mm_corrected),
                           "se" = c(sqrt(var_uncorrected_mm),
                                    sqrt(var_corrected_mm))) |> 
        dplyr::mutate("conf.low" = estimate - critical_t * se,
                      "conf.high" = estimate + critical_t * se,
                      "tau" = tau)
      
    } 
    
    if (se_method == "simulation"){
      
      # calculate the variance-covariance matrix
      vcov_mm_tau  <- matrix(c(var_uncorrected_mm,
                               cov_mm_tau,
                               cov_mm_tau,
                               var_tau),
                             nrow = 2)
      
      # run simulation
      sim_mm   <- as.data.frame(MASS::mvrnorm(.n_sims, 
                                              c(mm_uncorrected, tau), 
                                              vcov_mm_tau)) |>
        rlang::set_names(c("mm_uncorrected", "tau")) |>
        
        # corrected the estimate
        dplyr::mutate(mm_corrected = (mm_uncorrected - tau) / (1 - 2 * tau))
      
      # return
      output <- data.frame("estimand" = c("mm_uncorrected", "mm_corrected"),
                           "estimate" = c(mm_uncorrected, mean(sim_mm$mm_corrected)),
                           "se" = c(sqrt(var_uncorrected_mm), sd(sim_mm$mm_corrected))) |> 
        dplyr::mutate("conf.low" = estimate - critical_t * se,
                      "conf.high" = estimate + critical_t * se,
                      "tau" = tau)
      
    }  
    
    if (se_method == "bootstrap"){
      
      # save a vector of respondent IDs
      IDs_1 <- unique(data_for_irr$id)
      IDs_2 <- unique(data_for_estimand$id)
      
      out <- NULL
      
      for (i in 1:.n_boot){
        
        # randomly sample respondent IDs
        set.seed(i)
        id_1 <- sample(IDs_1, length(IDs_1), replace = TRUE)
        id_2 <- sample(IDs_2, length(IDs_2), replace = TRUE)
        
        # generate a bootstrapped data frame
        bs_sample_1 <- data.frame(id = id_1) |> 
          dplyr::left_join(data_for_irr, 
                           by = "id", 
                           relationship = "many-to-many")
        bs_sample_2 <- data.frame(id = id_2) |> 
          dplyr::left_join(data_for_estimand, 
                           by = "id", 
                           relationship = "many-to-many")
        
        # run intercept-only regression models
        reg_irr <- estimatr::lm_robust(agree ~ 1, 
                                       weights  = if (!rlang::quo_is_null(weights1_quo)) rlang::eval_tidy(weights1_quo, bs_sample_1) else NULL,
                                       clusters = if (!rlang::quo_is_null(clusters1_quo)) rlang::eval_tidy(clusters1_quo, bs_sample_1) else NULL,
                                       se_type = .se_type_1,
                                       data = bs_sample_1) |> 
          estimatr::tidy()
        
        reg_mm  <- estimatr::lm_robust(selected ~ 1, 
                                       weights  = if (!rlang::quo_is_null(weights2_quo)) rlang::eval_tidy(weights2_quo, bs_sample_2) else NULL,
                                       clusters = if (!rlang::quo_is_null(clusters2_quo)) rlang::eval_tidy(clusters2_quo, bs_sample_2) else NULL,
                                       se_type = .se_type_2,
                                       data = bs_sample_2) |> 
          estimatr::tidy()
        
        # calculate the means
        mm_uncorrected <-  reg_mm$estimate[1]
        
        if (is.null(.irr)){
          
          irr <- reg_irr$estimate[1]
          tau <- (1 - sqrt(1 - 2 * (1 - irr))) / 2
          
        } else {
          
          tau <- (1 - sqrt(1 - 2 * (1 - .irr))) / 2
          
        }
        
        # corrected the estimate
        mm_corrected   <- (mm_uncorrected - tau) / (1 - 2 * tau) 
        
        # save a temporary data frame
        temp <- data.frame("estimand" = c("mm_corrected", "mm_uncorrected"),
                           "estimate" = c(mm_corrected, mm_uncorrected))
        
        # add the data frame to "out"
        out <- dplyr::bind_rows(out, temp)
        
      }
      
      # return
      output <- data.frame("estimand" = c("mm_uncorrected", "mm_corrected"),
                           "estimate" = c(mean(out |> filter(estimand == "mm_uncorrected") |> pull(estimate)), 
                                          mean(out |> filter(estimand == "mm_corrected") |> pull(estimate))),
                           "se"       = c(sd(out |> filter(estimand == "mm_uncorrected") |> pull(estimate)), 
                                          sd(out |> filter(estimand == "mm_corrected") |> pull(estimate)))) |> 
        dplyr::mutate("conf.low" = estimate - critical_t * se,
                      "conf.high" = estimate + critical_t * se,
                      "tau" = tau)
    }
    
  } 
  
  # estimate and correct AMCEs ----------------------------------------------
  
  if (estimand == "amce"){
    
    if (se_method == "analytical"){
      
      # corrected the estimate
      amce_corrected   <- amce_uncorrected / (1 - 2 * tau)
      
      # analytically calculate variances
      var_corrected_amce <- 
        (amce_corrected^2 / (1 - (2 * tau))^2) * 
        (
          (var_uncorrected_amce / amce_corrected^2) 
          + 4 * cov_amce_tau / amce_corrected
          + 4 * var_tau
        )
      
      # return
      output <- data.frame("estimand" = c("amce_uncorrected", "amce_corrected"),
                           "estimate" = c(amce_uncorrected, amce_corrected),
                           "se" = c(sqrt(var_uncorrected_amce),
                                    sqrt(var_corrected_amce))) |> 
        dplyr::mutate("conf.low" = estimate - critical_t * se,
                      "conf.high" = estimate + critical_t * se,
                      "tau" = tau)
      
    } 
    
    if (se_method == "simulation"){
      
      # calculate the variance-covariance matrix
      vcov_amce_tau  <- matrix(c(var_uncorrected_amce,
                                 cov_amce_tau,
                                 cov_amce_tau,
                                 var_tau),
                               nrow = 2)
      
      # run simulation
      sim_amce   <- as.data.frame(MASS::mvrnorm(.n_sims, 
                                                c(amce_uncorrected, tau), 
                                                vcov_amce_tau)) |>
        rlang::set_names(c("amce_uncorrected", "tau")) |>
        
        # corrected the estimate
        dplyr::mutate(amce_corrected = amce_uncorrected / (1 - 2 * tau))
      
      # return
      output <- data.frame("estimand" = c("amce_uncorrected", "amce_corrected"),
                           "estimate" = c(amce_uncorrected, mean(sim_amce$amce_corrected)),
                           "se" = c(sqrt(var_uncorrected_amce), sd(sim_amce$amce_corrected))) |> 
        dplyr::mutate("conf.low" = estimate - critical_t * se,
                      "conf.high" = estimate + critical_t * se,
                      "tau" = tau)
      
    } 
    
    if (se_method == "bootstrap"){
      
      # save a vector of respondent IDs
      IDs_1 <- unique(data_for_irr$id)
      IDs_2 <- unique(data_for_estimand$id)
      
      out <- NULL
      
      for (i in 1:.n_boot){
        
        # randomly sample respondent IDs
        set.seed(i)
        id_1 <- sample(IDs_1, length(IDs_1), replace = TRUE)
        id_2 <- sample(IDs_2, length(IDs_2), replace = TRUE)
        
        # generate a bootstrapped data frame
        bs_sample_1 <- data.frame(id = id_1) |> 
          dplyr::left_join(data_for_irr, 
                           by = "id", 
                           relationship = "many-to-many")
        bs_sample_2 <- data.frame(id = id_2) |> 
          dplyr::left_join(data_for_estimand, 
                           by = "id", 
                           relationship = "many-to-many")
        
        # run intercept-only regression models
        reg_irr <- estimatr::lm_robust(agree ~ 1, 
                                       weights  = if (!rlang::quo_is_null(weights1_quo)) rlang::eval_tidy(weights1_quo, bs_sample_1) else NULL,
                                       clusters = if (!rlang::quo_is_null(clusters1_quo)) rlang::eval_tidy(clusters1_quo, bs_sample_1) else NULL,
                                       se_type = .se_type_1,
                                       data = bs_sample_1) |> 
          estimatr::tidy()
        
        reg_amce <- estimatr::lm_robust(selected ~ x, 
                                         weights  = if (!rlang::quo_is_null(weights2_quo)) rlang::eval_tidy(weights2_quo, bs_sample_2) else NULL,
                                         clusters = if (!rlang::quo_is_null(clusters2_quo)) rlang::eval_tidy(clusters2_quo, bs_sample_2) else NULL,
                                         se_type = .se_type_2,
                                         bs_sample_2) |> 
          estimatr::tidy()        
        
        # calculate the means
        amce_uncorrected <-  reg_amce$estimate[2]
        if (is.null(.irr)){
          
          irr <- reg_irr$estimate[1]
          tau <- (1 - sqrt(1 - 2 * (1 - irr))) / 2
          
        } else {
          
          tau <- (1 - sqrt(1 - 2 * (1 - .irr))) / 2
          
        }
        
        # corrected the estimate
        amce_corrected   <- amce_uncorrected / (1 - 2 * tau) 
        
        # save a temporary data frame
        temp <- data.frame("estimand" = c("amce_corrected", "amce_uncorrected"),
                           "estimate" = c(amce_corrected, amce_uncorrected))
        
        # add the data frame to "out"
        out <- dplyr::bind_rows(out, temp)
        
      }
      
      # return
      output <- data.frame("estimand" = c("amce_uncorrected", "amce_corrected"),
                           "estimate" = c(mean(out |> filter(estimand == "amce_uncorrected") |> pull(estimate)), 
                                          mean(out |> filter(estimand == "amce_corrected") |> pull(estimate))),
                           "se"       = c(sd(out |> filter(estimand == "amce_uncorrected") |> pull(estimate)), 
                                          sd(out |> filter(estimand == "amce_corrected") |> pull(estimate)))) |> 
        dplyr::mutate("conf.low" = estimate - critical_t * se,
                      "conf.high" = estimate + critical_t * se,
                      "tau" = tau)
      
    }
  }  
  
  # return the output -------------------------------------------------------
  
  return(output)
  
} 
#' Analyze a conjoint data set and correct for measurement error
#'
#' This is the internal function used to calculate and correct marginal means or average marginal component effects of a conjoint design.
#'
#' @import dplyr
#' @import rlang
#' @import estimatr
#' @importFrom MASS mvrnorm
#' @importFrom methods is
#' @importFrom methods new
#' @keywords internal
#' @param .data A `projoint_data` object
#' @param .qoi A `projoint_qoi` object. If NULL, defaults to producing all MMs and all AMCEs.
#' @param .by_var A dichotomous variable (character) used for subgroup analysis
#' @param .structure Either "profile_level" (default) or "choice_level" 
#' @param .estimand Either "mm" for marginal mean or "amce" for average marginal component effect
#' @param .se_method c("analytic", "simulation", "bootstrap") description
#' @param .irr NULL (default) if IRR is to be calculated using the repeated task. Otherwise, a numerical value
#' @param .remove_ties Logical: should ties be removed before estimation? Defaults to TRUE.
#' @param .ignore_position TRUE (default) if you ignore the location of profile (left or right. Relevant only if analyzed at the choice level
#' @param .n_sims The number of simulations. Relevant only if .se_method == "simulation" 
#' @param .n_boot The number of bootstrapped samples. Relevant only if .se_method == "bootstrap"
#' @param .weights_1 the weight to estimate IRR (see `lm_robust()`): NULL (default)
#' @param .clusters_1 the clusters to estimate IRR (see `lm_robust()`): NULL (default)
#' @param .se_type_1 the standard error type to estimate IRR (see `lm_robust()`): "classical" (default)
#' @param .weights_2 the weight to estimate MM or AMCE (see `lm_robust()`): NULL (default)
#' @param .clusters_2 the clusters to estimate MM or AMCE (see `lm_robust()`): NULL (default)
#' @param .se_type_2 the standard error type to estimate MM or AMCE (see `lm_robust()`): "classical" (default)
#' @return A `projoint_results` object

projoint_diff <- function(
    .data,
    .qoi = NULL,
    .by_var,
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
  
  # bind variables locally to the function ----------------------------------
  
  estimand <- NULL
  attribute <- NULL
  level <- NULL
  estimate <- NULL
  estimate_1 <- NULL
  estimate_0 <- NULL
  se <- NULL
  se_1 <- NULL
  se_0 <- NULL
  att_level_choose <- NULL
  att_level_notchoose <- NULL
  att_level_choose_baseline <- NULL
  att_level_notchoose_baseline <- NULL
  irr <- NULL
  
  # estimate QoIs by subgroups ----------------------------------------------
  
  subgroup1 <- .data@data %>% filter(.data[[.by_var]] == 1)
  subgroup0 <- .data@data %>% filter(.data[[.by_var]] == 0)
  
  data1 <-  projoint_data("labels" = .data@labels, "data" = subgroup1)
  data0 <-  projoint_data("labels" = .data@labels, "data" = subgroup0)
  
  out1 <- projoint_level(.data = data1,
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
  
  out0 <- projoint_level(.data = data0,
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
  
  # prepare to return the estimates -----------------------------------------
  
  estimate1 <- out1@estimates %>% 
    dplyr::select(estimand, att_level_choose,
                  "estimate_1" = estimate,
                  "se_1" = se) %>% 
    dplyr::mutate(tau = out1@tau)
  
  estimate0 <- out0@estimates %>% 
    dplyr::select(estimand, att_level_choose,
                  "estimate_0" = estimate,
                  "se_0" = se)
  
  estimates <- estimate1 %>% 
    dplyr::left_join(estimate0, by = c("estimand", "att_level_choose")) %>% 
    mutate(estimate = estimate_1 - estimate_0,
           se = sqrt(se_1^2 + se_0^2), 
           conf.low = estimate - 1.96 * se,
           conf.high = estimate + 1.96 * se) 
  
  tau <- data.frame("tau1" = out1@tau,
                    "tau0" = out0@tau)

  # return estimates --------------------------------------------------------
  
  if (is.null(.irr)){
    irr <- str_c("Assumed (", .irr, ")")
  } else{
    irr <- "Estimated" 
  }
  
  if (.estimand == "mm"){
    
    if(is.null(.qoi)){
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
                       labels = .data@labels,
                       data = .data@data) %>%
        return()
    } else {
      projoint_results("estimand" = .estimand,
                       "structure" = .structure,
                       "estimates" = estimates, 
                       "se_method" = .se_method,
                       "irr" = irr,
                       "tau" = tau,
                       "remove_ties" = .remove_ties,
                       "ignore_position" = .ignore_position,
                       "attribute_of_interest" = .qoi@attribute_of_interest,
                       "levels_of_interest" = .qoi@levels_of_interest,
                       "attribute_of_interest_0" = .qoi@attribute_of_interest_0,
                       "levels_of_interest_0" = .qoi@levels_of_interest_0,
                       "attribute_of_interest_baseline" = NULL,
                       "levels_of_interest_baseline" = NULL,
                       "attribute_of_interest_0_baseline" = NULL,
                       "levels_of_interest_0_baseline" = NULL,
                       labels = .data@labels,
                       data = .data@data) %>%
        return()
    }
    
  } else {
    
    if(is.null(.qoi)){
      projoint_results("estimand" = .estimand,
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
                       labels = .data@labels,
                       data = .data@data) %>%
        return()
    } else {
      projoint_results("estimand" = .estimand,
                       "structure" = .structure,
                       "estimates" = estimates, 
                       "se_method" = .se_method,
                       "irr" = irr,
                       "tau" = tau,
                       "remove_ties" = .remove_ties,
                       "ignore_position" = .ignore_position,
                       "attribute_of_interest" = .qoi@attribute_of_interest,
                       "levels_of_interest" = .qoi@levels_of_interest,
                       "attribute_of_interest_0" = .qoi@attribute_of_interest_0,
                       "levels_of_interest_0" = .qoi@levels_of_interest_0,
                       "attribute_of_interest_baseline" = .qoi@attribute_of_interest_baseline,
                       "levels_of_interest_baseline" = .qoi@levels_of_interest_baseline,
                       "attribute_of_interest_0_baseline" = .qoi@attribute_of_interest_0_baseline,
                       "levels_of_interest_0_baseline" = .qoi@levels_of_interest_0_baseline,
                       labels = .data@labels,
                       data = .data@data) %>%
        return()
    }
    
  }
}


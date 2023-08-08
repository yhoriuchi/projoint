#' Estimate and correct MMs or AMCEs
#'
#' This is the internal function used to calculate and correct marginal means or average marginal component effects of a conjoint design.
#'
#' @import dplyr
#' @import rlang
#' @import estimatr
#' @importFrom MASS mvrnorm
#' @keywords internal
#' @param .data A `projoint_data` object
#' @param .attribute A character column name identifying the attribute of interest
#' @param .level  A character vector identifying the levels of interest. Its length should be 1 for profile-level analysis and 2 for choice-level analysis
#' @param .structure A character identifying a data structure: "choice_level" (default) or "profile_level". Note: the right element of .level is the outcome of choice if .structure == "choice_level"
#' @param .estimand A character identifying the estimand: "mm" (default) or "amce"
#' @param .se_method  A character identifying the method of correcting measurement error bias: "analytical" (default), "simulation", or "bootstrap" (not recommended)
#' @param .irr NULL (default) if IRR is calculate using the repeated task. Otherwise, a numerical value
#' @param .baseline  A character vector identifying the baseline level. Its length should be 1 for profile-level analysis and 2 for choice-level analysis
#' @param .remove_ties TRUE (default) if you want to remove ties for the attribute of interest (in profile-level analysis)
#' @param .ignore_position TRUE if you ignore the location of profile (left or right). Relevant only if .structure == "choice_level". Defaults to NULL.
#' @param .n_sims The number of simulations. Relevant only if .se_method == "simulation" 
#' @param .n_boot The number of bootstrapped samples. Relevant only if .se_method == "bootstrap"
#' @param .weights_1 the weight to estimate IRR (see `lm_robust()`): NULL (default)
#' @param .clusters_1 the clusters to estimate IRR (see `lm_robust()`): NULL (default)
#' @param .se_type_1 the standard error type to estimate IRR (see `lm_robust()`): "classical" (default)
#' @param .weights_2 the weight to estimate MM or AMCE (see `lm_robust()`): NULL (default)
#' @param .clusters_2 the clusters to estimate MM or AMCE (see `lm_robust()`): NULL (default)
#' @param .se_type_2 the standard error type to estimate MM or AMCE (see `lm_robust()`): "classical" (default)
#' @return A data frame of estimates

# .data = out1
# .attribute = "att1"
# .level = "level2"
# .structure = "profile_level"
# .estimand = "amce"
# .se_method = "analytical"
# .irr = NULL
# .baseline = "level1"
# .remove_ties = TRUE
# .ignore_position = NULL
# .n_sims = NULL
# .n_boot = NULL
# .weights_1 <- NULL
# .clusters_1 <- NULL
# .se_type_1 <- "classical"
# .weights_2 <- NULL
# .clusters_2 <- NULL
# .se_type_2 <- "classical"

pj_estimate <- function(
    .data,
    .attribute,
    .level,
    .structure = "choice_level",
    .estimand = "mm",
    .se_method = "analytical",
    .irr = NULL,
    .baseline = NULL,
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
  
  .dataframe <- .data@data
  
  # bind variables locally to the function ----------------------------------
  
  id <- NULL
  selected <- NULL
  agree <- NULL
  x <- NULL
  cov <- NULL
  vcov <- NULL
  sd <- NULL
  estimate <- NULL
  cov_mm_tau <- NULL
  reg_tau <- NULL
  se <- NULL
  
  # check various settings --------------------------------------------------
  
  structure <- rlang::arg_match0(.structure, c("choice_level", "profile_level"))
  estimand  <- rlang::arg_match0(.estimand, c("mm", "amce"))
  se_method <- rlang::arg_match0(.se_method, c("analytical", "simulation", "bootstrap"))
  
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
    stop("Specify the .n_sims arguement for simulation")
  }
  
  if(.se_method != "simulation" & !is.null(.n_sims)){
    stop("You cannot specify the .n_sims arguement for analytical derivation or bootstrapping")
  }
  
  if(.se_method == "bootstrap" & is.null(.n_boot)){
    stop("Specify the .n_boot arguement for bootstrapping")
  }
  
  if(.se_method != "bootstrap" & !is.null(.n_boot)){
    stop("You cannot specify the .n_boot arguement for analytical derivation or simulation")
  }
  
  
  if (.structure == "choice_level" & .estimand == "mm" & .remove_ties == FALSE){
    stop("The .remove_ties argument should be TRUE to estimate choice-level MMs.")
  }
  
  if (.estimand == "mm" & !is.null(.baseline)){
    stop("The .baseline argument can be specified only when the .estimand argument is amce.")
  }
  
  if (.estimand == "amce" & is.null(.baseline)){
    stop("Specify .baseline argument for the estimation of AMCEs.")
  }
  
  
  # Organize data -----------------------------------------------------------
  
  if (.estimand == "mm"){
    
    if (.structure == "choice_level" & !is.null(.ignore_position) & isTRUE(.ignore_position)){
      
      temp1 <- organize_data(.dataframe,
                             .attribute,
                             .level,
                             .structure)
      temp2 <- organize_data(.dataframe,
                             .attribute,
                             rev(.level), # the order is reversed
                             .structure)
      
      # merge data to estimate irr
      data_for_irr <- bind_rows(
        temp1$data_for_irr,
        temp2$data_for_irr
      ) %>% 
        distinct()
      
      # merge data to estimate mm or amce
      data_for_estimand <- bind_rows(
        temp1$data_for_estimand,
        temp2$data_for_estimand %>% mutate(selected = 1 - selected)
      )
      
    } else {
      
      .list <- organize_data(.dataframe,
                             .attribute,
                             .level,
                             .structure,
                             .remove_ties)
      
      # save two data frames
      
      data_for_irr      <- .list$data_for_irr
      data_for_estimand <- .list$data_for_estimand
      
    }
  }
  
  
  if (.estimand == "amce"){
    
    if (.structure == "choice_level" & !is.null(.ignore_position) & isTRUE(.ignore_position)){
      
      temp1a <- organize_data(.dataframe,
                              .attribute,
                              .level,
                              .structure)
      temp2a <- organize_data(.dataframe,
                              .attribute,
                              .baseline, # this is the baseline
                              .structure)
      temp1b <- organize_data(.dataframe,
                              .attribute,
                              rev(.level),
                              .structure)
      temp2b <- organize_data(.dataframe,
                              .attribute,
                              rev(.baseline), # this is the baseline; the order is reversed
                              .structure)
      
      # merge data to estimate irr
      data_for_irr <- bind_rows(
        temp1a$data_for_irr,
        temp2a$data_for_irr,
        temp1b$data_for_irr,
        temp2b$data_for_irr
      ) %>%
        distinct()
      
      # merge data to estimate mm or amce
      data_for_estimand <- bind_rows(
        temp1a$data_for_estimand %>% mutate(x = 1),
        temp2a$data_for_estimand %>% mutate(x = 0),
        temp1b$data_for_estimand %>% mutate(x = 1),
        temp2b$data_for_estimand %>% mutate(x = 0)
      )
      
    } else {
      
      temp1 <- organize_data(.dataframe,
                             .attribute,
                             .level,
                             .structure,
                             .remove_ties)
      temp2 <- organize_data(.dataframe,
                             .attribute,
                             .baseline, # this is the baseline
                             .structure,
                             .remove_ties)
      
      # merge data to estimate irr
      data_for_irr <- bind_rows(
        temp1$data_for_irr,
        temp2$data_for_irr
      ) %>%
        distinct()
      
      # merge data to estimate mm or amce
      data_for_estimand <- bind_rows(
        temp1$data_for_estimand %>% mutate(x = 1),
        temp2$data_for_estimand %>% mutate(x = 0)
      )
      
    }
    
  }
  
  # Estimate or specify tau -------------------------------------------------
  
  if (is.null(.irr)){
    
    # run intercept-only regression models
    reg_irr <- estimatr::lm_robust(agree ~ 1, 
                                   weights = .weights_1, 
                                   clusters = .clusters_1,
                                   se_type = .se_type_1,
                                   data = data_for_irr) %>% tidy()
    
    irr     <- reg_irr$estimate[1]
    var_irr <- reg_irr$std.error[1]^2 
    
    tau     <- (1 - sqrt(1 - 2 * (1 - irr))) / 2
    var_tau <- 0.25 * (2 * irr - 1)^(-1) * var_irr
    
  } else if (!is.null(.irr) & !is.numeric(.irr)){
    
    stop(".irr should be numeric.")
    
  } else {
    
    irr <- .irr
    tau <- (1 - sqrt(1 - 2 * (1 - irr))) / 2
    var_tau <- 0
    
  }
  
  # Estimate MM or AMCE (uncorrected) ---------------------------------------
  
  if (estimand == "mm"){
    
    reg_mm  <- estimatr::lm_robust(selected ~ 1, 
                                   weights = .weights_2, 
                                   clusters = .clusters_2,
                                   se_type = .se_type_2,
                                   data = data_for_estimand) %>% tidy()
    
    # the critical t-value
    critical_t <- abs((reg_mm$conf.low[1] - reg_mm$estimate[1]) / reg_mm$std.error[1])
    
    mm_uncorrected <-  reg_mm$estimate[1]
    var_uncorrected_mm <- reg_mm$std.error[1]^2
    
  } 
  
  if (estimand == "amce") {
    
    reg_amce  <- estimatr::lm_robust(selected ~ x, 
                                     weights = .weights_2, 
                                     clusters = .clusters_2,
                                     se_type = .se_type_2,
                                     data = data_for_estimand) %>% tidy()
    
    # the critical t-value
    critical_t <- abs((reg_amce$conf.low[2] - reg_amce$estimate[2]) / reg_amce$std.error[2])
    
    amce_uncorrected <-  reg_amce$estimate[2]
    var_uncorrected_amce <- reg_amce$std.error[2]^2
    
  } 
  
  # Calculate the covariance ------------------------------------------------
  
  # Keep the observations with both "selected" and "agree"
  
  data_for_cov <- data_for_estimand %>% 
    filter(!is.na(agree))
  
  if (estimand == "mm"){
    
    if (is.null(.irr)){
      
      cov_mm_irr  <- cov(data_for_cov$selected, data_for_cov$agree) / nrow(data_for_irr) 
      
    } else {
      
      cov_mm_irr <- 0
      
    }
    
    cov_mm_tau  <- -0.5 * (2 * irr - 1)^(-1/2) * cov_mm_irr
    
  } 
  
  if (estimand == "amce") {
    
    d_cov0 <- data_for_cov %>% filter(x == 0)
    d_cov1 <- data_for_cov %>% filter(x == 1)
    
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
                                    sqrt(var_corrected_mm))) %>% 
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
                                              vcov_mm_tau)) %>%
        rlang::set_names(c("mm_uncorrected", "tau")) %>%
        
        # corrected the estimate
        dplyr::mutate(mm_corrected = (mm_uncorrected - tau) / (1 - 2 * tau))
      
      # return
      output <- data.frame("estimand" = c("mm_uncorrected", "mm_corrected"),
                           "estimate" = c(mm_uncorrected, mean(sim_mm$mm_corrected)),
                           "se" = c(sqrt(var_uncorrected_mm), sd(sim_mm$mm_corrected))) %>% 
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
        bs_sample_1 <- data.frame(id = id_1) %>% 
          dplyr::left_join(data_for_irr, 
                           by = "id", 
                           relationship = "many-to-many")
        bs_sample_2 <- data.frame(id = id_2) %>% 
          dplyr::left_join(data_for_estimand, 
                           by = "id", 
                           relationship = "many-to-many")
        
        # run intercept-only regression models
        reg_irr <- estimatr::lm_robust(agree ~ 1, 
                                       weights = .weights_1, 
                                       clusters = .clusters_1,
                                       se_type = .se_type_1,
                                       data = bs_sample_1) %>% tidy()
        
        reg_mm  <- estimatr::lm_robust(selected ~ 1, 
                                       weights = .weights_2, 
                                       clusters = .clusters_2,
                                       se_type = .se_type_2,
                                       data = bs_sample_2) %>% tidy()
        
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
                           "estimate" = c(mean(out %>% filter(estimand == "mm_uncorrected") %>% pull(estimate)), 
                                          mean(out %>% filter(estimand == "mm_corrected") %>% pull(estimate))),
                           "se"       = c(sd(out %>% filter(estimand == "mm_uncorrected") %>% pull(estimate)), 
                                          sd(out %>% filter(estimand == "mm_corrected") %>% pull(estimate)))) %>% 
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
                                    sqrt(var_corrected_amce))) %>% 
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
                                                vcov_amce_tau)) %>%
        rlang::set_names(c("amce_uncorrected", "tau")) %>%
        
        # corrected the estimate
        dplyr::mutate(amce_corrected = amce_uncorrected / (1 - 2 * tau))
      
      # return
      output <- data.frame("estimand" = c("amce_uncorrected", "amce_corrected"),
                           "estimate" = c(amce_uncorrected, mean(sim_amce$amce_corrected)),
                           "se" = c(sqrt(var_uncorrected_amce), sd(sim_amce$amce_corrected))) %>% 
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
        bs_sample_1 <- data.frame(id = id_1) %>% 
          dplyr::left_join(data_for_irr, 
                           by = "id", 
                           relationship = "many-to-many")
        bs_sample_2 <- data.frame(id = id_2) %>% 
          dplyr::left_join(data_for_estimand, 
                           by = "id", 
                           relationship = "many-to-many")
        
        # run intercept-only regression models
        reg_irr <- estimatr::lm_robust(agree ~ 1, 
                                       weights = .weights_1, 
                                       clusters = .clusters_1,
                                       se_type = .se_type_1,
                                       data = bs_sample_1) %>% tidy()
        
        reg_amce  <- estimatr::lm_robust(selected ~ x, 
                                         weights = .weights_2, 
                                         clusters = .clusters_2,
                                         se_type = .se_type_2,
                                         bs_sample_2) %>% tidy()
        
        
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
                           "estimate" = c(mean(out %>% filter(estimand == "amce_uncorrected") %>% pull(estimate)), 
                                          mean(out %>% filter(estimand == "amce_corrected") %>% pull(estimate))),
                           "se"       = c(sd(out %>% filter(estimand == "amce_uncorrected") %>% pull(estimate)), 
                                          sd(out %>% filter(estimand == "amce_corrected") %>% pull(estimate)))) %>% 
        dplyr::mutate("conf.low" = estimate - critical_t * se,
                      "conf.high" = estimate + critical_t * se,
                      "tau" = tau)
      
    }
  }  
  
  # return the output -------------------------------------------------------
  
  return(output)
  
} 
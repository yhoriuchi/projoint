#' Estimate and correct MMs or AMCEs
#'
#' This function ...
#'
#' @import dplyr
#' @import rlang
#' @importFrom MASS mvrnorm
#' @param .data A `projoint_data` object
#' @param .attribute A character column name identifying the attribute of interest
#' @param .level  A character vector identifying the levels of interest. Its length should be 1 for profile-level analysis and 2 for choice-level analysis
#' @param .structure A character identifying a data structure: "choice_level" (default) or "profile_level". Note: the right element of .level is the outcome of choice if .structure == "choice_level"
#' @param .estimand A character identifying the estimand: "mm" (default) or "amce"
#' @param .se_method  A character identifying the method of correcting measurement error bias: "analytical" (default), "simulation", or "bootstrap" (not recommended)
#' @param .irr NULL (default) if IRR is calculate using the repeated task. Otherwise, a numerical value
#' @param .remove_ties TRUE (default) if you want to remove ties for the attribute of interest (in profile-level analysis)
#' @param .repeated_task TRUE (default) if a repeated task is used to calculate IRR (recommended)
#' @param .ignore_position TRUE (default) if you ignore the location of profile (left or right. Relevant only if .structure == "choice_level"
#' @param .n_sims The number of simulations (default = 10000). Relevant only if .method == "simulation" 
#' @param .n_boot The number of bootstrapped samples (default = 1000). Relevant only if .method == "bootstrap"
#' @return A data frame
#' @export

pj_estimate <- function(
    .data,
    .attribute,
    .level,
    .structure = "profile_level",
    .estimand = "mm",
    .se_method = "analytical",
    .irr = NULL,
    .remove_ties = TRUE,
    .repeated_task = TRUE,
    .ignore_position = NULL,
    .n_sims = NULL,
    .n_boot = NULL
){
  
  .dataframe <- .data@data
  
  # bind variables locally to the function ----------------------------------
  
  id <- NULL
  selected <- NULL
  disagree <- NULL
  x <- NULL
  cov <- NULL
  vcov <- NULL
  sd <- NULL
  estimate <- NULL
  
  # check various settings --------------------------------------------------
  
  structure <- rlang::arg_match0(.structure, c("choice_level", "profile_level"))
  estimand  <- rlang::arg_match0(.estimand, c("mm", "amce"))
  se_method    <- rlang::arg_match0(.se_method, c("analytical", "simulation", "bootstrap"))
  
  if(!is.null(.irr) & !is.numeric(.irr) & length(.irr) == 1){
    stop("The .irr argument must be either a numeric scalar or NULL.")
  }
  
  if(!is.logical(.remove_ties)){
    stop("The .remove_ties argument must be either TRUE or FALSE.")
  }
  
  if(!is.logical(.repeated_task)){
    stop("The .repeated_task argument must be either TRUE or FALSE.")
  }
  
  if (.structure == "profile_level" & !is.null(.ignore_position)){
    stop("The .ignore_position argument can be specified only when the .structure argument is choice_level.")
  }
  
  if (.structure == "choice_level" & !is.logical(.ignore_position)){
    stop("The .ignore_position argument must be either TRUE or FALSE.")
  }
  
  if(!is.null(.n_sims) & !is.numeric(.n_sims) & length(.n_sims) == 1){
    stop("The .n_sims argument must be either a numeric scalar or NULL.")
  }
  
  if(!is.null(.n_boot) & !is.numeric(.n_boot) & length(.n_boot) == 1){
    stop("The .n_boot argument must be either a numeric scalar or NULL.")
  }
  
  # Organize data -----------------------------------------------------------
  
  if (.structure == "choice_level" & !is.null(.ignore_position) & !isTRUE(.ignore_position)){
    
    temp1 <- organize_data(.dataframe,
                           .attribute,
                           .level,
                           .structure,
                           .remove_ties,
                           .repeated_task)
    temp2 <- organize_data(.dataframe,
                           .attribute,
                           rev(.level), # the order is reversed
                           .structure,
                           .remove_ties,
                           .repeated_task)
    
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
                           .remove_ties,
                           .repeated_task)
    
    # save two data frames
    
    data_for_irr      <- .list$data_for_irr
    data_for_estimand <- .list$data_for_estimand
    
    
  }
  
  # Estimate or specify tau -------------------------------------------------
  
  if (is.null(.irr)){
    
    # run intercept-only regression models
    reg_tau <- stats::lm(disagree ~ 1, data_for_irr)
    
    tau     <- reg_tau$coefficient %>% as.numeric()
    var_tau <- vcov(reg_tau) %>% as.numeric()
    
  } else if (!is.null(.irr) & !is.numeric(.irr)){
    
    stop(".irr should be numeric.")
    
  } else {
    
    tau     <- 1 - .irr
    var_tau <- 0
    
  }
  
  # Estimate MM or AMCE (uncorrected) ---------------------------------------
  
  if (estimand == "mm"){
    
    reg_mm  <- stats::lm(selected ~ 1, data_for_estimand)
    
    mm_uncorrected <-  reg_mm$coefficient %>% as.numeric()
    var_uncorrected_mm <- vcov(reg_mm)  %>% as.numeric()
    
  } 
  
  if (estimand == "amce") {
    
    reg_amce  <- stats::lm(selected ~ x, data_for_estimand)
    
    amce_uncorrected <-  reg_amce$coefficient[2] %>% as.numeric()
    var_uncorrected_amce <- vcov(reg_amce)[2, 2]  %>% as.numeric()
    
  } 
  
  # Calculate the covariance ------------------------------------------------
  
  # Keep the observations with both "selected" and "disagree"
  
  data_for_cov <- data_for_estimand %>% 
    filter(!is.na(disagree))
  
  if (estimand == "mm"){
    
    if (is.null(.irr)){
      
      cov_mm_tau  <- cov(data_for_cov$selected, data_for_cov$disagree) / nrow(data_for_irr) 
      
    } else {
      
      cov_mm_tau <- 0
    }
    
  } 
  
  if (estimand == "amce") {
    
    d_cov0 <- data_for_cov %>% filter(x == 0)
    d_cov1 <- data_for_cov %>% filter(x == 1)
    
    if (is.null(.irr)){
      
      cov_mm0_tau  <- cov(d_cov0$selected, d_cov0$disagree) / nrow(data_for_irr)
      cov_mm1_tau  <- cov(d_cov1$selected, d_cov1$disagree) / nrow(data_for_irr)
      cov_amce_tau <- cov_mm1_tau - cov_mm0_tau
      
    } else {
      
      cov_amce_tau <- 0
      
    }
    
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
        dplyr::mutate("tau" = tau)
      
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
        dplyr::mutate("tau" = tau)
      
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
        reg_tau <- stats::lm(disagree ~ 1, bs_sample_1)
        reg_mm  <- stats::lm(selected ~ 1, bs_sample_2)
        
        # calculate the means
        mm_uncorrected <-  reg_mm$coefficient %>% as.numeric()
        if (is.null(.irr)){
          tau <- reg_tau$coefficient %>% as.numeric()
        } else {
          tau <- (1 - .irr)
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
        dplyr::mutate("tau" = tau)
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
        dplyr::mutate("tau" = tau)
      
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
        dplyr::mutate("tau" = tau)
      
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
        reg_tau  <- stats::lm(disagree ~ 1, bs_sample_1)
        reg_amce <- stats::lm(selected ~ x, bs_sample_2)
        
        # calculate the means
        amce_uncorrected <-  reg_amce$coefficient[2] %>% as.numeric()
        if (is.null(.irr)){
          tau <- reg_tau$coefficient %>% as.numeric()
        } else {
          tau <- (1 - .irr)
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
        dplyr::mutate("tau" = tau)
      
    }
  }  
  
  # return the output -------------------------------------------------------
  
  return(output)
  
} 
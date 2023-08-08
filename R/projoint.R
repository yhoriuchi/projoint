#' Analyze a conjoint data set and correct for measurement error
#'
#' This main function analyzes a conjoint data set and produces measurement error-corrected estimates of either marginal means or average marginal component effects, ready for plotting.
#' It  accepts a `projoint_data` object, and optionally a `projoint_qoi` object for users who wish to specify more complex quantities of interest.
#'
#' @import dplyr
#' @import rlang
#' @import estimatr
#' @importFrom MASS mvrnorm
#' @importFrom methods is
#' @importFrom methods new
#' @param .data A `projoint_data` object
#' @param .qoi A `projoint_qoi` object. If NULL, defaults to producing all MMs and all AMCEs.
#' @param .structure Either "profile_level" (default) or "choice_level" 
#' @param .estimand Either "mm" for marginal mean or "amce" for average marginal component effect
#' @param .remove_ties Logical: should ties be removed before estimation? Defaults to TRUE.
#' @param .irr NULL (default) if IRR is to be calculated using the repeated task. Otherwise, a numerical value
#' @param .ignore_position TRUE (default) if you ignore the location of profile (left or right. Relevant only if analyzed at the choice level
#' @param .se_method c("analytic", "simulation", "bootstrap") description
#' @param .n_sims The number of simulations. Relevant only if .se_method == "simulation" 
#' @param .n_boot The number of bootstrapped samples. Relevant only if .se_method == "bootstrap"
#' @param .weights_1 the weight to estimate IRR (see `lm_robust()`): NULL (default)
#' @param .clusters_1 the clusters to estimate IRR (see `lm_robust()`): NULL (default)
#' @param .se_type_1 the standard error type to estimate IRR (see `lm_robust()`): "classical" (default)
#' @param .weights_2 the weight to estimate MM or AMCE (see `lm_robust()`): NULL (default)
#' @param .clusters_2 the clusters to estimate MM or AMCE (see `lm_robust()`): NULL (default)
#' @param .se_type_2 the standard error type to estimate MM or AMCE (see `lm_robust()`): "classical" (default)
#' @return A `projoint_results` object
#' @export
#' @examples
#' 
#' library(projoint)
#' 
#' data("exampleData1")
#' head(exampleData1)
#'
#' outcomes <- paste0("choice", seq(from = 1, to = 8, by = 1))
#' outcomes <- c(outcomes, "choice1_repeated_flipped")
#' 
#' reshaped_data <- reshape_projoint(
#'   .dataframe = exampleData1, 
#'   .idvar = "ResponseId", 
#'   .outcomes = outcomes,
#'   .outcomes_ids = c("A", "B"),
#'   .alphabet = "K", 
#'   .repeated = TRUE,
#'   .flipped = TRUE)
#'
#' projoint(reshaped_data) 

projoint <- function(
    .data,
    .qoi = NULL,
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
  
  .baseline <- NULL
  
  # check various settings --------------------------------------------------
  # Also see the checking in pj_estimate()
  
  structure <- rlang::arg_match0(.structure, c("choice_level", "profile_level"))
  estimand  <- rlang::arg_match0(.estimand, c("mm", "amce"))
  se_method <- rlang::arg_match0(.se_method, c("analytical", "simulation", "bootstrap"))

  if(!is(.data, "projoint_data")){
    stop("The .data argument must be of class `projoint_data` from the `reshape_projoint` function.")
  }
  
  if(.estimand == "mm" & !is.null(.qoi) & !is(.qoi, "projoint_qoi_mm")){
    stop("The .qoi argument must be of class `projoint_qoi_mm` from the `set_qoi` function.")
  }
  
  if(.estimand == "amce" & !is.null(.qoi) & !is(.qoi, "projoint_qoi_amce")){
    stop("The .qoi argument must be of class `projoint_qoi_amce` from the `set_qoi` function.")
  }
  
  if(.se_method == "simulation" & is.null(.n_sims)){
    stop("If SEs are calculated by simulation, .n_sims must be specified (not NULL).")
  }
  
  if(.se_method == "bootstrap" & is.null(.n_boot)){
    stop("If SEs are calculated by bootstrap, .n_boot must be specified (not NULL).")
  }
  
  if(.estimand == "amce" & is.null(.qoi) & .structure == "choice_level"){
    stop("The .structure argument must be profile_level if the .qoi argument is NULL.")
  }

  # estimate all MMs or AMCEs -----------------------------------------------
  
  if (is.null(.qoi)){
    
    attribute_levels <- .data@labels$level_id
    
    out <- NULL
    
    for (i in seq_along(attribute_levels)){
      
      attribute <- str_extract(attribute_levels[i], "^.+(?=:)")
      level     <- str_extract(attribute_levels[i], "(?<=:).+$")
      
      if (.estimand == "mm"){
        
        temp <- pj_estimate(.data,
                            .attribute = attribute, # note: this is NOT .attribute
                            .level = level, # note: this is NOT .level
                            .structure,
                            .estimand = "mm",
                            .se_method,
                            .irr,
                            .baseline = NULL,
                            .remove_ties,
                            .ignore_position,
                            .n_sims,
                            .n_boot) %>% 
          mutate(attribute = attribute, 
                 level = paste0(level, collapse = ", "))
        
      }
      
      if (.estimand == "amce"){
        
        baseline <- "level1" # The default baseline is "level1"
        
        temp <- pj_estimate(.data,
                            .attribute = attribute, # note: this is NOT .attribute
                            .level = level, # note: this is NOT .level
                            .structure,
                            .estimand = "amce",
                            .se_method,
                            .irr,
                            .baseline = baseline, # note: this is NOT .baseline
                            .remove_ties,
                            .ignore_position,
                            .n_sims,
                            .n_boot) %>% 
          mutate(attribute = attribute, 
                 level = paste0(level, collapse = ", "),
                 baseline = baseline)
        
        
      }
      
      
      out <- bind_rows(out, temp)
      
    }
    
    if (.estimand == "amce"){
      
      out <- out %>% 
        filter(level != baseline)
      
    }
    
    
    
    
  } else{
    
    attribute <- .qoi@attribute_of_interest
    level <- .qoi@levels_of_interest
    
    
    if (.estimand == "mm"){
      
      out <- pj_estimate(.data,
                         .attribute = attribute, # note: this is NOT .attribute
                         .level = level, # note: this is NOT .level
                         .structure,
                         .estimand = "mm",
                         .se_method,
                         .irr,
                         .baseline = NULL,
                         .remove_ties,
                         .ignore_position,
                         .n_sims,
                         .n_boot) %>% 
        mutate(attribute = attribute, 
               level = paste0(level, collapse = ", "))
      
    }
    
    if (.estimand == "amce"){
      
      baseline <- .qoi@baseline
      
      out <- pj_estimate(.data,
                         .attribute = attribute, # note: this is NOT .attribute
                         .level = level, # note: this is NOT .level
                         .structure,
                         .estimand = "amce",
                         .se_method,
                         .irr,
                         .baseline = baseline, # note: this is NOT .baseline
                         .remove_ties,
                         .ignore_position,
                         .n_sims,
                         .n_boot) %>% 
        mutate(attribute = attribute, 
               level = paste0(level, collapse = ", "),
               baseline = paste0(baseline, collapse = ", "))
      
      
    }
    
  }
  
  # return(out)
  tau <- unique(out$tau)
  estimates <- out %>% 
    dplyr::select(-tau) %>% 
    as_tibble()
  
  
  if (.estimand == "mm"){
    
    if(is.null(.qoi)){
      projoint_results_mm("estimates" = estimates, "tau" = tau, # the slots specific to projoint_results
                          labels = .data@labels, data = .data@data, # the slots inherited from projoint_data
                          #irr = irr, figure = NULL # slots inherited from projoint_irr
                          attribute_of_interest = "all",
                          levels_of_interest = "all"
      ) %>% 
        return()
    } else {
      projoint_results_mm("estimates" = estimates, "tau" = tau, # the slots specific to projoint_results
                          labels = .data@labels, data = .data@data, # the slots inherited from projoint_data
                          #irr = irr, figure = NULL, # slots inherited from projoint_irr
                          attribute_of_interest = .qoi@attribute_of_interest,
                          levels_of_interest = .qoi@levels_of_interest
                          ) %>% 
        return()
    }
    
    
  } else if (.estimand == "amce"){
    
    if(is.null(.qoi)){
      projoint_results_amce("estimates" = estimates, "tau" = tau, # the slot specific to projoint_results
                            labels = .data@labels, data = .data@data, # the slots inherited from projoint_data
                            #irr = irr, figure = NULL # slots inherited from projoint_irr
                            attribute_of_interest = "all",
                            levels_of_interest = "all"
      ) %>% 
        return()
    } else {
      projoint_results_amce("estimates" = estimates, "tau" = tau, # the slot specific to projoint_results
                            labels = .data@labels, data = .data@data, # the slots inherited from projoint_data
                            # irr = irr, figure = NULL, # slots inherited from projoint_irr
                            attribute_of_interest = .qoi@attribute_of_interest,
                            levels_of_interest = .qoi@levels_of_interest
                            ) %>% 
        return()
    }
    
    
  } else{
    stop("Estimand must be either 'mm' or 'amce'")
  }
  
}


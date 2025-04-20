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
#' @param .data A \code{\link{projoint_data}} object
#' @param .qoi A \code{\link{projoint_qoi}} object. If \code{NULL}, defaults to producing all MMs and all AMCEs.
#' @param .structure Either \code{"profile_level"} (default) or \code{"choice_level"} 
#' @param .estimand Either \code{"mm"} for marginal mean or \code{"amce"} for average marginal component effect
#' @param .se_method By default, \code{c("analytic", "simulation", "bootstrap")} description
#' @param .irr \code{NULL} (default) if IRR is to be calculated using the repeated task. Otherwise, a numerical value
#' @param .remove_ties Logical: should ties be removed before estimation? Defaults to \code{TRUE}.
#' @param .ignore_position TRUE (default) if you ignore the location of profile (left or right). Relevant only if analyzed at the choice level
#' @param .n_sims The number of simulations. Relevant only if \code{.se_method == "simulation"} 
#' @param .n_boot The number of bootstrapped samples. Relevant only if \code{.se_method == "bootstrap"}
#' @param .weights_1 the weight to estimate IRR (see \code{\link[estimatr]{lm_robust}}): \code{NULL} (default)
#' @param .clusters_1 the clusters to estimate IRR (see \code{\link[estimatr]{lm_robust}}): \code{NULL} (default)
#' @param .se_type_1 the standard error type to estimate IRR (see \code{\link[estimatr]{lm_robust}}): \code{"classical"} (default)
#' @param .weights_2 the weight to estimate MM or AMCE (see \code{\link[estimatr]{lm_robust}}): \code{NULL} (default)
#' @param .clusters_2 the clusters to estimate MM or AMCE (see \code{\link[estimatr]{lm_robust}}): \code{NULL} (default)
#' @param .se_type_2 the standard error type to estimate MM or AMCE (see \code{\link[estimatr]{lm_robust}}): \code{"classical"} (default)
#' @return A \code{\link{projoint_results}} object

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
    .se_type_2
){
  
  # bind variables locally to the function ----------------------------------
  
  baseline <- NULL
  att_level_choose <- NULL
  att_level_notchoose <- NULL
  att_level_choose_baseline <- NULL
  att_level_notchoose_baseline <- NULL
  irr <- NULL
  
  # check various settings --------------------------------------------------
  # Also see the checking in pj_estimate()
  
  structure <- rlang::arg_match0(.structure, c("choice_level", "profile_level"))
  estimand  <- rlang::arg_match0(.estimand, c("mm", "amce"))
  se_method <- rlang::arg_match0(.se_method, c("analytical", "simulation", "bootstrap"))
  
  if(!is(.data, "projoint_data")){
    stop("The .data argument must be of class `projoint_data` from the `reshape_projoint` function.")
  }
  
  if(!is.null(.qoi) & !is(.qoi, "projoint_qoi")){
    stop("The .qoi argument must be of class `projoint_qoi` from the `set_qoi` function.")
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
  
  if (is.null(.qoi) & structure == "choice-level"){
    stop("The .qoi argument must be specified for choice-level analysis.") 
  }
  
  # estimate all MMs or AMCEs -----------------------------------------------
  
  if (is.null(.qoi)){
    
    attribute_levels <- .data$labels$level_id
    
    out <- NULL
    
    for (i in seq_along(attribute_levels)){
      
      attribute <- stringr::str_extract(attribute_levels[i], "^.+(?=:)")
      level     <- stringr::str_extract(attribute_levels[i], "(?<=:).+$")
      
      if (estimand == "mm"){
        
        temp1 <- pj_estimate(.data,
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
                             .se_type_2) |> 
          dplyr::mutate(att_level_choose = stringr::str_c(stringr::str_c(attribute, level, sep = ":"), collapse = " or "))
        
      } else {
        
        temp1 <- pj_estimate(.data,
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
                             .se_type_2) |> 
          dplyr::mutate(att_level_choose = stringr::str_c(stringr::str_c(attribute, level, sep = ":"), collapse = " or "),
                        att_level_choose_baseline = stringr::str_c(stringr::str_c(attribute, "level1", sep = ":"), collapse = " or "),
          )
        
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
    
    temp <- pj_estimate(.data,
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
  
  
  # return(out)
  tau <- unique(out$tau)
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
                              labels = .data$labels,
                              data = .data$data)
      )
    }
    
  }
  
}

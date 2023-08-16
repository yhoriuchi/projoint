#' Plot all "profile-level" MMs or AMCEs
#'
#' This method produces profile-level MM or AMCE plots given a \code{\link{projoint_results}} object, the output from the \code{\link{projoint}} function. The structure must be profile-level to use this function.
#'
#' @import ggplot2
#' @import ggthemes
#' @import dplyr
#' @import stringr
#' @keywords internal
#' @param x A \code{\link{projoint_results}} object
#' @param .estimates The estimates to be plotted, either \code{"corrected"} (default) or \code{"uncorrected"}
#' @param .labels A character vector for the x-axis labels
#' @param .base_size base font size, given in pts.
#' @param .base_family base font family
#' @param ... Additional optional arguments
#' @return A \code{ggplot} object
#' @export

plot_projoint_choice_level_mm <- function(
    x,
    .estimates = "corrected",
    .labels = NULL,
    .base_size = 12,
    .base_family = "",
    ...
){
  
  # bind variables locally to the function ----------------------------------
  
  level <- NULL
  estimate <- NULL
  estimand <- NULL
  se <- NULL
  conf.low <- NULL
  conf.high <- NULL
  att_level <- NULL
  att_level_choose <- NULL
  att_level_notchoose <- NULL
  
  # check -------------------------------------------------------------------
  
  if (!(.estimates %in% c("corrected", "uncorrected"))){
    stop("The .estimates argument must be either 'corrected' or 'uncorrected'.")
  }
  
  if(!is(x, "projoint_results")){
    stop("The x argument must be of class `projoint_results` from the `projoint` function.")
  }
  
  if (.estimates == "corrected"){
    corrected <- "_corrected"
  } else{
    corrected <- "_uncorrected"
  }
  
  output_reshaped <- bind_rows(
    x@estimates %>% 
      select(estimand, estimate, se, conf.low, conf.high, 
             "att_level" = att_level_choose), 
    x@estimates %>% 
      select(estimand, estimate, se, conf.low, conf.high, 
             "att_level" = att_level_notchoose) %>% 
      mutate(estimate  = 1 - estimate,
             conf.low  = 1 - conf.low, 
             conf.high = 1 - conf.high)
  ) %>% 
    filter(str_detect(estimand, corrected))
  
  if (!is.null(.labels)){
    
    output_reshaped <- output_reshaped %>% 
      mutate(att_level = factor(att_level, labels = .labels))
    
  }
  
  g <- ggplot2::ggplot(output_reshaped) +
    ggthemes::theme_few(base_size = .base_size, 
                        base_family = .base_family) %+replace%
    ggplot2::theme(axis.text.x =  ggplot2::element_text(size = .base_size, 
                                                        color = "black", 
                                                        hjust = .5 , 
                                                        vjust = 1),
                   axis.text.y =  ggplot2::element_text(size = .base_size, 
                                                        color = "black", 
                                                        hjust = 0  , 
                                                        vjust = 0.5),
                   axis.ticks =   ggplot2::element_line(colour = "grey50"),
                   axis.title.y = ggplot2::element_text(size = .base_size, 
                                                        angle = 90, 
                                                        vjust = .01, 
                                                        hjust = .1)) +
    ggplot2::geom_col(aes(x = att_level, 
                          y = estimate),
                      fill = "gray", 
                      width = 0.5) +
    ggplot2::geom_errorbar(aes(x = att_level, 
                               ymin = conf.low,
                               ymax = conf.high), 
                           width = 0.2) +
    ggthemes::theme_few() 
  
  return(g)
  
}
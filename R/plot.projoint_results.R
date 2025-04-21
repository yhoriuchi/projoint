#' Plot Marginal Means (MMs) or AMCEs from projoint Results
#'
#' This function creates publication-ready plots based on the output from `projoint()`.
#' It supports both profile-level and choice-level analyses, with tailored plotting options.
#'
#' @param x A `projoint_results` object, typically created by `projoint()`.
#' @param .estimates Character. Which estimates to plot: `"corrected"` (default), `"uncorrected"`, or `"both"`.
#' @param .by_var Logical. (Profile-level only) Whether to plot subgroup differences. Default is `FALSE`.
#' @param .labels Character vector. (Choice-level only) Custom x-axis labels for bar or point-range plots.
#' @param .base_size Numeric. Base font size for plot text. Default is 12.
#' @param .base_family Character. Base font family for plot text. Default is "" (system default).
#' @param .type Character. (Choice-level only) Type of plot: `"bar"` (default) or `"pointrange"`.
#' @param .show_attribute Logical. (Choice-level only) Whether to display the attribute name as a plot title. Default is `FALSE`.
#' @param .xtitle Character. (Choice-level only) X-axis label text. Default is `"Choice-level marginal mean"`.
#' @param .remove_xaxis Logical. (Choice-level only) Whether to remove x-axis labels and ticks. Default is `FALSE`.
#' @param .xlim Numeric vector of length 2. (Choice-level only) X-axis limits. Default is `c(0, 1)`.
#' @param .hjust_left Numeric. (Choice-level only) Horizontal adjustment for the left-side label. Default is 1.
#' @param .hjust_right Numeric. (Choice-level only) Horizontal adjustment for the right-side label. Default is 0.
#' @param .title_size Numeric. (Choice-level only) Font size for the plot title if `.show_attribute = TRUE`. Default is 11.
#' @param .plot.margin Numeric vector of length 4. (Choice-level only) Margins around the plot in centimeters. Default is `c(0, 3, 0, 3)`.
#' @param ... Additional arguments passed to underlying plotting functions.
#'
#' @details
#' For **profile-level results**, only `.by_var`, `.base_size`, and `.base_family` are relevant.
#'
#' For **choice-level results**, only `.type`, `.labels`, `.show_attribute`, `.xtitle`, `.remove_xaxis`,
#' `.xlim`, `.hjust_left`, `.hjust_right`, `.title_size`, and `.plot.margin` are relevant.
#'
#' If irrelevant arguments are provided for a given structure, a warning will be issued and the arguments will be ignored.
#'
#' @return A `ggplot2` object.
#' @seealso [projoint()], [projoint_results], [plot_projoint_profile_level()], [plot_projoint_choice_level_mm()]
#' @export
plot.projoint_results <- function(
    x, 
    .estimates = "corrected",
    
    .by_var = FALSE,
    .labels = NULL,
    .base_size = 12,
    .base_family = "",
    
    .type = c("bar", "pointrange"),
    .show_attribute = FALSE, 
    .xtitle = "Choice-level marginal mean", 
    .remove_xaxis = FALSE, 
    .xlim = c(0, 1),
    .hjust_left = 1,
    .hjust_right = 0,
    .title_size = 11, 
    .plot.margin = c(0, 3, 0, 3),
    ...
) {
  
  if (!inherits(x, "projoint_results")) {
    stop("The `x` argument must be a `projoint_results` object, output from `projoint()`.")
  }
  
  .estimand <- x$estimand
  .structure <- x$structure
  
  .structure <- match.arg(.structure, choices = c("profile_level", "choice_level"))
  .estimand <- match.arg(.estimand, choices = c("mm", "amce"))
  
  
  if (.structure == "profile_level") {
  
    .estimates <- match.arg(.estimates, choices = c("corrected", "uncorrected", "both"))
    
    irrelevant_args <- c()
    
    if (!missing(.show_attribute) && .show_attribute != FALSE) irrelevant_args <- c(irrelevant_args, ".show_attribute")
    if (!missing(.xtitle) && .xtitle != "Choice-level marginal mean") irrelevant_args <- c(irrelevant_args, ".xtitle")
    if (!missing(.remove_xaxis) && .remove_xaxis != FALSE) irrelevant_args <- c(irrelevant_args, ".remove_xaxis")
    if (!missing(.xlim) && !identical(.xlim, c(0, 1))) irrelevant_args <- c(irrelevant_args, ".xlim")
    if (!missing(.hjust_left) && .hjust_left != 1) irrelevant_args <- c(irrelevant_args, ".hjust_left")
    if (!missing(.hjust_right) && .hjust_right != 0) irrelevant_args <- c(irrelevant_args, ".hjust_right")
    if (!missing(.title_size) && .title_size != 11) irrelevant_args <- c(irrelevant_args, ".title_size")
    if (!missing(.plot.margin) && !identical(.plot.margin, c(0, 3, 0, 3))) irrelevant_args <- c(irrelevant_args, ".plot.margin")
    
    if (length(irrelevant_args) > 0) {
      warning("The following arguments are ignored for profile-level plots: ", paste(irrelevant_args, collapse = ", "))
    }
    
    out <- plot_projoint_profile_level(
      x = x,
      .estimates = .estimates,
      .by_var = .by_var,
      .base_size = .base_size,
      .base_family = .base_family,
      ...
    )
    
  } else {
    
    if (.estimand != "mm") {
      stop("Currently, plotting is only supported for `choice_level` structure and `mm` estimand. Stay tuned!")
    }
    
    .estimates <- match.arg(.estimates, choices = c("corrected", "uncorrected"))
    
    .type <- match.arg(.type)
    
    irrelevant_args <- c()
    
    if (!missing(.by_var) && .by_var != FALSE) irrelevant_args <- c(irrelevant_args, ".by_var")
    
    if (length(irrelevant_args) > 0) {
      warning("The following arguments are ignored for choice-level plots: ", paste(irrelevant_args, collapse = ", "))
    }

    out <- plot_projoint_choice_level_mm(
      x = x,
      .type = .type,
      .estimates = .estimates,
      .labels = .labels,
      .base_size = .base_size,
      .base_family = .base_family,
      .show_attribute = .show_attribute, 
      .xtitle = .xtitle, 
      .remove_xaxis = .remove_xaxis, 
      .xlim = .xlim,
      .hjust_left = .hjust_left,
      .hjust_right = .hjust_right,
      .title_size = .title_size, 
      .plot.margin = .plot.margin,
      ...
    )
    
  } 
  
  return(out)
}

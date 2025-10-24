#' Plot method for \code{projoint_results}
#'
#' Creates publication-ready plots from a \code{projoint_results} object produced by
#' \code{\link{projoint}}. Supports both profile-level and choice-level analyses,
#' with plotting options tailored to each structure.
#'
#' @param x A \code{projoint_results} object (typically from \code{\link{projoint}}).
#' @param .estimates Character: which estimates to plot. One of
#'   \code{"corrected"}, \code{"uncorrected"}, or \code{"both"} (for profile-level),
#'   and \code{"corrected"} or \code{"uncorrected"} (for choice-level). Default \code{"corrected"}.
#' @param .by_var Logical (profile-level only). Whether to plot subgroup differences.
#'   Default \code{FALSE}.
#' @param .labels Character vector of length 2 (choice-level only). Custom x-axis
#'   labels for bar/pointrange plots. If \code{NULL}, labels are taken from \code{x$labels}.
#' @param .base_size Numeric. Base font size. Default \code{12}.
#' @param .base_family Character. Base font family. Default \code{""} (system default).
#' @param .type Character (choice-level only). One of \code{"bar"} or \code{"pointrange"}.
#'   Default \code{"bar"}.
#' @param .show_attribute Logical (choice-level only). Show the attribute name as the
#'   title when both levels belong to the same attribute. Default \code{TRUE}.
#' @param .remove_xaxis Logical (choice-level only). Remove x-axis line, ticks, and labels.
#'   Default \code{FALSE}.
#' @param .xlim Numeric length-2 vector (choice-level only). X-axis limits. Default \code{c(0, 1)}.
#' @param .plot.margin Numeric length-4 vector (choice-level only). Plot margins in cm:
#'   \code{c(top, left, bottom, right)}. Default \code{c(0, 3, 0, 3)}.
#' @param ... Additional arguments passed to downstream plotting helpers.
#'
#' @details
#' For profile-level results, only \code{.by_var}, \code{.base_size}, and \code{.base_family}
#' are relevant. For choice-level results, only \code{.type}, \code{.labels},
#' \code{.show_attribute}, \code{.remove_xaxis}, \code{.xlim}, and \code{.plot.margin}
#' are relevant. Irrelevant arguments are ignored with a warning.
#'
#' @return A \code{ggplot2} object.
#'
#' @seealso \code{\link{projoint}}, \code{plot_projoint_profile_level},
#'   \code{\link{plot_projoint_choice_level_mm}}
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(exampleData1)
#'
#' # Two base tasks (1 & 2) + repeated of task 1 (last)
#' dat <- reshape_projoint(
#'   exampleData1,
#'   .outcomes = c("choice1", "choice2", "choice1_repeated_flipped")
#' )
#'
#' # Build a valid QOI from the labels
#' att <- unique(dat$labels$attribute_id)[1]
#' levs <- subset(dat$labels, attribute_id == att)$level_id
#' lev_names <- sub(".*:", "", levs)
#'
#' q <- set_qoi(
#'   .structure     = "choice_level",
#'   .estimand      = "mm",
#'   .att_choose    = att,
#'   .lev_choose    = lev_names[2],
#'   .att_notchoose = att,
#'   .lev_notchoose = lev_names[1]
#' )
#'
#' fit <- projoint(dat, .qoi = q)
#'
#' # Plot method
#' plot(fit)
#' }
plot.projoint_results <- function(
    x, 
    .estimates = "corrected",
    
    .by_var = FALSE,
    .labels = NULL,
    .base_size = 12,
    .base_family = "",
    
    .type = c("bar", "pointrange"),
    .show_attribute = TRUE, 
    .remove_xaxis = FALSE, 
    .xlim = c(0, 1),
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
    if (!missing(.remove_xaxis) && .remove_xaxis != FALSE) irrelevant_args <- c(irrelevant_args, ".remove_xaxis")
    if (!missing(.xlim) && !identical(.xlim, c(0, 1))) irrelevant_args <- c(irrelevant_args, ".xlim")
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
      .show_attribute = .show_attribute, 
      .remove_xaxis = .remove_xaxis, 
      .xlim = .xlim,
      .plot.margin = .plot.margin,
      ...
    )
    
  } 
  
  return(out)
}

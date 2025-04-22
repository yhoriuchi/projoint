#' Plot Choice-Level Marginal Means (MMs)
#'
#' This function produces a plot of choice-level marginal means (MMs) given a \code{\link{projoint_results}} object, 
#' which is the output from the \code{\link{projoint}} function. 
#' It supports two types of visualizations: a bar plot and a pointrange plot.
#'
#' @param x A \code{\link{projoint_results}} object. This is the output from the \code{projoint()} function.
#' @param .type Type of plot to produce. Either \code{"bar"} (default) for bar plots or \code{"pointrange"} for a horizontal point-range plot.
#' @param .estimates Which estimates to plot. Options are \code{"corrected"} (default) or \code{"uncorrected"}.
#' @param .labels A character vector specifying custom labels for the x-axis (optional). If \code{NULL} (default), labels are automatically generated from the data.
#' @param .show_attribute Logical; if \code{TRUE} (default), display the attribute name as the plot title.
#' @param .remove_xaxis Logical; if \code{TRUE}, remove the x-axis elements (ticks, labels, and line). Default is \code{FALSE}.
#' @param .xlim A numeric vector specifying the x-axis limits (default is \code{c(0, 1)}).
#' @param .plot.margin A numeric vector specifying plot margins in centimeters, ordered as \code{c(top, left, bottom, right)}. Default is \code{c(1, 2, 1, 2)}. 
#' If \code{NULL}, margins are automatically set based on label lengths.
#' @param ... Additional arguments (currently unused).
#'
#' @details
#' When \code{.type = "pointrange"}, the two attribute levels are automatically positioned slightly outside the [0,1] x-axis range to avoid overlap with confidence intervals. 
#' If \code{.labels} is not supplied, the function attempts to extract level labels from the input object.
#'
#' If the two levels being compared belong to different attributes, a warning message is shown and no title is added.
#'
#' @return A \code{ggplot2} object.
#'
#' @import ggplot2
#' @import ggthemes
#' @import dplyr
#' @import stringr
#' @importFrom grid unit
#' @importFrom scales pretty_breaks
#' @keywords internal
#' @export
plot_projoint_choice_level_mm <- function(
    x,
    .type = "pointrange",
    .estimates = "corrected",
    .labels = NULL,
    .show_attribute = TRUE,
    .remove_xaxis = FALSE,
    .xlim = c(0, 1),
    .plot.margin = c(top = 1, left = 2, bottom = 1, right = 2),
    ...
) {
  ...
}

plot_projoint_choice_level_mm <- function(
    x,
    .type = "pointrange",
    .estimates = "corrected",
    .labels = NULL,
    .show_attribute = TRUE, 
    .remove_xaxis = FALSE, 
    .xlim = c(0, 1),
    .plot.margin = c(top = 1, 
                     left = 2, 
                     bottom = 1, 
                     right = 2),
    ...
) {
  
  # Validation
  
  if (!inherits(x, "projoint_results")) {
    stop("Input `x` must be a `projoint_results` object.")
  }
  
  .type <- match.arg(.type, choices = c("bar", "pointrange"))
  .estimates <- match.arg(.estimates, choices = c("corrected", "uncorrected"))
  
  label_att1 <- str_extract(x$estimates$att_level_choose, "att\\d+")[1]
  label_att0 <- str_extract(x$estimates$att_level_notchoose, "att\\d+")[1]
  
  if (label_att1 != label_att0){
    
    message("The attributes are different between the two levels. Check your setting carefully. The attribute label is not added to the figure.")
    label_att <- NULL
    
  } else{
    label_att <- tibble(attribute_id = label_att1) |> 
      left_join(x$labels, by = "attribute_id") |> 
      select(attribute) |> 
      distinct() |> 
      pull()
  }
  
  estimand <- ifelse(x$estimand == "mm", 
                     "Marginal Mean",
                     "Average Marginal Component Effect") |> 
    as.character()
  
  # Additional labels
  
  if (is.null(.labels)) {
    
    # Try to get nice labels from your data
    labels_df <- x$labels
    label0 <- labels_df$level[match(x$estimates$att_level_notchoose[1], labels_df$level_id)]
    label1 <- labels_df$level[match(x$estimates$att_level_choose[1], labels_df$level_id)]
    .labels <- c(label0, label1)
    
    label0_length <- nchar(label0)
    label1_length <- nchar(label1)
    
    # margin0 <- min(max(nchar(label0) / 3, 1), 10)
    # margin1 <- min(max(nchar(label1) / 3, 1), 10)
    # .plot.margin <- c(top = 1, 
    #                   right = margin0, 
    #                   bottom = 1, 
    #                   left = margin1)
    
  }
  
  label0 <- .labels[1]
  label1 <- .labels[2]
  
  if (.type == "bar") {
    
    # Variables
    corrected <- ifelse(.estimates == "corrected", "_corrected", "_uncorrected")
    
    output_reshaped <- dplyr::bind_rows(
      x$estimates |> 
        dplyr::select(estimand, estimate, se, conf.low, conf.high, 
                      "att_level" = att_level_choose), 
      x$estimates |> 
        dplyr::select(estimand, estimate, se, conf.low, conf.high, 
                      "att_level" = att_level_notchoose) |> 
        dplyr::mutate(estimate = 1 - estimate,
                      conf.low = 1 - conf.low,
                      conf.high = 1 - conf.high)
    ) |> 
      dplyr::filter(stringr::str_detect(estimand, corrected)) |> 
      left_join(x$labels, by = c("att_level" = "level_id"))
    
    
    if (!is.null(.labels)) {
      output_reshaped <- output_reshaped |>
        dplyr::mutate(att_level = factor(level, 
                                         levels = .labels))
    }
    
    g <- ggplot2::ggplot(output_reshaped) +
      ggthemes::theme_few() +
      ggplot2::theme(plot.margin = grid::unit(.plot.margin, "cm")) +
      ggplot2::geom_col(ggplot2::aes(x = att_level, y = estimate),
                        fill = "gray", width = 0.5) +
      ggplot2::geom_errorbar(ggplot2::aes(x = att_level, ymin = conf.low, ymax = conf.high),
                             width = 0.2) +
      labs(title = label_att, 
           y = estimand, 
           x = NULL)
    
    return(g)
    
  } else {  # pointrange style
    
    out <- x$estimates |>
      dplyr::filter(estimand == "mm_corrected") |>
      dplyr::select(-estimand) |>
      dplyr::left_join(x$labels |> 
                         dplyr::select(level_id, level, attribute), 
                       by = c("att_level_choose" = "level_id")) |>
      dplyr::rename(level1 = level, attribute1 = attribute) |>
      dplyr::left_join(x$labels |> 
                         dplyr::select(level_id, level, attribute), 
                       by = c("att_level_notchoose" = "level_id")) |>
      dplyr::rename(level0 = level, attribute0 = attribute)
    
    
    if (nrow(out) != 1) {
      stop("This plotting function expects only one attribute-level pair at a time.")
    }
    
    g <- ggplot2::ggplot(out, ggplot2::aes(x = estimate, y = 1)) +
      ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray") +
      ggplot2::geom_pointrange(ggplot2::aes(xmin = conf.low, xmax = conf.high)) +
      ggplot2::geom_text(ggplot2::aes(label = format(round(estimate, 2), nsmall = 2)),
                         vjust = -1) +
      annotate("text", x = -0.08, y = 1, label = label0, hjust = 1) +
      annotate("text", x = 1.08, y = 1, label = label1, hjust = 0) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 4)(.xlim)) +
      ggplot2::labs(y = NULL, x = NULL) +
      ggplot2::coord_cartesian(xlim = .xlim, clip = "off") +
      ggthemes::theme_few() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = 11, color = "black", hjust = .5, vjust = 1),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        plot.margin = grid::unit(.plot.margin, "cm")
      )
    
    
    if (.show_attribute) {
      g <- g +
        ggplot2::labs(title = label_att) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 11, face = "bold", vjust = -0.5))
    }
    
    if (.remove_xaxis) {
      g <- g + ggplot2::theme(
        axis.ticks.x = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_blank()
      ) +
        ggplot2::labs(x = NULL)
    } else {
      g <- g + ggplot2::labs(x = estimand)
    }
    
    return(g)
    
  }
}

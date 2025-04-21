#' Plot all "choice-level" MMs
#'
#' This function produces choice-level MM plots given a \code{\link{projoint_results}} object, the output from the \code{\link{projoint}} function. 
#'
#' @import ggplot2
#' @import ggthemes
#' @import dplyr
#' @import stringr
#' @keywords internal
#' @param x A \code{\link{projoint_results}} object.
#' @param .type Type of plot: \code{"bar"} (default) or \code{"pointrange"}.
#' @param .estimates Which estimates to plot: \code{"corrected"} (default) or \code{"uncorrected"}.
#' @param .labels A character vector for x-axis labels (optional).
#' @param .show_attribute Logical; show attribute as plot title (for pointrange type).
#' @param .remove_xaxis Logical; remove x-axis (for pointrange type).
#' @param .xlim Limits of x-axis (for pointrange type).
#' @param .plot.margin Plot margins (for pointrange type).
#' @param ... Additional arguments (currently unused).
#'
#' @return A ggplot object.
#' @export
plot_projoint_choice_level_mm <- function(
    x,
    .type = "pointrange",
    .estimates = "corrected",
    .labels = NULL,
    .show_attribute = TRUE, 
    .remove_xaxis = FALSE, 
    .xlim = c(0, 1),
    .plot.margin = NULL,
    ...
) {
  
  # Validation
  
  if (!inherits(x, "projoint_results")) {
    stop("Input `x` must be a `projoint_results` object.")
  }
  
  .type <- match.arg(.type, choices = c("bar", "pointrange"))
  .estimates <- match.arg(.estimates, choices = c("corrected", "uncorrected"))
  
  label_att <- x$labels$attribute[1]
  
  estimand <- ifelse(x$estimand == "mm", 
                     "Marginal Mean",
                     "Average Marginal Component Effect") %>% 
    as.character()
  
  # Additional labels
  
  if (is.null(.labels)) {
    
    # Try to get nice labels from your data
    labels_df <- x$labels
    label_left <- labels_df$level[match(x$estimates$att_level_notchoose[1], labels_df$level_id)]
    label_right <- labels_df$level[match(x$estimates$att_level_choose[1], labels_df$level_id)]
    .labels <- c(label_left, label_right)
    
    left_label_length <- nchar(label_left)
    right_label_length <- nchar(label_right)
    
    left_margin <- min(max(nchar(label_left) / 5, 1), 10)
    right_margin <- min(max(nchar(label_right) / 5, 1), 10)
    .plot.margin <- c(1, right_margin, 1, left_margin)
    
    
  }

  if (.type == "bar") {
    
    # Variables
    corrected <- ifelse(.estimates == "corrected", "_corrected", "_uncorrected")
    
    output_reshaped <- dplyr::bind_rows(
      x$estimates %>% 
        dplyr::select(estimand, estimate, se, conf.low, conf.high, 
                      "att_level" = att_level_choose), 
      x$estimates %>% 
        dplyr::select(estimand, estimate, se, conf.low, conf.high, 
                      "att_level" = att_level_notchoose) %>% 
        dplyr::mutate(estimate = 1 - estimate,
                      conf.low = 1 - conf.low,
                      conf.high = 1 - conf.high)
    ) %>% 
      dplyr::filter(stringr::str_detect(estimand, corrected))
    
    
    if (!is.null(.labels)) {
      output_reshaped <- output_reshaped %>%
        dplyr::mutate(att_level = factor(att_level, labels = .labels))
    }
    
    g <- ggplot2::ggplot(output_reshaped) +
      ggthemes::theme_few() +
      ggplot2::geom_col(ggplot2::aes(x = att_level, y = estimate),
                        fill = "gray", width = 0.5) +
      ggplot2::geom_errorbar(ggplot2::aes(x = att_level, ymin = conf.low, ymax = conf.high),
                             width = 0.2) +
      labs(title = label_att, 
           y = estimand, 
           x = NULL)
    
    g
    return(g)
    
  } else {  # pointrange style
    
    out <- x$estimates %>%
      dplyr::filter(estimand == "mm_corrected") %>%
      dplyr::select(-estimand) %>%
      dplyr::left_join(x$labels %>% dplyr::select(level_id, level, attribute), 
                       by = c("att_level_choose" = "level_id")) %>%
      dplyr::rename(level1 = level, attribute1 = attribute) %>%
      dplyr::left_join(x$labels %>% dplyr::select(level_id, level, attribute), 
                       by = c("att_level_notchoose" = "level_id")) %>%
      dplyr::rename(level0 = level, attribute0 = attribute)
    
    
    if (nrow(out) != 1) {
      stop("This plotting function expects only one attribute-level pair at a time.")
    }
    
    g <- ggplot2::ggplot(out, ggplot2::aes(x = estimate, y = 1)) +
      ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray") +
      ggplot2::geom_pointrange(ggplot2::aes(xmin = conf.low, xmax = conf.high)) +
      ggplot2::geom_text(ggplot2::aes(label = format(round(estimate, 2), nsmall = 2)),
                         vjust = -1) +
      annotate("text", x = -0.08, y = 1, label = label_left, hjust = 1) +
      annotate("text", x = 1.08, y = 1, label = label_right, hjust = 0) +
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
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_blank()
      )
    } else {
      g <- g + ggplot2::labs(x = estimand)
    }
    
    return(g)
  }
}

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
#' @param .base_size Base font size.
#' @param .base_family Base font family.
#' @param .show_attribute Logical; show attribute as plot title (for pointrange type).
#' @param .xtitle X-axis title (for pointrange type).
#' @param .remove_xaxis Logical; remove x-axis (for pointrange type).
#' @param .xlim Limits of x-axis (for pointrange type).
#' @param .hjust_left Justification for left text (for pointrange type).
#' @param .hjust_right Justification for right text (for pointrange type).
#' @param .title_size Size of title text (for pointrange type).
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
    .base_size = 12,
    .base_family = "",
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
  
  # Validation
  
  if (!inherits(x, "projoint_results")) {
    stop("Input `x` must be a `projoint_results` object.")
  }
  
  .type <- match.arg(.type, choices = c("bar", "pointrange"))
  .estimates <- match.arg(.estimates, choices = c("corrected", "uncorrected"))

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
      ggthemes::theme_few(base_size = .base_size, base_family = .base_family) %+replace%
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = .base_size, color = "black", hjust = .5, vjust = 1),
        axis.text.y = ggplot2::element_text(size = .base_size, color = "black", hjust = 0, vjust = 0.5),
        axis.ticks = ggplot2::element_line(color = "grey50"),
        axis.title.y = ggplot2::element_text(size = .base_size, angle = 90, vjust = .01, hjust = .1)
      ) +
      ggplot2::geom_col(ggplot2::aes(x = att_level, y = estimate),
                        fill = "gray", width = 0.5) +
      ggplot2::geom_errorbar(ggplot2::aes(x = att_level, ymin = conf.low, ymax = conf.high),
                             width = 0.2)
    
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
    
    label_att <- out$attribute[1]
    label_left <- out$level0[1]
    label_right <- out$level1[1]
    
    g <- ggplot2::ggplot(out, ggplot2::aes(x = estimate, y = 1)) +
      ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray") +
      ggplot2::geom_pointrange(ggplot2::aes(xmin = conf.low, xmax = conf.high)) +
      ggplot2::geom_text(ggplot2::aes(label = format(round(estimate, 2), nsmall = 2)),
                         vjust = -1) +
      ggplot2::annotate("text", x = 0, y = 1, label = label_left, hjust = .hjust_left) +
      ggplot2::annotate("text", x = 1, y = 1, label = label_right, hjust = .hjust_right) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 4)(.xlim)) +
      ggplot2::labs(y = NULL, x = NULL) +
      ggplot2::coord_cartesian(xlim = .xlim, clip = "off") +
      ggthemes::theme_few(base_size = .base_size, base_family = .base_family) %+replace%
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = .base_size, color = "black", hjust = .5, vjust = 1),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        plot.margin = grid::unit(.plot.margin, "cm")
      )
    
    
    if (.show_attribute) {
      g <- g +
        ggplot2::labs(title = label_att) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = .title_size, face = "bold", vjust = -0.5))
    }
    
    if (.remove_xaxis) {
      g <- g + ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_blank()
      )
    } else {
      g <- g + ggplot2::labs(x = .xtitle)
    }
    
    return(g)
  }
}

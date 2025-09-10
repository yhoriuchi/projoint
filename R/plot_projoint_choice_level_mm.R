#' Plot choice-level marginal means (MMs) (helper)
#'
#' Internal helper used by \code{\link{plot.projoint_results}} to render
#' choice-level marginal means (MMs). Supports a bar chart or a horizontal
#' pointrange layout and optional custom level labels.
#'
#' @keywords internal
#'
#' @param x A \code{\link{projoint_results}} object produced by
#'   \code{\link{projoint}} with \code{structure = "choice_level"} and
#'   \code{estimand = "mm"}.
#' @param .type Character. Either \code{"bar"} (two bars with CIs) or
#'   \code{"pointrange"} (horizontal estimate with CI and level labels at the
#'   extremes). Default \code{"pointrange"}.
#' @param .estimates Character. Which estimate version to plot:
#'   \code{"corrected"} (default) or \code{"uncorrected"}.
#' @param .labels Optional character vector of length 2 for custom level labels
#'   (left/right). If \code{NULL}, labels are derived from \code{x$labels}.
#' @param .show_attribute Logical; if \code{TRUE} (default), add the attribute
#'   name as the title when both levels are from the same attribute.
#' @param .remove_xaxis Logical; if \code{TRUE}, remove x-axis line, ticks, and
#'   labels (useful when embedding). Default \code{FALSE}.
#' @param .xlim Numeric length-2 giving the x-axis limits. Default \code{c(0, 1)}.
#' @param .plot.margin Numeric vector of plot margins in cm,
#'   \code{c(top, left, bottom, right)}. Default \code{c(1, 2, 1, 2)}.
#' @param ... Currently unused (reserved for future extensions).
#'
#' @details
#' This helper expects that the \code{projoint_results} object \emph{already}
#' contains a single pair of choice-level MMs (i.e., one \code{att_level_choose}
#' and one \code{att_level_notchoose}). It is called internally by
#' \code{\link{plot.projoint_results}} when \code{x$structure == "choice_level"}
#' and \code{x$estimand == "mm"}.
#'
#' When \code{.type = "pointrange"}, the level labels are placed just outside
#' \code{.xlim} to avoid overlap with the confidence interval. If the two levels
#' correspond to different attributes, the attribute title is omitted and a
#' message is emitted.
#'
#' @return A \code{ggplot2} object.
#'
#' @seealso \code{\link{plot.projoint_results}} for the user-facing plot method.
#'
#' @examples
#' data(exampleData1)
#' dat <- reshape_projoint(exampleData1,
#'   .outcomes = c("choice1", "choice2", "choice1_repeated_flipped")
#' )
#' att <- unique(dat$labels$attribute_id)[1]
#' levs <- subset(dat$labels, attribute_id == att)$level_id
#' lev_names <- sub(".*:", "", levs)
#' q <- set_qoi("choice_level", "mm",
#'   .att_choose = att, .lev_choose = lev_names[2],
#'   .att_notchoose = att, .lev_notchoose = lev_names[1]
#' )
#' fit <- projoint(dat, .qoi = q)
#' plot(fit)
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
  if (!inherits(x, "projoint_results")) {
    stop("Input `x` must be a `projoint_results` object.")
  }
  
  .type <- match.arg(.type, choices = c("bar", "pointrange"))
  .estimates <- match.arg(.estimates, choices = c("corrected", "uncorrected"))
  
  # Attribute label logic
  label_att1 <- stringr::str_extract(x$estimates$att_level_choose, "att\\d+")[1]
  label_att0 <- stringr::str_extract(x$estimates$att_level_notchoose, "att\\d+")[1]
  
  if (label_att1 != label_att0) {
    message("The attributes are different between the two levels. Check your setting carefully. The attribute label is not added to the figure.")
    label_att <- NULL
  } else {
    label_att <- data.frame(attribute_id = label_att1) |>
      dplyr::left_join(x$labels, by = "attribute_id") |>
      dplyr::distinct(attribute) |>
      dplyr::pull()
  }
  
  estimand <- ifelse(x$estimand == "mm", "Marginal Mean", "Average Marginal Component Effect")
  
  # Resolve labels
  if (!is.null(.labels)) {
    if (length(.labels) != 2L) stop("`.labels` must be a character vector of length 2.")
  } else {
    labels_df <- x$labels
    label0 <- labels_df$level[match(x$estimates$att_level_notchoose[1], labels_df$level_id)]
    label1 <- labels_df$level[match(x$estimates$att_level_choose[1],    labels_df$level_id)]
    .labels <- c(label0, label1)
  }
  
  label0 <- .labels[1]
  label1 <- .labels[2]
  
  if (.type == "bar") {
    suffix <- if (.estimates == "corrected") "_corrected" else "_uncorrected"
    
    output_reshaped <- dplyr::bind_rows(
      x$estimates |>
        dplyr::select(estimand, estimate, se, conf.low, conf.high, "att_level" = att_level_choose),
      x$estimates |>
        dplyr::select(estimand, estimate, se, conf.low, conf.high, "att_level" = att_level_notchoose) |>
        dplyr::mutate(estimate = 1 - estimate,
                      conf.low = 1 - conf.low,
                      conf.high = 1 - conf.high)
    ) |>
      dplyr::filter(stringr::str_detect(estimand, suffix)) |>
      dplyr::left_join(x$labels, by = c("att_level" = "level_id"))
    
    if (!is.null(.labels)) {
      output_reshaped <- output_reshaped |>
        dplyr::mutate(att_level = factor(level, levels = .labels))
    }
    
    g <- ggplot2::ggplot(output_reshaped) +
      ggthemes::theme_few() +
      ggplot2::theme(plot.margin = grid::unit(.plot.margin, "cm")) +
      ggplot2::geom_col(ggplot2::aes(x = att_level, y = estimate), width = 0.5, fill = "gray") +
      ggplot2::geom_errorbar(ggplot2::aes(x = att_level, ymin = conf.low, ymax = conf.high), width = 0.2) +
      ggplot2::labs(title = label_att, y = estimand, x = NULL)
    
    return(g)
  }
  
  # pointrange
  target_estimand <- paste0("mm_", .estimates)
  
  out <- x$estimates |>
    dplyr::filter(.data$estimand == target_estimand) |>
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
    ggplot2::geom_text(ggplot2::aes(label = format(round(estimate, 2), nsmall = 2)), vjust = -1) +
    ggplot2::annotate("text", x = .xlim[1] - 0.08 * diff(.xlim), y = 1, label = label0, hjust = 1) +
    ggplot2::annotate("text", x = .xlim[2] + 0.08 * diff(.xlim), y = 1, label = label1, hjust = 0) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 4)(.xlim)) +
    ggplot2::labs(y = NULL, x = NULL) +
    ggplot2::coord_cartesian(xlim = .xlim, clip = "off") +
    ggthemes::theme_few() +
    ggplot2::theme(
      axis.text.x  = ggplot2::element_text(size = 11, color = "black", hjust = .5, vjust = 1),
      axis.text.y  = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.margin  = grid::unit(.plot.margin, "cm")
    )
  
  if (.show_attribute) {
    g <- g +
      ggplot2::labs(title = label_att) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 11, face = "bold", vjust = -0.5))
  }
  
  if (.remove_xaxis) {
    g <- g +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                     axis.line.x  = ggplot2::element_blank()) +
      ggplot2::labs(x = NULL)
  } else {
    g <- g + ggplot2::labs(x = estimand)
  }
  
  g
}

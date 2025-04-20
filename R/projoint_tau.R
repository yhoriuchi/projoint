#' Create a projoint_tau Object
#'
#' This function creates a \code{projoint_tau} S3 object containing
#' the estimated reliability (\code{tau}) and the associated diagnostic plot.
#'
#' @param irr A tibble containing the extrapolated IRR predictions.
#' @param figure A \code{ggplot2} object showing the extrapolation plot.
#'
#' @return An object of class \code{projoint_tau}.
#'
#' @keywords internal
projoint_tau <- function(irr, figure) {
  structure(
    list(
      irr = irr,
      figure = figure
    ),
    class = "projoint_tau"
  )
}

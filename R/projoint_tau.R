#' Create a projoint_tau Object
#'
#' Internal constructor for \code{projoint_tau} objects.  
#' A \code{projoint_tau} stores intra-respondent reliability (IRR) estimates
#' obtained via the extrapolation method, along with a diagnostic figure.
#' It is normally created by \code{\link{predict_tau}} and not called directly.
#'
#' @param irr A tibble (data frame) containing predicted IRR values as a function
#'   of the number of differing attributes between task pairs.
#'   Must include at least columns \code{x} (number of differing attributes) and
#'   \code{predicted} (estimated reliability).
#' @param figure A \code{ggplot2} object showing the extrapolation plot
#'   of IRR versus number of differing attributes.
#'
#' @return A list of class \code{"projoint_tau"} with elements:
#'   \itemize{
#'     \item \code{irr}: Tibble of IRR estimates and predictions.
#'     \item \code{figure}: A \code{ggplot2} object with the diagnostic plot.
#'   }
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

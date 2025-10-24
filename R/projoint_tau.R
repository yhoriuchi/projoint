#' Create a projoint_tau Object
#'
#' Internal constructor for \code{projoint_tau} objects.
#' A \code{projoint_tau} stores intra-respondent reliability (IRR) estimates
#' obtained via the extrapolation method, along with an optional diagnostic figure.
#'
#' @param irr A data frame (or tibble) with predicted IRR values as a function
#'   of the number of differing attributes between task pairs. Must include at
#'   least column \code{predicted}. If available, include \code{x} (number of differing attributes).
#' @param figure Optional. A \code{ggplot2} object showing the extrapolation plot
#'   of IRR versus number of differing attributes, or \code{NULL}.
#'
#' @return A list of class \code{"projoint_tau"} with elements:
#'   \itemize{
#'     \item \code{irr}: Data frame of IRR estimates and predictions.
#'     \item \code{figure}: A \code{ggplot2} object with the diagnostic plot, or \code{NULL}.
#'   }
#'
#' @keywords internal
#' @noRd
projoint_tau <- function(irr, figure = NULL) {
  if (!is.data.frame(irr)) {
    stop("`irr` must be a data frame (or tibble).", call. = FALSE)
  }
  if (!("predicted" %in% names(irr))) {
    stop("`irr` must contain a `predicted` column.", call. = FALSE)
  }
  if (!is.null(figure) && !inherits(figure, "ggplot")) {
    stop("`figure` must be a ggplot object or NULL.", call. = FALSE)
  }
  
  structure(
    list(
      irr = irr,
      figure = figure
    ),
    class = "projoint_tau"
  )
}

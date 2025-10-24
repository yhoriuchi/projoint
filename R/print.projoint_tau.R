#' Print method for projoint_tau objects
#'
#' Custom print method for objects of class \code{projoint_tau}, typically created
#' by \code{\link{projoint}} or related functions.
#'
#' @param x An object of class \code{projoint_tau}.
#' @param ... Additional arguments (currently unused).
#'
#' @return No return value; called for its side effect of printing a summary of
#'   the estimated intra-respondent reliability (\eqn{\tau}).
#'
#' @export
#'
#' @examples
#' toy_tau <- structure(
#'   list(irr = data.frame(predicted = 0.413, se = 0.02, n = 200)),
#'   class = "projoint_tau"
#' )
#' print(toy_tau)
print.projoint_tau <- function(x, ...) {
  irr <- x$irr
  if (!is.data.frame(irr) || !"predicted" %in% names(irr) || nrow(irr) < 1) {
    message("Tau: <unavailable>")
    return(invisible(x))
  }
  val <- irr$predicted[1]
  message("Tau estimated using the extrapolation method: ", round(val, 3))
  invisible(x)
}

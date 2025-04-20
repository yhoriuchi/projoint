#' Visualize the results of the extrapolation method for estimating tau.
#'
#' @param x An object of class \code{\link{projoint_tau}} derived from \code{\link{predict_tau}}
#' @param ... Optional arguments; currently none accepted
#' @export
plot.projoint_tau <- function(x, ...) {
  print(x$figure)
}

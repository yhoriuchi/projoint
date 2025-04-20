#' Print method for projoint_tau objects
#'
#' @keywords internal
#' @param x An object of class \code{\link{projoint_tau}} from \code{\link{predict_tau}}
#' @param ... Additional arguments (currently unused)
#' @export
#' @keywords internal
print.projoint_tau <- function(x, ...) {
  message(paste(
    "Tau estimated using the extrapolation method:",
    round(x$irr$predicted[1], 3)
  ))
}

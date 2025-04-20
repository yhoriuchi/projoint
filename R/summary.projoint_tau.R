#' Summary method for projoint_tau objects
#'
#' @keywords internal
#' @param object An object of class \code{\link{projoint_tau}} from \code{\link{predict_tau}}
#' @param ... Additional arguments (currently unused)
#' @return A tibble showing IRR estimates by the number of differing attributes.
#' @export
#' @keywords internal
summary.projoint_tau <- function(object, ...) {
  object$irr
}

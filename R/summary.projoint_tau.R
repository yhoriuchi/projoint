#' Summary method for projoint_tau objects
#'
#' Custom summary method for objects of class \code{projoint_tau}, typically created
#' by \code{\link{projoint}} or related functions. Summarizes intra-respondent
#' reliability (IRR) estimates.
#'
#' @param object An object of class \code{projoint_tau}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A tibble (data frame) showing IRR estimates, typically by the number
#'   of differing attributes, as stored in \code{object$irr}.
#'
#' @export
#'
#' @examples
#' toy_tau <- structure(
#'   list(irr = data.frame(predicted = 0.413, se = 0.02, n = 200)),
#'   class = "projoint_tau"
#' )
#' summary(toy_tau)
summary.projoint_tau <- function(object, ...) {
  irr <- object$irr
  if (is.null(irr) || !is.data.frame(irr)) {
    stop("Invalid 'projoint_tau' object: missing or invalid `irr`.", call. = FALSE)
  }
  irr
}

#' Summary method for projoint_tau objects
#'
#' Custom summary method for objects of class \code{projoint_tau}, typically created
#' by \code{\link{predict_tau}} or related functions. Summarizes intra-respondent
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
#' \donttest{
#' # Example workflow (assuming predict_tau is available):
#' # tau_fit <- projoint_tau(exampleData1)
#' # summary(tau_fit)
#' }
summary.projoint_tau <- function(object, ...) {
  object$irr
}

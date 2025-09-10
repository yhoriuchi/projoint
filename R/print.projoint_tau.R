#' Print method for projoint_tau objects
#'
#' Custom print method for objects of class \code{projoint_tau}, typically created
#' by \code{\link{predict_tau}} or related functions.
#'
#' @param x An object of class \code{projoint_tau}.
#' @param ... Additional arguments (currently unused).
#'
#' @return No return value, called for its side effect of printing a summary of
#'   the estimated intra-respondent reliability (\eqn{\tau}).
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Example workflow:
#' # tau_fit <- projoint_tau(exampleData1)
#' # print(tau_fit)
#' }
print.projoint_tau <- function(x, ...) {
  message(paste(
    "Tau estimated using the extrapolation method:",
    round(x$irr$predicted[1], 3)
  ))
}

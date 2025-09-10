#' Plot method for \code{projoint_tau}
#'
#' Visualizes the estimated intra-respondent reliability (\eqn{\tau}) produced
#' by the extrapolation method and stored in a \code{projoint_tau} object.
#'
#' @param x A \code{projoint_tau} object.
#' @param ... Optional arguments (currently unused).
#'
#' @return A \code{ggplot2} object representing the IRR (\eqn{\tau}) visualization.
#'   The plot is drawn for its side effect and also returned (invisibly).
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Estimate tau, then plot:
#' # dat <- reshape_projoint(exampleData1, .outcomes = c("choice1","choice2"))
#' # tau_fit <- projoint_tau(dat)  # or predict_tau(dat)
#' # p <- plot(tau_fit)            # also returns the ggplot object (invisibly)
#' }
plot.projoint_tau <- function(x, ...) {
  p <- x$figure
  print(p)
  invisible(p)
}

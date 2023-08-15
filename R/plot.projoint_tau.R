#' Visualize the results of the extrapolation method for estimating tau.
#'
#' @param x An object of class \code{\link{projoint_tau}} derived from \code{\link{predict_tau}}
#' @param ... Optional arguments; currently none accepted
#' @export
#' @examples
#' library(projoint)
#' library(stringr)
#' 
#' ## Example 1: repeated, flipped task
#' data("exampleData1")
#' head(exampleData1)
#'
#' outcomes <- paste0("choice", seq(from = 1, to = 8, by = 1))
#' outcomes <- c(outcomes, "choice1_repeated_flipped")
#' reshaped_data <- reshape_projoint(
#'   .dataframe = exampleData1, 
#'   .idvar = "ResponseId", 
#'   .outcomes = outcomes,
#'   .outcomes_ids = c("A", "B"),
#'   .alphabet = "K", 
#'   .repeated = TRUE,
#'   .flipped = TRUE)
#'   
#' tau1 <- predict_tau(reshaped_data)
#' plot(tau1)


plot.projoint_tau <- function(x, ...) {
  x@figure
}


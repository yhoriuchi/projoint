#' Create a projoint_results Object
#'
#' This function creates a \code{projoint_results} S3 object containing estimation results and metadata.
#' It is used internally by \code{\link{projoint}}.
#'
#' @param ... Named elements including estimates, labels, structure, estimand, irr, tau, and settings.
#'
#' @return An object of class \code{projoint_results}.
#'
#' @keywords internal
projoint_results <- function(...) {
  structure(
    list(...),
    class = "projoint_results"
  )
}

#' Print method for projoint_results
#'
#' @keywords internal
#' @param x An object of class \code{projoint_results}
#' @param ... Additional arguments (ignored)
#'
#' @export
print.projoint_results <- function(x, ...) {
  
  cat("\nProjoint results object\n")
  cat("-------------------------\n")
  cat("Estimand: ", x$estimand, "\n")
  cat("Structure: ", x$structure, "\n")
  cat("Standard error method: ", x$se_method, "\n")
  cat("IRR: ", x$irr, "\n")
  cat("Tau: ", round(x$tau, 3), "\n")
  cat("Number of estimates: ", nrow(x$estimates), "\n")
  invisible(x)
}

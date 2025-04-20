#' Summary method for projoint_results
#'
#' @keywords internal
#' @param object An object of class \code{projoint_results}
#' @param ... Additional arguments (ignored)
#'
#' @export
summary.projoint_results <- function(object, ...) {
  
  estimates_summary <- object$estimates
  
  cat("\nSummary of Projoint Estimates\n")
  cat("------------------------------\n")
  cat("Estimand: ", object$estimand, "\n")
  cat("Structure: ", object$structure, "\n")
  cat("Standard error method: ", object$se_method, "\n")
  cat("IRR: ", object$irr, "\n")
  cat("Tau: ", round(object$tau, 3), "\n")
  cat("\n")
  
  return(estimates_summary)
}

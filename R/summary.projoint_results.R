#' Summary method for projoint_results
#'
#' @keywords internal
#' @param object An object of class \code{projoint_results}
#' @param ... Additional arguments (ignored)
#'
#' @export
summary.projoint_results <- function(object, ...) {
  
  se_line <- if (is.null(object$se_type_used)) NA_character_ else object$se_type_used
  by_line <- if (is.null(object$cluster_by))   NA_character_ else object$cluster_by
  
  estimates_summary <- object$estimates
  
  cat("\nSummary of Projoint Estimates\n")
  cat("------------------------------\n")
  cat("Estimand: ", object$estimand, "\n", sep = "")
  cat("Structure: ", object$structure, "\n", sep = "")
  cat("Standard error method: ", object$se_method, "\n", sep = "")
  
  if (!is.na(se_line)) {
    if (!is.na(by_line)) {
      cat("SE type (lm_robust):   ", se_line, " (clustered by ", by_line, ")\n", sep = "")
    } else {
      cat("SE type (lm_robust):   ", se_line, "\n", sep = "")
    }
  }
  
  cat("IRR: ", object$irr, "\n", sep = "")
  cat("Tau: ", round(object$tau, 3), "\n", sep = "")
  cat("\n")
  
  return(estimates_summary)
}

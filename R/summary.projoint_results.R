#' Summary method for \code{projoint_results}
#'
#' Creates a concise tabular summary of a \code{projoint_results} object,
#' including the chosen estimand, analysis structure, standard-error settings,
#' and a data frame of estimates.
#'
#' @param object An object of class \code{projoint_results}.
#' @param ... Additional arguments (ignored).
#'
#' @return A data frame (often a tibble) summarizing the estimated effects.
#'   At minimum, it contains the columns produced in \code{object$estimates}
#'   (e.g., attribute/level identifiers and the point estimate with its
#'   standard error and confidence interval in columns such as
#'   \code{estimate}, \code{std.error}, \code{conf.low}, \code{conf.high}).
#'   This table is suitable for further processing or printing.
#'
#' @export
#'
#' @examples
#' \donttest{
#'   data(exampleData1)
#'
#'   # Reshape data for two base tasks + repeated (for IRR estimation)
#'   dat <- reshape_projoint(
#'     exampleData1,
#'     .outcomes = c("choice1", "choice2", "choice1_repeated_flipped")
#'   )
#'
#'   # Build a valid choice-level QoI
#'   att <- unique(dat$labels$attribute_id)[1]
#'   lev_ids   <- dat$labels$level_id[dat$labels$attribute_id == att]
#'   lev_names <- sub(".*:", "", lev_ids)
#'
#'   q <- set_qoi(
#'     .structure     = "choice_level",
#'     .estimand      = "mm",
#'     .att_choose    = att,
#'     .lev_choose    = lev_names[2],
#'     .att_notchoose = att,
#'     .lev_notchoose = lev_names[1]
#'   )
#'
#'   # Fit model
#'   fit <- projoint(dat, .qoi = q)
#'
#'   # Get the tabular summary of estimates
#'   tab <- summary(fit)
#'   head(tab)
#' }
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

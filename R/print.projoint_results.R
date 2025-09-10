#' Print method for projoint_results
#'
#' Custom print method for objects of class \code{projoint_results}.
#'
#' @param x An object of class \code{projoint_results}.
#' @param ... Additional arguments (ignored).
#'
#' @return No return value, called for its side effect of printing a summary of
#'   the \code{projoint_results} object to the console.
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(exampleData1)
#' dat <- reshape_projoint(
#'   exampleData1,
#'   .outcomes = c("choice1", "choice2", "choice1_repeated_flipped")
#' )
#' att <- unique(dat$labels$attribute_id)[1]
#' levs <- subset(dat$labels, attribute_id == att)$level_id
#' lev_names <- sub(".*:", "", levs)
#' q <- set_qoi(
#'   .structure     = "choice_level",
#'   .estimand      = "mm",
#'   .att_choose    = att,
#'   .lev_choose    = lev_names[2],
#'   .att_notchoose = att,
#'   .lev_notchoose = lev_names[1]
#' )
#' fit <- projoint(dat, .qoi = q)
#' print(fit)
#' }
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

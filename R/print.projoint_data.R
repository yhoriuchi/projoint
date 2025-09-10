#' Print a projoint_data object
#'
#' Custom print method for objects of class \code{projoint_data}.
#'
#' @param x A \code{projoint_data} object.
#' @param ... Additional arguments (currently unused).
#'
#' @return No return value, called for its side effect of printing a summary of
#'   the \code{projoint_data} object to the console.
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(exampleData1)
#' dat <- reshape_projoint(
#'   exampleData1,
#'   .outcomes = c("choice1", "choice2")
#' )
#' print(dat)
#' }
print.projoint_data <- function(x, ...) {
  cat("<projoint_data>\n")
  
  if (!is.null(x$data)) {
    cat("- data:    ", nrow(x$data), "rows,", ncol(x$data), "columns\n")
  } else {
    cat("- data:    NULL\n")
  }
  
  if (!is.null(x$labels)) {
    cat("- labels:  ", nrow(x$labels), "levels,", ncol(x$labels), "columns\n")
  } else {
    cat("- labels:  NULL\n")
  }
  
  invisible(x)
}

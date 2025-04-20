#' Print a projoint_data object
#'
#' Custom print method for objects of class `projoint_data`.
#'
#' @keywords internal
#' @param x A `projoint_data` object.
#' @param ... Additional arguments (currently unused).
#' @export
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

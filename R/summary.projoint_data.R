#' Summary for \code{projoint_data}
#'
#' Custom summary method for objects of class \code{projoint_data}. Prints a brief
#' overview of the main data and attribute-level labels contained in the object.
#'
#' @param object A \code{projoint_data} object.
#' @param ... Additional arguments (currently unused).
#'
#' @return No return value, called for its side effect of printing a summary of the
#'   \code{projoint_data} object to the console.
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
#' summary(dat)
#' }
summary.projoint_data <- function(object, ...) {
  cat("<Summary of projoint_data>\n\n")
  
  if (!is.null(object$data)) {
    cat("Main Data:\n")
    cat("- Number of rows:", nrow(object$data), "\n")
    cat("- Number of columns:", ncol(object$data), "\n")
    cat("- Sample columns:\n")
    print(utils::head(names(object$data), 5))
  } else {
    cat("- Main data is NULL\n")
  }
  
  cat("\n")
  
  if (!is.null(object$labels)) {
    cat("Attribute-Level Labels:\n")
    cat("- Number of levels:", nrow(object$labels), "\n")
    cat("- Attributes found:\n")
    print(unique(object$labels$attribute))
  } else {
    cat("- Labels data is NULL\n")
  }
  
  invisible(object)
}

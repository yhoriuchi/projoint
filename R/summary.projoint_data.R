#' Summarize a projoint_data object
#'
#' Custom summary method for objects of class `projoint_data`.
#'
#' @keywords internal
#' @param object A `projoint_data` object.
#' @param ... Additional arguments (currently unused).
#' @return A summary list (invisible).
#' @export
summary.projoint_data <- function(object, ...) {
  
  cat("<Summary of projoint_data>\n\n")
  
  if (!is.null(object$data)) {
    cat("Main Data:\n")
    cat("- Number of rows:", nrow(object$data), "\n")
    cat("- Number of columns:", ncol(object$data), "\n")
    
    cat("- Sample columns:\n")
    print(utils::head(names(object$data), 5))  # Show first 5 column names
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

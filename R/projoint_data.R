#' Create a projoint_data Object
#'
#' This function creates a \code{projoint_data} S3 object containing conjoint survey labels and response data.
#' It is used internally by \code{\link{reshape_projoint}} and \code{\link{make_projoint_data}}.
#'
#' @param labels A data frame of conjoint attribute and level labels, with attribute and level IDs.
#' @param data A data frame containing the reshaped conjoint survey responses.
#'
#' @return An object of class \code{projoint_data}.
#'
#' @keywords internal
projoint_data <- function(labels, data) {
  structure(
    list(
      labels = labels,
      data = data
    ),
    class = "projoint_data"
  )
}

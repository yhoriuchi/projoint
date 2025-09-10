#' Create a projoint_data Object
#'
#' Internal constructor for \code{projoint_data} objects.  
#' Used by \code{\link{reshape_projoint}} and \code{\link{make_projoint_data}} to
#' bundle conjoint survey labels and response data into a consistent structure.
#'
#' @param labels A data frame of conjoint attribute–level metadata. Must include
#'   attribute names, attribute IDs (e.g., \code{"att1"}), and level IDs
#'   (e.g., \code{"att1:lev1"}).
#' @param data A data frame (typically a tibble) containing the reshaped conjoint
#'   survey responses, one row per respondent–task–profile, with attribute columns,
#'   \code{selected}, \code{agree}, and any covariates.
#'
#' @return A list of length two with class \code{"projoint_data"}, containing:
#'   \itemize{
#'     \item \code{$labels}: attribute–level mapping
#'     \item \code{$data}: respondent–task–profile dataset
#'   }
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

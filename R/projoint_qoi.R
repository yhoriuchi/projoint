#' Create a projoint_qoi Object
#'
#' Internal constructor for \code{projoint_qoi} objects.  
#' A \code{projoint_qoi} stores the specification of custom
#' quantities of interest (QOIs) for conjoint estimation.
#' Typically called by \code{\link{set_qoi}} rather than directly.
#'
#' @param ... Named elements specifying QOI details, such as:
#'   \itemize{
#'     \item \code{structure}: Analysis level (\code{"profile_level"} or \code{"choice_level"}).
#'     \item \code{estimand}: Quantity of interest (\code{"mm"} or \code{"amce"}).
#'     \item \code{attribute_of_interest}, \code{levels_of_interest}, etc.
#'   }
#'
#' @return A list of QOI specifications with class \code{"projoint_qoi"}.
#'
#' @keywords internal
projoint_qoi <- function(...) {
  structure(
    list(...),
    class = "projoint_qoi"
  )
}

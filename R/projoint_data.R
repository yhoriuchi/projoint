#' Class generator for \code{\link{projoint_data}}
#' @importFrom methods is
#' @importFrom methods new
#' @keywords internal

projoint_data <- setClass("projoint_data", slots = c("labels", "data"))

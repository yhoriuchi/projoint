#' Class generator for \code{projoint_tau} objects
#' @importFrom methods is
#' @importFrom methods new
#' @keywords internal
#' @param slots Takes two slots: \code{irr} and \code{figure}
#' @param contains Inherits slots from \code{\link{projoint_data}}

projoint_tau <- setClass("projoint_tau",
                         slots = c("irr",
                                   "figure",
                                   "method"),
                         contains = c("projoint_data"))

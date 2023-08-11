#' Class generator for `projoint_tau`
#' @importFrom methods is
#' @importFrom methods new
#' @keywords internal
#' @param slots Takes two slots: `irr` and `figure`
#' @param contains Inherits slots from `projoint_data`

projoint_tau <- setClass("projoint_tau",
                         slots = c("irr",
                                   "figure",
                                   "method"),
                         contains = c("projoint_data"))

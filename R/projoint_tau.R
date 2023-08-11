#' Class generator for `projoint_tau`
#' @importFrom methods is
#' @importFrom methods new
#' @keywords internal
#' @param slots Takes two slots: `predicted_tau` and `figure`
#' @param contains Inherits slots from `projoint_data`

projoint_qoi <- setClass("projoint_tau",
                         slots = c("predicted_tau",
                                   "figure"),
                         contains = c("projoint_data"))

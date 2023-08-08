#' Class generators for `projoint_results_mm` and `projoint_results_amce`
#' @importFrom methods is
#' @importFrom methods new
#' @keywords internal
#' @param slots Takes two slots: `estimate`, a data frame and `tau`, a numerical scalar
#' @param contains Inherits slots from `projoint_data` and, optionally `projoint_qoi_mm` or `projoint_qoi_amce`

projoint_results_mm <- setClass("projoint_results_mm",
                                slots = c("estimates", "tau"),
                                contains = c("projoint_data", 
                                             "projoint_qoi_mm"))

projoint_results_amce <- setClass("projoint_results_amce",
                                  slots = c("estimates", "tau"),
                                  contains = c("projoint_data", 
                                               "projoint_qoi_amce"))
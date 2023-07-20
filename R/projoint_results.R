#' Class generators for `projoint_results_mm` and `projoint_results_amce`
#' @importFrom methods is
#' @importFrom methods new
#' @keywords internal
#' @param slots Takes one slot: `estimate`, a data frame.
#' @param contains Inherits slots from `projoint_data`, `projoint_irr`, and, optionally `projoint_qoi_mm` or `projoint_qoi_amce`

projoint_results_mm <- setClass("projoint_results_mm",
                                slots = c("estimates"),
                                contains = c("projoint_data", 
                                             "projoint_qoi_mm",
                                             "projoint_irr"))

projoint_results_amce <- setClass("projoint_results_amce",
                                  slots = c("estimates"),
                                  contains = c("projoint_data", 
                                               "projoint_qoi_amce",
                                               "projoint_irr"))
#' Class generator for `projoint_results`
#' @param slots Takes one slot: `estimate`, a data frame.
#' @param contains Inherits slots from `projoint_data`, `projoint_irr`, and, optionally, `projoint_structure`

projoint_results <- setClass("projoint_results",
                             slots = c("estimate"),
                             contains = c("projoint_data", 
                                          "projoint_qoi",
                                          "projoint_irr"))

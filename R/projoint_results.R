#' Class generator for `projoint_results`
#' @param slots Takes three slots: `irr`, `mm`, and `amce`. `irr` is a scalar, `mm` and `amce` are data frames.
#' @param contains Inherits slots from `projoint_data` and, optionally, `projoint_structure`

projoint_results <- setClass("projoint_results",
                             slots = c("irr", 
                                       "mm", 
                                       "amce"),
                             contains = c("projoint_data", 
                                          "projoint_structure"))

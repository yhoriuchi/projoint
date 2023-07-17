#' Class generator for `projoint.results`
#' @param slots Takes three slots: `irr`, `mm`, and `amce`. `irr` is a scalar, `mm` and `amce` are data frames.
#' @param contains Inherits slots from `projoint.data` and, optionally, `projoint.structure`

projoint.results <- setClass("projoint.results",
                          slots = c("irr","mm", "amce"),
                          contains = c("projoint.data", "projoint.structure")

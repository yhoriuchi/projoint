#' Class generator for `projoint.results`
#' @param slots Takes four slots: `irr`, `mm`, `amce`, and `structure`. `irr` is a scalar, `mm` and `amce` are data frames, and `structure` is a list.
#' @param contains Inherits slots from `projoint.data`

projoint.results <- setClass("projoint.results",
                          slots = c("irr","mm", "amce", "structure"),
                          contains = "projoint.data")

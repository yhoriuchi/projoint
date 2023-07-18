#' Class generator for `projoint_qoi`
#' @importFrom methods is
#' @importFrom methods new
#' @param slots Takes two slots: a scalar indicating the attribute of interest, and a scalar or vector indicating the levels of interest

projoint_qoi <- setClass("projoint_qoi",
                         slots = c("attribute_of_interest",
                                   "levels_of_interest"))

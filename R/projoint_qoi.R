#' Class generator for `data`
#' @param slots Takes two slots

projoint_qoi <- setClass("projoint_qoi",
                         slots = c("attribute_of_interest",
                                   "levels_of_interest"))

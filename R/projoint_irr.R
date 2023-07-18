#' Class generator for `projoint_irr`
#' @param slots Takes two slots: a scalar for irr, and a ggplot object for figure

projoint_qoi <- setClass("projoint_irr",
                         slots = c("irr",
                                   "figure"))

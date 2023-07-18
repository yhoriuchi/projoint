#' Class generator for `projoint_irr`
#' @importFrom methods is
#' @importFrom methods new
#' @param slots Takes two slots: a scalar for irr, and a ggplot object for figure

projoint_irr <- setClass("projoint_irr",
                         slots = c("irr",
                                   "figure"))

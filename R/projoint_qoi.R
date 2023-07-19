#' Class generator for `projoint_qoi`
#' @importFrom methods is
#' @importFrom methods new
#' @param slots Takes two or three slots: a scalar indicating the attribute of interest, a scalar or vector indicating the levels of interest, and a scalar or vector indicating the baseline levels of interest (for AMCE)

projoint_qoi_mm <- setClass("projoint_qoi_mm",
                            slots = c("attribute_of_interest",
                                      "levels_of_interest"))

projoint_qoi_amce <- setClass("projoint_qoi_amce",
                              slots = c("attribute_of_interest",
                                        "levels_of_interest",
                                        "baseline"))

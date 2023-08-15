#' Class generator for \code{projoint_qoi}
#' @importFrom methods is
#' @importFrom methods new
#' @keywords internal
#' @param slots Takes four for MMs and eight for AMCEs

projoint_qoi <- setClass("projoint_qoi",
                         slots = c("attribute_of_interest",
                                   "levels_of_interest",
                                   "attribute_of_interest_0",
                                   "levels_of_interest_0",
                                   "attribute_of_interest_baseline",
                                   "levels_of_interest_baseline",
                                   "attribute_of_interest_0_baseline",
                                   "levels_of_interest_0_baseline"))

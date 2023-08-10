#' Class generators for `projoint_results_mm` and `projoint_results_amce`
#' @importFrom methods is
#' @importFrom methods new
#' @keywords internal
#' @param slots Takes 10 slots: `estimate`, a data frame and `tau`, a numerical scalar, ... [to be written]
#' @param contains Inherits slots from `projoint_data`

projoint_results <- setClass("projoint_results",
                             slots = c("estimates", 
                                       "tau",
                                       "attribute_of_interest",
                                       "levels_of_interest",
                                       "attribute_of_interest_0",
                                       "levels_of_interest_0",
                                       "attribute_of_interest_baseline",
                                       "levels_of_interest_baseline",
                                       "attribute_of_interest_0_baseline",
                                       "levels_of_interest_0_baseline"),
                             contains = c("projoint_data"))

# projoint_results_mm <- setClass("projoint_results_mm",
#                                 slots = c("estimates", "tau"),
#                                 contains = c("projoint_data", 
#                                              "projoint_qoi"))
# 
# projoint_results_amce <- setClass("projoint_results_amce",
#                                   slots = c("estimates", "tau"),
#                                   contains = c("projoint_data", 
#                                                "projoint_qoi"))
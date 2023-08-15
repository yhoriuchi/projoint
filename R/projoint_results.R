#' Class generators for \code{projoint_results}
#' @importFrom methods is
#' @importFrom methods new
#' @keywords internal
#' @param slots Takes 16 slots. See documentation for \code{\link{projoint}}.
#' @param contains Inherits slots from \code{\link{projoint_data}}

projoint_results <- setClass("projoint_results",
                             slots = c("estimand",
                                       "structure",
                                       "estimates",
                                       "se_method",
                                       "irr",
                                       "tau",
                                       "remove_ties",
                                       "ignore_position",
                                       "attribute_of_interest",
                                       "levels_of_interest",
                                       "attribute_of_interest_0",
                                       "levels_of_interest_0",
                                       "attribute_of_interest_baseline",
                                       "levels_of_interest_baseline",
                                       "attribute_of_interest_0_baseline",
                                       "levels_of_interest_0_baseline"),
                             contains = c("projoint_data"))
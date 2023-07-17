#' Class generator for `data`
#' @param slots Takes ?? slots. `unit` can be either "choice" or "profile". 

projoint.structure <- setClass("projoint.structure",
                          slots = c("unit",
                                    "attribute_of_interest",
                                    "levels_of_interest",
                                    "remove_ties"))

#' Set the quantities of interest
#'
#' This function allows users to specify more fine-tuned details of their conjoint analysis. In particular, users can set very specific quantities of interest aside from simple AMCEs or MMs.
#'
#' @import rlang
#' @import stringr
#' @param .structure Either \code{"choice_level"} (default) or \code{"profile_level"}
#' @param .estimand Either \code{"mm"} for marginal mean or \code{"amce"} for average marginal component effect
#' @param .att_choose A character column name identifying the attribute of interest (i.e., for the attribute-level or attribute-levels \strong{chosen}).
#' @param .lev_choose  A character vector identifying the level or levels of interest (i.e., for the attribute-level or attribute-levels \strong{chosen}). Its length should be 1 for profile-level analysis and 1+ for choice-level analysis
#' @param .att_notchoose A character column name identifying the attribute of interest (i.e., for the attribute-level or attribute-levels \strong{not chosen}). This argument should be specified only if the \code{.structure} argument is \code{"choice-level"}.
#' @param .lev_notchoose  A character vector identifying the level or levels of interest (i.e., for the attribute-level or attribute-levels \strong{not chosen}). Its length should be 1 for profile-level analysis and 1+ for choice-level analysis. This argument should be specified only if the \code{.structure} argument is \code{"choice-level"}.
#' @param .att_choose_b [baseline for AMCE] A character column name identifying the attribute of interest (i.e., for the attribute-level or attribute-levels \strong{chosen}).
#' @param .lev_choose_b  [baseline for AMCE] A character vector identifying the level or levels of interest (i.e., for the attribute-level or attribute-levels \strong{chosen}). Its length should be 1 for profile-level analysis and 1+ for choice-level analysis
#' @param .att_notchoose_b [baseline for AMCE] A character column name identifying the attribute of interest (i.e., for the attribute-level or attribute-levels \strong{not chosen*}). This argument should be specified only if the \code{.structure} argument is \code{"choice-level"}.
#' @param .lev_notchoose_b [baseline for AMCE]  A character vector identifying the level or levels of interest (i.e., for the attribute-level or attribute-levels \strong{not chosen}). Its length should be 1 for profile-level analysis and 1+ for choice-level analysis. This argument should be specified only if the \code{.structure} argument is \code{"choice-level"}.
#' @return A \code{\link{projoint_qoi}} object
#' @export

set_qoi <- function(
    .structure = "choice_level",
    .estimand = "mm",
    .att_choose, 
    .lev_choose,
    .att_notchoose = NULL, 
    .lev_notchoose = NULL,
    .att_choose_b = NULL, 
    .lev_choose_b = NULL,
    .att_notchoose_b = NULL, 
    .lev_notchoose_b = NULL
){
  
  
  # check -------------------------------------------------------------------
  
  structure  <- rlang::arg_match0(.structure, c("choice_level", "profile_level"))
  estimand  <- rlang::arg_match0(.estimand, c("mm", "amce"))
  
  # return ------------------------------------------------------------------
  
  projoint_qoi("structure" = structure,
               "estimand" = estimand,
               "attribute_of_interest" = .att_choose,
               "levels_of_interest" = .lev_choose,
               "attribute_of_interest_0" = .att_notchoose,
               "levels_of_interest_0" = .lev_notchoose,
               "attribute_of_interest_baseline" = .att_choose_b,
               "levels_of_interest_baseline" = .lev_choose_b,
               "attribute_of_interest_0_baseline" = .att_notchoose_b,
               "levels_of_interest_0_baseline" = .lev_notchoose_b)
  
}
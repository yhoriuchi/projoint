#' Set the quantities of interest (QoIs)
#'
#' Constructs a quantities-of-interest (QoI) specification for \pkg{projoint}.
#' Use this to request specific estimands—marginal means (MMs) or average
#' marginal component effects (AMCEs)—at either the choice- or profile-level,
#' and to declare which attribute levels are compared (including baselines).
#'
#' @param .structure Either \code{"choice_level"} (default) or \code{"profile_level"}.
#' @param .estimand Either \code{"mm"} for marginal means or \code{"amce"} for
#'   average marginal component effects.
#' @param .att_choose Character scalar: the attribute (column) for the level(s)
#'   that are \emph{chosen}.
#' @param .lev_choose Character vector: the level id(s) for the \emph{chosen}
#'   side. Length 1 for profile-level, \eqn{\ge}1 for choice-level.
#' @param .att_notchoose Character scalar: the attribute (column) for the level(s)
#'   that are \emph{not chosen}. Only used for \code{.structure == "choice_level"}.
#' @param .lev_notchoose Character vector: the level id(s) for the \emph{not chosen}
#'   side. Length 1 for profile-level, \eqn{\ge}1 for choice-level. Only used for
#'   \code{.structure == "choice_level"}.
#' @param .att_choose_b Character scalar: \emph{baseline} attribute for the
#'   chosen side when computing AMCEs.
#' @param .lev_choose_b Character vector: \emph{baseline} level id(s) for the
#'   chosen side when computing AMCEs. Length 1 for profile-level, \eqn{\ge}1 for
#'   choice-level.
#' @param .att_notchoose_b Character scalar: \emph{baseline} attribute for the
#'   not-chosen side (choice-level only) when computing AMCEs.
#' @param .lev_notchoose_b Character vector: \emph{baseline} level id(s) for the
#'   not-chosen side (choice-level only) when computing AMCEs.
#'
#' @return A \code{projoint_qoi} object (list-like) containing fields such as:
#'   \code{structure}, \code{estimand}, \code{attribute_of_interest},
#'   \code{levels_of_interest}, and their baseline counterparts. This object
#'   can be supplied to downstream estimation helpers that accept a QoI spec.
#'
#' @export
#'
#' @examples
#' # Specify a simple choice-level MM comparison for att1 levels:
#' q_mm <- set_qoi(
#'   .structure  = "choice_level",
#'   .estimand   = "mm",
#'   .att_choose = "att1",
#'   .lev_choose = c("att1:lev2"),
#'   .att_notchoose = "att1",
#'   .lev_notchoose = c("att1:lev1")
#' )
#' str(q_mm)
#'
#' # Example AMCE with explicit baselines (profile-level):
#' q_amce <- set_qoi(
#'   .structure   = "profile_level",
#'   .estimand    = "amce",
#'   .att_choose  = "att2",
#'   .lev_choose  = "att2:lev3",
#'   .att_choose_b = "att2",
#'   .lev_choose_b = "att2:lev1"
#' )
#' str(q_amce)
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
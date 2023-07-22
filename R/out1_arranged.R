#' Reorganized Projoint Example Data Set 1
#'
#' This data set is identical to Example Data Set 1, except its attributes and levels have been manually relabeled and reordered.
#' 
#' @format ## `exampleData1`
#' A data frame with 201 rows and 186 columns:
#' \describe{
#'   \item{ResponseId}{Unique respondent ID}
#'   \item{race}{Respondent race: 6 categories}
#'   \item{party_1}{Respondent party: Democrat, Republican, Independent, Something else}
#'   \item{party_2}{If party_1 is not D or R: closer to Democrats or closer to Republicans?}
#'   \item{party_3}{If party_1 is R: strong R or not very strong R}
#'   \item{party_4}{If party_1 is D: strong D or not very strong D}
#'   \item{ideology}{Respondent ideology from Extremely liberal to Extremely conservative}
#'   \item{honesty}{Attention check: respondents should select "never"}
#'   \item{choice1, choice2, ..., choice8}{Respondent selections for the initial 8 conjoint tasks}
#'   \item{choice1_repeated_flipped}{Respondent choice for the task that repeats choice1 but flipped}
#' }
#' @source Qualtrics and Prolific; see Clayton et al. replication materials.
"out1_arranged"
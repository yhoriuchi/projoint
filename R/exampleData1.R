#' Projoint Example Data Set 1: Building Conjoint with a Repeated, Flipped Task
#'
#' A cleaned Qualtrics output of a conjoint study that compares two potential
#' new building developments. There are 8 standard tasks as well as a repeat
#' of the first task, used to calculate response instability.
#'
#' @format ## `exampleData1`
#' A data frame with 201 rows and 186 columns:
#' \describe{
#'   \item{ResponseId}{Unique respondent ID}
#'   \item{choice1, choice2, ..., choice8}{Respondent selections for the initial 8 conjoint tasks}
#'   \item{choice1_repeated_flipped}{Respondent choice for the task that repeats choice1 but flipped}
#'   \item{race}{Respondent race: 5 categories}
#'   \item{party1}{Respondent party}
#'   \item{party2}{If party1 is not D or R: closer to Democrats or closer to Republicans?}
#'   \item{party3}{If party1 is R: strong R or not very strong R}
#'   \item{party4}{If party1 is D: strong D or not very strong D}
#'   \item{ideology}{Respondent ideology from Extremely liberal to Extremely conservative}
#'   \item{hoensty}{Attention check: respondents should select "never"}
#'   \item{K-1-1, K-1-2, ..., K-8-5}{K-X-Y indicates the name of the attribute that is in the Yth position in the table of the Xth task}
#'   \item{K-1-1-1, K-1-1-2, ..., K-8-2-7}{K-X-Y-Z indicates the value of the attribute that is in the Zth position for the Yth profile (either 1 or 2 for left and right profiles) for the Xth task}
#' }
#' @source Qualtrics and Prolific; see Clayton et al. replication materials.
"exampleData1"
#' Projoint Example Data Set 2: Building Conjoint with a Repeated, Unflipped Task
#'
#' A cleaned Qualtrics output of a conjoint study that compares two potential
#' new building developments. There are 8 standard tasks as well as a repeat
#' of the first task, used to calculate response instability.
#'
#' @format ## `exampleData2`
#' A data frame with 478 rows and 132 columns:
#' \describe{
#'   \item{ResponseId}{Unique respondent ID}
#'   \item{gender}{Respondent gender}
#'   \item{age}{Year}
#'   \item{choice1, choice2, ..., choice8}{Respondent selections for the initial 8 conjoint tasks}
#'   \item{choice1_repeated_notflipped}{Respondent choice for the task that repeats choice1, unflipped}
#'   \item{K-1-1, K-1-2, ..., K-10-6}{K-X-Y indicates the name of the attribute that is in the Yth position in the table of the Xth task}
#'   \item{K-1-1-1, K-1-1-2, ..., K-10-2-4}{K-X-Y-Z indicates the value of the attribute that is in the Zth position for the Yth profile (either 1 or 2 for left and right profiles) for the Xth task}
#' }
#' @source Qualtrics and Prolific; see Clayton et al. replication materials.
"exampleData2"
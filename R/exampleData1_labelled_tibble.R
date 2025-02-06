#' Projoint Example Data Set 1: "labelled tibble"
#'
#' An already cleaned data frame (tibble) with each attribute corresponds to each column with the proper attribute name. The unit of observation must be The unit of observation must be each of two profiles in each task for each respondent.
#'
#' @format ## `exampleData1_labelled_tibble`
#' A data frame with 6,400 rows and 14 columns:
#' \describe{
#'   \item{id}{Unique respondent ID}
#'   \item{task}{Task number}
#'   \item{profile}{Profile number = \{1,2\}}
#'   \item{selected}{Whether or not a profile was selected}
#'   \item{selected_repeated}{Whether or not a profile was selected in the repeated task}
#'   \item{School Quality, ... Type of Place}{attributes}
#'   \item{race}{Respondent race: 6 categories}
#'   \item{ideology}{Respondent ideology from Extremely liberal to Extremely conservative}
#' }
#' @source Qualtrics and Prolific; see Clayton et al. replication materials.
"exampleData1_labelled_tibble"
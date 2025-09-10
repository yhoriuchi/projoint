#' Projoint Example Data Set 1: "Labelled Tibble"
#'
#' A cleaned tibble where each attribute corresponds to a separate column with a
#' descriptive attribute name. The unit of observation is each of two profiles in
#' each task for each respondent.
#'
#' @format A tibble with 6,400 rows and 14 columns. Contains survey responses
#'   including outcome choices and conjoint attribute values (columns typically
#'   named with a `K-*-*` convention).
#'
#' @source Qualtrics survey on Prolific; see Clayton et al. replication materials.
#'
#' @usage data(exampleData1_labelled_tibble)
#'
#' @details This dataset is intended for illustrating reading, reshaping, and
#'   analysis workflows in \pkg{projoint}. Column names are compatible with
#'   \code{reshape_projoint()}.
#'
#' @examples
#' # Load the data
#' data(exampleData1_labelled_tibble)
#'
#' # Basic inspection (fast and always runnable)
#' head(exampleData1_labelled_tibble)
#' dim(exampleData1_labelled_tibble)
#'
#' # Optional: quick structure peek (names only)
#' names(exampleData1_labelled_tibble)
#'
#' @name exampleData1_labelled_tibble
#' @docType data
#' @keywords datasets
NULL


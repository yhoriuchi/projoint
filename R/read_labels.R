#' Reads in a CSV of reordered attributes and levels, and applies it to a `projoint_data` object.
#'
#' For users interested in reordering the attributes and levels of their conjoint data set.
#' First save the existing order to a CSV using `save_labels`, then manually reorder them in the CSV.
#' Finally, use this function to read in the modified CSV and automatically apply the new order to the existing `projoint_data`.
#' 
#' @import readr
#' @import dplyr
#' @import forcats
#' @import tidyr
#' @import stringr
#' @import tidyselect
#' @param .data A `projoint_data` object
#' @param .filename The name of a revised CSV file, originally derived from `save_labels`, after manual arrangement
#' @return A projoint object of class `projoint_data` ready to pass to `projoint()`.
#' @export
#' @examples
#' library(projoint)
#' library(readr)
#' 
#' data("exampleData1")
#' head(exampleData1)
#'
#' # Write outcome column names
#' outcomes <- paste0("choice", seq(from = 1, to = 8, by = 1))
#' outcomes <- c(outcomes, "choice1_repeated_flipped")
#' 
#' # Reshape the data
#' reshaped_data <- reshape_projoint(
#'   .dataframe = exampleData1, 
#'   .idvar = "ResponseId", 
#'   .outcomes = outcomes,
#'   .outcomes_ids = c("A", "B"),
#'   .alphabet = "K", 
#'   .repeated = TRUE,
#'   .flipped = TRUE)
#'   
#' # Save a CSV file (Not Run)
#' # save_labels(reshaped_data, "data-raw/labels_original.csv")
#' 
#' # Not Run: Read a revised CSV file
#' # reshaped_data_arranged <- read_labels(reshaped_data, "data-raw/labels_arranged.csv")

read_labels <- function(
    .data,
    .filename
){
  
  # bind variables locally to the function
  
  attribute_id <- NULL
  attribute_id_arranged <- NULL
  level_id <- NULL
  level_id_arranged <- NULL
  value <- NULL
  name <- NULL
  attribute <- NULL
  level <- NULL
  
  labels_arranged <- readr::read_csv(.filename, 
                                     show_col_types = FALSE) %>% 
    dplyr::mutate(attribute_id = stringr::str_extract(level_id, "^.+(?=\\:)"),
                  attribute_id_arranged = forcats::fct_reorder(attribute_id, order),
                  attribute_id_arranged = stringr::str_c("att", as.numeric(attribute_id_arranged))) %>% 
    dplyr::arrange(order) %>% 
    dplyr::group_by(attribute_id_arranged) %>%
    dplyr::mutate(level_id_arranged = dplyr::row_number(),
                  level_id_arranged = stringr::str_c(attribute_id_arranged, ":level", level_id_arranged)) %>%
    dplyr::ungroup() 
  
  data_arranged <- .data@data %>% 
    tidyr::pivot_longer(cols = tidyselect::contains("att")) %>% 
    dplyr::mutate(value = as.character(value)) %>% 
    dplyr::left_join(labels_arranged, 
                     by = c("value" = "level_id")) %>% 
    dplyr::select(-name, -value, -attribute, -attribute_id, -level, -order) %>% 
    tidyr::pivot_wider(names_from = attribute_id_arranged, 
                       values_from = level_id_arranged)
  
  labels_arranged <- labels_arranged %>% 
    dplyr::select(-order, -attribute_id, -level_id) %>% 
    dplyr::rename(attribute_id = attribute_id_arranged,
                  level_id = level_id_arranged)
  
  # return the data frame and the variable labels as a list
  out <- projoint_data("labels" = labels_arranged, 
                       "data" = data_arranged)
  
  return(out)
  
}
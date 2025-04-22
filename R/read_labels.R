#' Reads in a CSV of reordered attributes and levels, and applies it to a \code{\link{projoint_data}} object.
#'
#' For users interested in reordering the attributes and levels of their conjoint data set.
#' First save the existing order to a CSV using \code{\link{save_labels}}, then manually reorder them in the CSV.
#' Finally, use this function to read in the modified CSV and automatically apply the new order to the existing \code{\link{projoint_data}}.
#' 
#' @import readr
#' @import dplyr
#' @import forcats
#' @import tidyr
#' @import stringr
#' @import tidyselect
#' @param .data A \code{\link{projoint_data}} object
#' @param .filename The name of a revised CSV file, originally derived from \code{\link{save_labels}}, after manual arrangement
#' @return A projoint object of class \code{\link{projoint_data}} ready to pass to \code{\link{projoint}}.
#' @export
read_labels <- function(
    .data,
    .filename
){
  
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
  
  data_arranged <- .data$data %>% 
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
  
  # return new projoint_data object
  out <- projoint_data("labels" = labels_arranged, 
                       "data" = data_arranged)
  
  return(out)
  
}
#' Calculate marginal means given a data set
#'
#' @param .data A conjoint data set.
#' @return A summarized data set of marginal means for each attribute-level



calculate_mm <- function(.data){

  selected <- NULL
  id <- NULL
  att_level <- NULL

  .data %>%
    dplyr::filter(!is.na(selected)) %>%
    dplyr::select(id, selected, dplyr::contains("att")) %>%
    tidyr::pivot_longer(3:ncol(.), names_to = "attribute", values_to = "att_level") %>%
    dplyr::group_by(att_level) %>%
    dplyr::summarize(mm = mean(selected), .groups = "drop") %>%
    dplyr::ungroup()

}

#' Calculates the intra-respondent reliability (IRR), average marginal component effect (AMCE), marginal mean (MM), and correcte AMCEs and MMs for a given conjoint data set, as well as bootstrapped confidence intervals.
#'
#' @param .data A conjoint data set.
#' @return A summarized data set describing the results of a conjoint analysis, corrected for IRR measurement error.




pj_bs <- function(.data, .id = "id", .n_boot = 500, .diff = NULL){
  
  # Unique IDs
  ids <- .data %>%
    dplyr::select(dplyr::all_of(.id)) %>%
    dplyr::distinct() %>%
    pull()
  
  # Bootstrapped IDs
  bs_ids <- sample(x = ids, size = length(ids) * .n_boot, replace = TRUE)
  
  # Bootstrapped sample IDs
  sample <- rep(1:.n_boot, length(ids))
  
  # Bootstrapped data frames
  bs_data <- data.frame(sample = sample,
                        id = bs_ids) %>%
    dplyr::left_join(.data, by = "id")
  
  # Return
  purrr::map_dfr(1:.n_boot, pj_bs_sample, bs_data, .diff)
  
}

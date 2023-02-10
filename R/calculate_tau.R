#' Calculate intra-respondent reliability (IRR) from a conjoint data set. This is an internal function.
#'
#' @param .data A conjoint data set.
#' @return A scalar indicating IRR.



calculate_tau <- function(.data){

  profile <- NULL
  selected <- NULL
  selected_repeated <- NULL

  .data %>%
    # select just one profile of a repeated task
    dplyr::filter(profile == 1 & !is.na(selected_repeated)) %>%
    # calculate tau (swapping error)
    dplyr::mutate(tau = ifelse(selected != selected_repeated, 1, 0)) %>%
    # save a vector
    dplyr::pull(tau) %>%
    # calculate a mean
    mean()

}

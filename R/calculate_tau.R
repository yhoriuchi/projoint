#' Calculate "tau" (1 - Inter-coder Reliability)
#'
#' Based on the repeated task, calculate the proportion of tasks with different choices
#'
#' @import dplyr
#' @param .data A data frame (ashaped for conjoint analysis)
#' @return A numeric
#'

calculate_tau <- function(.data){

  task <- profile <- selected <- selected_repeated <- same <- NULL

  .data %>%
    # need only one task/profile to calculate tau
    filter(task == 1 & profile == 1) %>%
    mutate(same = selected != selected_repeated) %>%
    pull(same) %>%
    mean()

}

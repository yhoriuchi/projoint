#' Reshape data for conjoint analysis
#'
#' Reshape the wide-format data to the long-format data
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @param .data A data frame
#' @param .idvar A variable name for the respondent identifier
#' @param .outcomes A character vector of outcome variables
#' @param .alphabet The alphabet used for conjoint attribute-levels
#' @param .flipped TRUE if the profiles of the repeated task are flipped (recommended)
#' @return A data frame (long format) ready for conjoint analysis
#' @export
#'

reshape_conjoint <- function(.data, .idvar, .outcomes, .alphabet, .flipped = TRUE)
{

  . <- code <- x <- name <- attribute <- attribute_name <- NULL
  level_name <- task <- outcome_qnum <- outcomes <- NULL
  profile <- response <- selected <- selected_repeated <-  NULL

  idvar_quo <- enquo(.idvar)
  n_tasks <- length(.outcomes)

  df <- .data %>% mutate(id = !!idvar_quo)

  temp1 <- df %>%
    select(id, contains(paste0(.alphabet, "-"))) %>%
    pivot_longer(names_to = "code", values_to = "name", cols = 2:ncol(.)) %>%
    filter(str_detect(code, paste0(.alphabet, "-\\d+-\\d+$"))) %>%
    separate(code, into = c("x", "task", "attribute"), sep = "\\-") %>%
    select(-x) %>%
    rename(attribute_name = name)

  temp2 <- df %>%
    select(id, contains(paste0(.alphabet, "-"))) %>%
    pivot_longer(names_to = "code", values_to = "name", cols = 2:ncol(.)) %>%
    filter(str_detect(code, paste0(.alphabet, "-\\d+-\\d+-\\d+"))) %>%
    separate(code, into = c("x", "task", "profile", "attribute"), sep = "\\-") %>%
    select(-x) %>%
    rename(level_name = name)

  attribute_levels <- left_join(temp1, temp2,
                                by = c("id", "task", "attribute")) %>%
    select(-attribute) %>%
    mutate_at(c("task", "profile"), .funs = as.numeric) %>%
    rename(attribute = attribute_name,
           level = level_name) %>%
    filter(task <= n_tasks) %>%
    group_by(id, task) %>%
    pivot_wider(names_from = "attribute", values_from = "level")

  attribute_levels_repeated <- attribute_levels %>%
    filter(task == 1)

  responses <- df %>%
    select(id, all_of(.outcomes)) %>%
    pivot_longer(names_to = "outcome_qnum", values_to = "response", cols = 2:ncol(.)) %>%
    mutate(task = NA)

  for (i in 1:n_tasks) {
    responses <- responses %>%
      mutate(task = ifelse(outcome_qnum == .outcomes[i], i, task))
  }

  response_cleaned <- responses %>%
    mutate(selected = str_extract(response, "\\d") %>% as.integer()) %>%
    select(-response, -outcome_qnum)


  # Tasks to estimate AMCEs/MMs
  out1 <- attribute_levels %>%
    left_join(response_cleaned %>%
                filter(task != n_tasks), by = c("id", "task")) %>%
    mutate(selected = ifelse(profile == selected, 1, 0))

  # Tasks to estimate ICR
  out2 <- attribute_levels_repeated %>%
    left_join(response_cleaned %>%
                filter(task == n_tasks) %>%
                mutate(task = 1), by = c("id", "task")) %>%
    mutate(selected = case_when(profile == selected & .flipped == FALSE ~ 1,
                                profile == selected & .flipped == TRUE  ~ 0,
                                profile != selected & .flipped == FALSE ~ 0,
                                profile != selected & .flipped == TRUE  ~ 1)) %>%
    rename(selected_repeated = selected)

  # Merge and return
  left_join(out1, out2) %>%
    return()

}

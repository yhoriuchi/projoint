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
#' @return A data frame (long format) ready for conjoint analysis
#' @export
#'
reshape_conjoint <- function(.data, .idvar, .outcomes, .alphabet)
{

  . <- code <- x <- name <- attribute <- attribute_name <- NULL
  level_name <- task <- outcome_qnum <- outcomes <- NULL

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

  responses <- df %>%
    select(id, all_of(.outcomes)) %>%
    pivot_longer(names_to = "outcome_qnum", values_to = "response", cols = 2:ncol(.)) %>%
    mutate(task = NA)

  for (i in 1:n_tasks) {
    responses <- responses %>%
      mutate(task = ifelse(outcome_qnum == .outcomes[i], i, task))
  }

  out <- attribute_levels %>%
    left_join(responses, by = c("id", "task"))

}

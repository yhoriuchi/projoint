#' Reshapes survey response data for conjoint analysis
#'
#' This function takes a data frame, preferably from \dQuote{read_Qualtrics()}, and reshapes it from wide to long such that each row is a distinct conjoint task rather than a respondent.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @param .data A data frame, preferably from \dQuote{read_Qualtrics()}
#' @param .idvar A character identifying the column name containing respondent IDs
#' @param .outcomes A character vector identifying the column names that contain outcomes
#' @param .alphabet The letter indicating conjoint attributes. If using Strezhnev's package (https://github.com/astrezhnev/conjointsdt) in Qualtrics, the default is "F".
#' @return A conjoint task-level data frame (in other words, in long format) ready for conjoint analysis. See \dQuote{pj}.
#' @export
#'
reshape_conjoint <- function(.data, .idvar, .outcomes, .alphabet = "F"){

  . <- code <- x <- name <- NULL

  task <- NULL

  profile <- NULL

  attribute <- attribute_name <- NULL
  level <- level_name <- NULL
  outcomes <- outcome_qnum <- NULL
  response <- selected <-  NULL

  idvar_quo <- enquo(.idvar)
  n_tasks <- length(.outcomes)

  df <- .data %>% mutate(id = !!idvar_quo)

  # Attributes
  attributes <- df %>%
    select(id, contains(paste0(.alphabet, "-"))) %>%
    pivot_longer(names_to = "code", values_to = "name", cols = 2:ncol(.)) %>%
    filter(str_detect(code, paste0(.alphabet, "-\\d+-\\d+$"))) %>%
    separate(code, into = c("x", "task", "attribute"), sep = "\\-") %>%
    select(-x) %>%
    rename(attribute_name = name)

  # Levels
  levels <- df %>%
    select(id, contains(paste0(.alphabet, "-"))) %>%
    pivot_longer(names_to = "code", values_to = "name", cols = 2:ncol(.)) %>%
    filter(str_detect(code, paste0(.alphabet, "-\\d+-\\d+-\\d+"))) %>%
    separate(code, into = c("x", "task", "profile", "attribute"), sep = "\\-") %>%
    select(-x) %>%
    rename(level_name = name)

  # Attributes and Levels
  attribute_levels <- left_join(attributes, levels,
                                by = c("id", "task", "attribute")) %>%
    select(-attribute) %>%
    mutate_at(c("task", "profile"), .funs = as.numeric) %>%
    rename(attribute = attribute_name,
           level = level_name) %>%
    filter(!is.na(attribute) & !is.na(level)) %>%
    filter(task <= n_tasks) %>%
    group_by(id, task) %>%
    pivot_wider(names_from = "attribute", values_from = "level") %>%
    ungroup()

  # Make a small data frame to assign "Task" number
  df_outcomes <- data.frame(outcome_qnum = .outcomes,
                            task = seq(from = 1, to = length(.outcomes), by = 1))

  # Responses
  choice <- df %>%
    select(id, all_of(.outcomes)) %>%
    pivot_longer(names_to = "outcome_qnum", values_to = "response", cols = 2:ncol(.)) %>%
    left_join(df_outcomes, by = "outcome_qnum") %>%
    mutate(response = str_extract(response, "\\d") %>% as.numeric()) %>%
    select(id, task, response)

  # Attributes, Levels, and Responses
  attribute_levels %>%
    left_join(choice, by = c("id", "task")) %>%
    mutate(selected = ifelse(profile == response, 1, 0)) %>%
    select(-response) %>%
    return()

}

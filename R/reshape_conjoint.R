#' Reshapes survey response data for conjoint analysis
#'
#' This function takes a data frame, preferably from \dQuote{read_Qualtrics()}, and reshapes it from wide to long such that each row is a distinct conjoint task rather than a respondent.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import rlang
#' @param .data A data frame, preferably from \dQuote{read_Qualtrics()}
#' @param .idvar A character identifying the column name containing respondent IDs
#' @param .outcomes A character vector identifying the column names that contain outcomes
#' @param .alphabet The letter indicating conjoint attributes. If using Strezhnev's package (https://github.com/astrezhnev/conjointsdt) in Qualtrics, the default is "F".
#' @param .flipped TRUE if the profiles of the repeated task are flipped (recommended)
#' @return A conjoint task-level data frame (in other words, in long format) ready for conjoint analysis. See \dQuote{pj}.
#' @export
#' @examples
#' library(projoint)
#' 
#' data("exampleData1")
#' head(exampleData1)
#' reshaped_data = reshape_conjoint(exampleData1, .idvar = "ResponseId",
#'                                  .outcomes = c("Q4.1", "Q5.1", "Q6.1", "Q7.1", "Q8.1","Q9.1"),
#'                                  .alphabet ="A")

reshape_conjoint <- function(.data, .idvar, .outcomes, .alphabet = "F", .flipped = TRUE)
{

  . <- code <- x <- name <- attribute <- attribute_name <- NULL
  level_name <- task <- outcome_qnum <- outcomes <- NULL
  profile <- response <- selected <- selected_repeated <-  NULL

  idvar_quo <- rlang::enquo(.idvar)
  n_tasks <- length(.outcomes)

  # Remove if a conjoint table is empty -------------------------------------

  alphabet11 <- paste0(.alphabet, "-1-1") %>% rlang::sym()

  df <- .data %>%
    mutate(id = !!idvar_quo) %>%
    filter(!is.na(!!alphabet11))

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

  attribute_levels_long <- left_join(temp1, temp2,
                                     by = c("id", "task", "attribute")) %>%
    select(-attribute) %>%
    mutate_at(c("task", "profile"), .funs = as.numeric) %>%
    rename(attribute = attribute_name,
           level = level_name) %>%
    filter(task <= n_tasks)

  # Make a list of attributes and levels, as well as their IDs
  labels <- attribute_levels_long %>%
    arrange(attribute, level) %>%
    select(attribute, level) %>%
    distinct() %>%
    group_by(attribute) %>%
    mutate(attribute_id = cur_group_id(),
           level_id = row_number()) %>%
    ungroup() %>%
    mutate(attribute_id = str_c("att", attribute_id),
           level_id = str_c(attribute_id, ":level", level_id))

  attribute_levels_wide <- attribute_levels_long %>%
    left_join(labels) %>%
    select(-attribute, -level) %>%
    rename(attribute = attribute_id,
           level = level_id) %>%
    group_by(id, task) %>%
    pivot_wider(names_from = "attribute", values_from = "level") %>%
    ungroup()

  attribute_levels_repeated <- attribute_levels_wide %>%
    filter(task == 1)

  # Response variable
  responses <- df %>%
    select(id, all_of(.outcomes)) %>%
    pivot_longer(names_to = "outcome_qnum", values_to = "response", cols = 2:ncol(.)) %>%
    mutate(task = NA)

  for (i in 1:n_tasks) {
    responses <- responses %>%
      mutate(task = ifelse(outcome_qnum == .outcomes[i], i, task))
  }

  response_cleaned <- responses %>%
    mutate(selected = str_extract(response, ".$"),
           selected = case_when(selected %in% c("1", "A") ~ 1,
                                selected %in% c("2", "B") ~ 2)) %>%
    select(-response, -outcome_qnum)

  # Tasks to estimate AMCEs/MMs
  out1 <- attribute_levels_wide %>%
    left_join(response_cleaned %>%
                filter(task != n_tasks),
              by = c("id", "task")) %>%
    mutate(selected = ifelse(profile == selected, 1, 0))

  # Tasks to estimate ICR
  out2 <- attribute_levels_repeated %>%
    left_join(response_cleaned %>%
                filter(task == n_tasks) %>%
                mutate(task = 1),
              by = c("id", "task")) %>%
    mutate(selected = case_when(profile == selected & .flipped == FALSE ~ 1,
                                profile == selected & .flipped == TRUE  ~ 0,
                                profile != selected & .flipped == FALSE ~ 0,
                                profile != selected & .flipped == TRUE  ~ 1)) %>%
    rename(selected_repeated = selected)

  # Merge
  out_final <- left_join(out1, out2) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(id = as.character(id)) %>%
    as.data.frame()

  # Return
  list(labels, out_final)

}

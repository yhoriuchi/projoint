#' Reshapes survey response data for conjoint analysis
#'
#' This function takes a data frame, preferably from \dQuote{read_Qualtrics()}, and reshapes it from wide to long such that each row is a distinct conjoint task rather than a respondent.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import rlang
#' @import tidyselect
#' @param .dataframe A data frame, preferably from \dQuote{read_Qualtrics()}
#' @param .idvar A character identifying the column name containing respondent IDs
#' @param .outcomes A character vector identifying the column names that contain outcomes
#' @param .outcomes_ids A vector identifying the profiles of the outcomes -- e.g., c("A", "B")
#' @param .alphabet The letter indicating conjoint attributes. If using Strezhnev's package (https://github.com/astrezhnev/conjointsdt) in Qualtrics, the default is "F".
#' @param .repeated TRUE if there is a repeated task (recommended). The repeated task should be the same as the first task.
#' @param .flipped TRUE if the profiles of the repeated task are flipped (recommended)
#' @return A conjoint task-level data frame (in other words, in long format) ready for conjoint analysis.
#' @export
#' @examples
#' library(projoint)
#' library(stringr)
#' 
#' data("exampleData1")
#' head(exampleData1)
#'
#' outcomes <- str_c("choice", seq(from = 1, to = 8, by = 1))
#' outcomes <- c(outcomes, "choice1_repeated_flipped")
#' reshaped_data = reshaped_data <- reshape_conjoint(
#'   .data = exampleData1, 
#'   .idvar = "ResponseId", 
#'   .outcomes = outcomes,
#'   .outcomes_ids = c("1", "2"),
#'   .alphabet = "K", 
#'   .repeated = TRUE,
#'   .flipped = TRUE)

reshape_conjoint <- function(
    .dataframe, 
    .idvar, 
    .outcomes, # This should include the repeated task if .repeated == TRUE
    .outcomes_ids = c("1", "2"),
    .alphabet = "F", 
    .repeated = FALSE,
    .flipped = NULL
){
  
  # Number of tasks (including the repeated task)
  n_tasks_all <- length(.outcomes)
  
  # Number of tasks (excluding the repeated task)
  if (.repeated == FALSE){
    n_tasks <- n_tasks_all
  } else if (.repeated == TRUE){
    n_tasks <- n_tasks_all - 1
  } else{
    print("Error: .repeated must be logical.")
  }
  
  # Check the consistency between .repeated and .flipped
  if (.repeated == FALSE & !is.null(.flipped)){
    stop("Error: .flipped should be NULL if .repeated is FALSE.")
  } 
  if (.repeated == TRUE & !is.logical(.flipped)){
    stop("Error: .flipped should be logical if .repeated is TRUE.")
  }
  
  # Initial data cleaning
  df <- .dataframe %>%
    # Rename the respondent identifier "id"
    dplyr::rename("id" = all_of(.idvar)) %>%
    # Sometimes empty conjoint tables are generated due to server problems. 
    # The following line removes tasks with no information (using "-1-1") 
    # assuming that all contents are empty if "-1-1" is NA.
    dplyr::filter(!is.na(!!rlang::sym(paste0(.alphabet, "-1-1"))))
  
  # Data frame that only includes ID and conjoint-related variables
  temp0 <- df %>% 
    dplyr::select(id, tidyselect::contains(paste0(.alphabet, "-")))
  
  # Number of columns in temp0
  n_col <- ncol(temp0)
  
  # c("id", "task", "attribute", "attribute_name")
  temp1 <- temp0 %>%
    tidyr::pivot_longer(names_to = "code", values_to = "name", cols = all_of(2:n_col)) %>%
    dplyr::filter(stringr::str_detect(.data$code, paste0(.alphabet, "-\\d+-\\d+$"))) %>%
    tidyr::separate(.data$code, into = c("x", "task", "attribute"), sep = "\\-") %>%
    dplyr::select(-all_of("x")) %>%
    rlang::set_names(c("id", "task", "attribute", "attribute_name"))
  
  # c("id", "task", "profile", "attribute", "level_name")
  temp2 <- temp0 %>%
    tidyr::pivot_longer(names_to = "code", values_to = "name", cols = all_of(2:n_col)) %>%
    dplyr::filter(stringr::str_detect(.data$code, paste0(.alphabet, "-\\d+-\\d+-\\d+"))) %>%
    tidyr::separate(.data$code, into = c("x", "task", "profile", "attribute"), sep = "\\-") %>%
    dplyr::select(-all_of("x")) %>%
    rlang::set_names(c("id", "task", "profile", "attribute", "level_name"))
  
  # Merge temp1 and temp2 and do further wrangling
  attribute_levels_long <- left_join(temp1, temp2,
                                     by = c("id", "task", "attribute")) %>%
    dplyr::select(-all_of("attribute")) %>%
    dplyr::mutate_at(c("task", "profile"), .funs = as.numeric) %>%
    dplyr::rename("attribute" = .data$attribute_name,
                  "level" = .data$level_name) %>%
    dplyr::filter(.data$task <= n_tasks)
  
  # Make a list of attributes and levels, as well as their IDs
  labels <- attribute_levels_long %>%
    dplyr::arrange(.data$attribute, .data$level) %>%
    dplyr::select(.data$attribute, .data$level) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$attribute) %>%
    dplyr::mutate(attribute_id = dplyr::cur_group_id(),
                  level_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(attribute_id = stringr::str_c("att", .data$attribute_id),
                  level_id = stringr::str_c(.data$attribute_id, ":level", .data$level_id))
  
  # Make a wide-form data frae with the attribute and level IDs
  attribute_levels_wide <- attribute_levels_long %>%
    dplyr::left_join(labels, by = c("attribute", "level")) %>%
    dplyr::select(-.data$attribute, -.data$level) %>%
    dplyr::rename("attribute" = .data$attribute_id,
                  "level" = .data$level_id) %>%
    dplyr::group_by(.data$id, .data$task) %>%
    tidyr::pivot_wider(names_from = "attribute", values_from = "level") %>%
    dplyr::ungroup()
  
  # Keep the first task, which is used in the repeated task
  attribute_levels_repeated <- attribute_levels_wide %>%
    dplyr::filter(.data$task == 1)
  
  # Wrangle the response variables
  responses <- df %>%
    dplyr::select(id, all_of(.outcomes)) %>%
    tidyr::pivot_longer(names_to = "outcome_qnum", values_to = "response", cols = 2:(n_tasks_all + 1)) %>%
    dplyr::mutate(task = NA)
  
  # Assign the response numbers
  for (i in 1:n_tasks_all) {
    responses <- responses %>%
      dplyr::mutate(task = ifelse(.data$outcome_qnum == .outcomes[i], i, .data$task))
  }
  
  # Further cleaning of the response data frame
  response_cleaned <- responses %>%
    dplyr::mutate(selected = str_extract(.data$response, ".$"),
                  selected = dplyr::case_when(.data$selected == .outcomes_ids[1] ~ 1,
                                              .data$selected == .outcomes_ids[2] ~ 2)) %>%
    dplyr::select(-.data$response, -.data$outcome_qnum)
  
  # Data frame excluding the repeated task
  out1 <- attribute_levels_wide %>%
    left_join(response_cleaned %>%
                dplyr::filter(.data$task <= n_tasks),
              by = c("id", "task")) %>%
    dplyr::mutate(selected = ifelse(.data$profile == .data$selected, 1, 0))
  
  
  # Add the repeated task (if any)
  if (.repeated == TRUE){
    
    # Tasks to estimate ICR
    out2 <- attribute_levels_repeated %>%
      left_join(response_cleaned %>%
                  dplyr::filter(.data$task == n_tasks_all) %>%
                  dplyr::mutate(task = 1),
                by = c("id", "task")) %>%
      dplyr::mutate(selected = dplyr::case_when(.data$profile == .data$selected & .flipped == FALSE ~ 1,
                                                .data$profile == .data$selected & .flipped == TRUE  ~ 0,
                                                .data$profile != .data$selected & .flipped == FALSE ~ 0,
                                                .data$profile != .data$selected & .flipped == TRUE  ~ 1)) %>%
      rename("selected_repeated" = .data$selected)
    
    # Merge
    suppressMessages(
      out_final <- left_join(out1, out2)
    )
    
  } else if (.repeated == FALSE){
    
    out_final <- out1
    
  }
  
  # Final data frame
  out <- out_final %>%
    dplyr::mutate_if(is.character, as.factor) %>%
    dplyr::mutate(id = as.character(.data$id)) %>%
    as.data.frame()
  
  # Return the data frame and the variable labels as a list
  list(labels, out)
  
}

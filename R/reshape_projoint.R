#' Reshapes survey response data for conjoint analysis
#'
#' This function takes a data frame, preferably from`read_Qualtrics()`, and reshapes it from wide to long such that each row is a distinct conjoint task rather than a respondent.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import rlang
#' @import tidyselect
#' @param .dataframe A data frame, preferably from `read_Qualtrics()`
#' @param .idvar A character identifying the column name containing respondent IDs
#' @param .outcomes A character vector identifying the column names that contain outcomes. If there is a repeated task, it should be the LAST element in this vector.
#' @param .outcomes_ids A vector identifying the possibilities for the outcome variables -- e.g., `c("Candidate A", "Candidate B")`
#' @param .alphabet The letter indicating conjoint attributes. If using Strezhnev's package (https://github.com/astrezhnev/conjointsdt) in Qualtrics, the default is "F".
#' @param .repeated TRUE if there is a repeated task (recommended). The repeated task should be the same as the first task.
#' @param .flipped TRUE if the profiles of the repeated task are flipped (recommended)
#' @return A projoint object of class `projoint_data` ready to pass to `projoint()`.
#' @export
#' @examples
#' library(projoint)
#' library(stringr)
#' 
#' data("exampleData1")
#' head(exampleData1)
#'
#' # Write outcome column names
#' outcomes <- str_c("choice", seq(from = 1, to = 8, by = 1))
#' outcomes <- c(outcomes, "choice1_repeated_flipped")
#' 
#' # Reshape the data
#' reshaped_data <- reshape_projoint(
#'   .dataframe = exampleData1, 
#'   .idvar = "ResponseId", 
#'   .outcomes = outcomes,
#'   .outcomes_ids = c("A", "B"),
#'   .alphabet = "K", 
#'   .repeated = TRUE,
#'   .flipped = TRUE)

reshape_projoint <- function(
    .dataframe, 
    .idvar, 
    .outcomes, # This should include the repeated task if .repeated == TRUE
    .outcomes_ids = c("1", "2"),
    .alphabet = "F", 
    .repeated = FALSE,
    .flipped = NULL
){
  
  # bind variables locally to the function
  
  attribute_name <- NULL
  level_name <- NULL
  attribute <- NULL
  level <- NULL
  attribute_id <- NULL
  level_id <- NULL
  response <- NULL
  outcome_qnum <- NULL
  selected <- NULL
  selected_repeated <- NULL
  code <- NULL
  task <- NULL
  profile <- NULL
  
  
  # number of tasks (including the repeated task)
  n_tasks_all <- length(.outcomes)
  
  # number of tasks (excluding the repeated task)
  if (.repeated == FALSE){
    n_tasks <- n_tasks_all
  } else if (.repeated == TRUE){
    n_tasks <- n_tasks_all - 1
  } else{
    print("Error: .repeated must be logical.")
  }

  # repeated_task recommended
  if(!is.logical(.repeated)){
    stop("The .repeated_task argument must be either TRUE or FALSE.")
  }
  
  # check the consistency between .repeated and .flipped
  if (.repeated == FALSE & !is.null(.flipped)){
    stop("Error: The .flipped argument should be NULL if the .repeated argument is FALSE.")
  } 
  if (.repeated == TRUE & !is.logical(.flipped)){
    stop("Error: The .flipped argument should be logical if the .repeated argument is TRUE.")
  }

  # initial data cleaning
  df <- .dataframe %>%
    # Rename the respondent identifier "id"
    dplyr::rename("id" = all_of(.idvar)) %>%
    # Sometimes empty conjoint tables are generated due to server problems. 
    # The following line removes tasks with no information (using "-1-1") 
    # assuming that all contents are empty if "-1-1" is NA.
    dplyr::filter(!is.na(!!rlang::sym(paste0(.alphabet, "-1-1"))))
  
  # data frame that only includes ID and conjoint-related variables
  temp0 <- df %>% 
    dplyr::select(id, tidyselect::contains(paste0(.alphabet, "-")))
  
  # number of columns in temp0
  n_col <- ncol(temp0)
  
  # c("id", "task", "attribute", "attribute_name")
  temp1 <- temp0 %>%
    tidyr::pivot_longer(names_to = "code", values_to = "name", cols = all_of(2:n_col)) %>%
    dplyr::filter(stringr::str_detect(code, paste0(.alphabet, "-\\d+-\\d+$"))) %>%
    tidyr::separate(code, into = c("x", "task", "attribute"), sep = "\\-") %>%
    dplyr::select(-all_of("x")) %>%
    rlang::set_names(c("id", "task", "attribute", "attribute_name"))
  
  # c("id", "task", "profile", "attribute", "level_name")
  temp2 <- temp0 %>%
    tidyr::pivot_longer(names_to = "code", values_to = "name", cols = all_of(2:n_col)) %>%
    dplyr::filter(stringr::str_detect(code, paste0(.alphabet, "-\\d+-\\d+-\\d+"))) %>%
    tidyr::separate(code, into = c("x", "task", "profile", "attribute"), sep = "\\-") %>%
    dplyr::select(-all_of("x")) %>%
    rlang::set_names(c("id", "task", "profile", "attribute", "level_name"))
  
  # merge temp1 and temp2 and do further wrangling
  attribute_levels_long <- left_join(temp1, temp2,
                                     by = c("id", "task", "attribute"),
                                     multiple = "all") %>%
    dplyr::select(-all_of("attribute")) %>%
    dplyr::mutate_at(c("task", "profile"), .funs = as.numeric) %>%
    dplyr::rename("attribute" = attribute_name,
                  "level" = level_name) %>%
    dplyr::filter(task <= n_tasks)
  
  # make a list of attributes and levels, as well as their IDs
  labels <- attribute_levels_long %>%
    dplyr::arrange(attribute, level) %>%
    dplyr::select(attribute, level) %>%
    dplyr::distinct() %>%
    dplyr::group_by(attribute) %>%
    dplyr::mutate(attribute_id = dplyr::cur_group_id(),
                  level_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(attribute_id = stringr::str_c("att", attribute_id),
                  level_id = stringr::str_c(attribute_id, ":level", level_id))
  
  # make a wide-form data frae with the attribute and level IDs
  attribute_levels_wide <- attribute_levels_long %>%
    dplyr::left_join(labels, by = c("attribute", "level")) %>%
    dplyr::select(-attribute, -level) %>%
    dplyr::rename("attribute" = attribute_id,
                  "level" = level_id) %>%
    dplyr::group_by(id, task) %>%
    tidyr::pivot_wider(names_from = "attribute", values_from = "level") %>%
    dplyr::ungroup()
  
  # keep the first task, which is used in the repeated task
  attribute_levels_repeated <- attribute_levels_wide %>%
    dplyr::filter(task == 1)
  
  # wrangle the response variables
  responses <- df %>%
    dplyr::select(id, all_of(.outcomes)) %>%
    tidyr::pivot_longer(names_to = "outcome_qnum", values_to = "response", cols = 2:(n_tasks_all + 1)) %>%
    dplyr::mutate(task = NA)
  
  # assign the response numbers
  for (i in 1:n_tasks_all) {
    responses <- responses %>%
      dplyr::mutate(task = ifelse(outcome_qnum == .outcomes[i], i, task))
  }
  
  # further cleaning of the response data frame
  response_cleaned <- responses %>%
    dplyr::mutate(selected = str_extract(response, ".$"),
                  selected = dplyr::case_when(selected == .outcomes_ids[1] ~ 1,
                                              selected == .outcomes_ids[2] ~ 2)) %>%
    dplyr::select(-response, -outcome_qnum)
  
  # data frame excluding the repeated task
  out1 <- attribute_levels_wide %>%
    left_join(response_cleaned %>%
                dplyr::filter(task <= n_tasks),
              by = c("id", "task")) %>%
    dplyr::mutate(selected = ifelse(profile == selected, 1, 0))
  
  
  # add the repeated task (if any)
  if (.repeated == TRUE){
    
    # tasks to estimate ICR
    out2 <- attribute_levels_repeated %>%
      left_join(response_cleaned %>%
                  dplyr::filter(task == n_tasks_all) %>%
                  dplyr::mutate(task = 1),
                by = c("id", "task")) %>%
      dplyr::mutate(selected = dplyr::case_when(profile == selected & .flipped == FALSE ~ 1,
                                                profile == selected & .flipped == TRUE  ~ 0,
                                                profile != selected & .flipped == FALSE ~ 0,
                                                profile != selected & .flipped == TRUE  ~ 1)) %>%
      dplyr::rename("selected_repeated" = selected) 
    
    # merge
    suppressMessages(
      out_final <- left_join(out1, out2) %>% 
        dplyr::mutate(agree = ifelse(selected == selected_repeated, 1, 0)) 
    )
    
  } else if (.repeated == FALSE){
    
    out_final <- out1 %>% 
      dplyr::mutate(agree = NA) 
    
  }
  
  # add a variable indicating disagreement ----------------------------------
  
  # final data frame
  out <- out_final %>%
    dplyr::mutate_if(is.character, as.factor) %>%
    dplyr::mutate(id = as.character(id)) %>%
    as_tibble()
  
  # return the data frame and the variable labels as a list
  out2 <- projoint_data("labels" = labels, 
                        "data" = out)
  return(out2)
  
}
#' Make a projoint_data object using a labelled tibble (data frame)
#'
#' This function converts a labelled tibble (data frame) to a "projoint_data" class object necessary for \code{\link{projoint}}.
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import rlang
#' @import tidyselect
#' @param .dataframe A data frame. The unit of observation must be each of two profiles in each task for each respondent.
#' @param .attribute_vars A character vector identifying the names of attributes
#' @param .id_var A character identifying the name of a column containing respondent IDs (default: "id")
#' @param .task_var A character identifying the name of a column containing task numbers (default: "task")
#' @param .profile_var A character identifying the name of a column containing profile numbers IDs (default: "profile")
#' @param .selected_var A character identifying the name of a column containing each dichotomous response ({0, 1}) for each task (default: "selected")
#' @param .selected_repeated_var A character identifying the name of a column containing each dichotomous response ({0, 1}) for the repeated task (default: NULL)
#' @param .fill A logical vector: TRUE if you want to use information about whether a respondent chose the same profile for the repeated task and "fill" (using the `tidyr` package) missing values for the non-repeated tasks, FALSE (otherwise). If the number of respondents is small, if the number of specific profile pairs of your interest is small, and/or if the number of specific respondent subgroups you want to study is small, it is worth changing this option to TRUE. But please note that `.fill = TRUE` is based on an assumption that IRR is independent of information contained in conjoint tables. Although our empirical tests suggest the validity of this assumption, if you are unsure about it, it is better to use the default value (FALSE).
#' @return A projoint object of class \code{\link{projoint_data}} ready to pass to \code{\link{projoint}}.
#' @export
make_projoint_data <- function(
    .dataframe, 
    .attribute_vars,
    .id_var = "id",
    .task_var = "task",
    .profile_var = "profile",
    .selected_var = "selected", 
    .selected_repeated_var = NULL, 
    .fill = FALSE){
  
  # bind variables locally to the function
  
  . <- NULL
  attribute <- NULL
  attribute_id <- NULL
  level <- NULL
  level_id <- NULL
  selected <- NULL
  selected_repeated <- NULL
  agree <- NULL
  task <- NULL
  
  # check
  
  if (!is.data.frame(.dataframe)){
    stop("The .dataframe argument must be a data frame (or a tibble).")
  }
  if (!is.character(.attribute_vars)){
    stop("The .attribute_vars argument must be a vector of characters with the length of 1 or more.")
  } 
  if (!is.character(.id_var)){
    stop("The .id_var argument must be a character")
  } 
  if (!is.character(.task_var)){
    stop("The .task_var argument must be a character.")
  } 
  if (!is.character(.profile_var)){
    stop("The .profile_var argument must be a character.")
  } 
  if (!is.character(.selected_var)){
    stop("The .attribute_vars argument must be a character.")
  } 
  if (!is.null(.selected_repeated_var) & !is.character(.selected_repeated_var)){
    stop("The .selected_repeated_var argument must be NULL or a character.")
  } 
  
  # assign attribute_id (in an alphabetical order)
  attributes <- data.frame(attribute = .attribute_vars) %>% 
    dplyr::mutate(attribute_id = factor(attribute) %>% as.numeric(),
                  attribute_id = stringr::str_c("att", attribute_id))
  
  # rename variables
  variable_names <- data.frame("original" = c("id", "task", "profile"), 
                               "renamed" = c("id", "task", "profile")) %>%
    dplyr::bind_rows(attributes %>% 
                       rlang::set_names(c("original", "renamed"))) %>% 
    dplyr::add_row(original = "selected", 
                   renamed = "selected")
  
  # rename and reorder variables
  if (is.null(.selected_repeated_var)){
    
    data <- .dataframe %>% 
      dplyr::select("id" = .id_var,
                    "task" = .task_var, 
                    "profile" = .profile_var,
                    all_of(.attribute_vars),
                    "selected" = .selected_var) %>% 
      rlang::set_names(pull(variable_names))
    
  } else{
    
    variable_names <- variable_names %>% 
      dplyr::add_row(original = "selected_repeated", 
                     renamed = "selected_repeated")
    
    data <- .dataframe %>% 
      dplyr::select("id" = .id_var,
                    "task" = .task_var, 
                    "profile" = .profile_var,
                    all_of(.attribute_vars),
                    "selected" = .selected_var, 
                    "selected_repeated" = .selected_repeated_var) %>% 
      rlang::set_names(pull(variable_names)) %>% 
      mutate(agree = ifelse(selected == selected_repeated, 1, 0))
    
  }
  
  # make "labels" data frame
  labels <- data %>% 
    dplyr::select(contains("att")) %>%
    pivot_longer(names_to = "attribute_id", values_to = "level", cols = 1:ncol(.)) %>% 
    distinct() %>% 
    dplyr::arrange(attribute_id, level) %>% 
    dplyr::group_by(attribute_id) %>% 
    dplyr::mutate(level_id = row_number()) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(level_id = str_c(attribute_id, ":lev", level_id)) %>% 
    dplyr::left_join(attributes, by = "attribute_id") %>% 
    dplyr::arrange(attribute, level, attribute_id, level_id)
  
  for (i in 1:nrow(attributes)){
    
    .attribute_id <- attributes$attribute_id[i]
    var_quo <- rlang::sym(.attribute_id)
    
    temp <- labels %>% 
      dplyr::filter(attribute_id == .attribute_id) %>% 
      dplyr::select(level, level_id) %>% 
      rlang::set_names(c(.attribute_id, "level_id"))
    
    data <- data %>% 
      
      dplyr::left_join(temp) %>% 
      dplyr::select(-all_of(.attribute_id)) %>% 
      dplyr::rename({{var_quo}} := level_id)
    
    
  }
  
  if (.fill == TRUE){
    
    data_final <- data %>% 
      dplyr::arrange(id, task, agree) %>% 
      tidyr::fill(agree)
    
  } else{
    
    data_final <- data %>% 
      dplyr::arrange(id, task, agree)
  }
  
  
  # a class generator  
  out <- projoint_data("labels" = labels,
                       "data" = data_final)
  
  # return
  return(out)
  
}
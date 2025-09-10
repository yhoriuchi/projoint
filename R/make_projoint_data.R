#' Make a \code{projoint_data} object from a labelled tibble
#'
#' Converts a labelled tibble/data frame (one column per attribute) into an object
#' of class \code{projoint_data} that downstream functions (e.g., \code{\link{projoint}})
#' can consume. The unit of observation should be each of two profiles in each task
#' for each respondent.
#'
#' @param .dataframe A data frame (or tibble). One row per profile per task per respondent.
#' @param .attribute_vars Character vector of attribute column names.
#' @param .id_var Column name (character) with respondent IDs. Default \code{"id"}.
#' @param .task_var Column name (character) with task numbers. Default \code{"task"}.
#' @param .profile_var Column name (character) with profile numbers. Default \code{"profile"}.
#' @param .selected_var Column name (character) with the binary choice for each task
#'   (values in \code{\{0,1\}}). Default \code{"selected"}.
#' @param .selected_repeated_var Optional column name (character) with the binary choice
#'   for the repeated task. Default \code{NULL}.
#' @param .fill Logical. If \code{TRUE}, uses repeated-task agreement to \emph{fill}
#'   missing agreement values for non-repeated tasks (assumes IRR is independent of
#'   table content). If unsure, prefer the default \code{FALSE}.
#'
#' @return A \code{projoint_data} object (a list-like object containing a \code{labels}
#'   tibble and a \code{data} tibble) ready to pass to \code{\link{projoint}} and related
#'   functions.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Example: build a projoint_data object from the labelled-tibble example
#' data(exampleData1_labelled_tibble)
#'
#' att_cols <- c("School Quality", "Violent Crime Rate (Vs National Rate)",
#'               "Racial Composition", "Housing Cost",
#'               "Presidential Vote (2020)", 
#'               "Total Daily Driving Time for Commuting and Errands",
#'               "Type of Place")
#'
#' pj_dat <- make_projoint_data(
#'   .dataframe             = exampleData1_labelled_tibble,
#'   .attribute_vars        = att_cols,
#'   .id_var                = "id",
#'   .task_var              = "task",
#'   .profile_var           = "profile",
#'   .selected_var          = "selected",
#'   .selected_repeated_var = "selected_repeated",
#'   .fill                  = FALSE
#' )
#'
#' class(pj_dat)
#' # [1] "projoint_data"
#' }
make_projoint_data <- function(
    .dataframe, 
    .attribute_vars,
    .id_var = "id",
    .task_var = "task",
    .profile_var = "profile",
    .selected_var = "selected", 
    .selected_repeated_var = NULL, 
    .fill = FALSE){
  
  # check
  
  if (!is.data.frame(.dataframe)){
    stop("The .dataframe argument must be a data frame (or a tibble).")
  }
  if (!is.character(.attribute_vars)){
    stop("The .attribute_vars argument must be a vector of characters with the length of 1 or more.")
  } 
  if (length(.attribute_vars) == 0L) {
    stop("`.attribute_vars` is empty. Supply the names of your attribute columns.")
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
  attributes <- data.frame(attribute = .attribute_vars) |> 
    dplyr::mutate(attribute_id = factor(attribute) |> as.numeric(),
                  attribute_id = stringr::str_c("att", attribute_id))
  
  # rename variables
  variable_names <- data.frame("original" = c("id", "task", "profile"), 
                               "renamed" = c("id", "task", "profile")) |>
    dplyr::bind_rows(attributes |> 
                       rlang::set_names(c("original", "renamed"))) |> 
    dplyr::add_row(original = "selected", 
                   renamed = "selected")
  
  # rename and reorder variables
  if (is.null(.selected_repeated_var)){
    
    data <- .dataframe |> 
      dplyr::select("id" = .id_var,
                    "task" = .task_var, 
                    "profile" = .profile_var,
                    all_of(.attribute_vars),
                    "selected" = .selected_var) |> 
      rlang::set_names(pull(variable_names)) |> 
      mutate(selected_repeated = NA,
             agree = NA)
    
  } else{
    
    variable_names <- variable_names |> 
      dplyr::add_row(original = "selected_repeated", 
                     renamed = "selected_repeated")
    
    data <- .dataframe |> 
      dplyr::select("id" = .id_var,
                    "task" = .task_var, 
                    "profile" = .profile_var,
                    all_of(.attribute_vars),
                    "selected" = .selected_var, 
                    "selected_repeated" = .selected_repeated_var) |> 
      rlang::set_names(pull(variable_names)) |> 
      mutate(agree = ifelse(selected == selected_repeated, 1, 0))
    
  }
  
  # make "labels" data frame
  labels <- data |> 
    dplyr::select(dplyr::all_of(attributes$attribute_id)) |>
    pivot_longer(names_to = "attribute_id", values_to = "level", cols = dplyr::everything()) |> 
    distinct() |> 
    dplyr::arrange(attribute_id, level) |> 
    dplyr::group_by(attribute_id) |> 
    dplyr::mutate(level_id = dplyr::row_number()) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(level_id = stringr::str_c(attribute_id, ":lev", level_id)) |> 
    dplyr::left_join(attributes, by = "attribute_id") |> 
    dplyr::arrange(attribute, level, attribute_id, level_id)
  
  for (i in 1:nrow(attributes)){
    
    .attribute_id <- attributes$attribute_id[i]
    var_quo <- rlang::sym(.attribute_id)
    
    temp <- labels |> 
      dplyr::filter(attribute_id == .attribute_id) |> 
      dplyr::select(level, level_id) |> 
      rlang::set_names(c(.attribute_id, "level_id"))
    
    suppressMessages(
      data <- data |> 
        
        dplyr::left_join(temp) |> 
        dplyr::select(-all_of(.attribute_id)) |> 
        dplyr::rename({{var_quo}} := level_id)
    )
    
    
  }
  
  if (.fill == TRUE){
    
    data_final <- data |> 
      dplyr::arrange(id, task, agree) |> 
      tidyr::fill(agree)
    
  } else{
    
    data_final <- data |> 
      dplyr::arrange(id, task, agree)
  }
  
  
  # a class generator  
  out <- projoint_data("labels" = labels,
                       "data" = data_final)
  
  # return
  return(out)
  
}
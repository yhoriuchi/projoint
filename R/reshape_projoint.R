#' Reshape survey response data for conjoint analysis (single task set)
#'
#' Takes a wide survey data frame (e.g., from \code{\link{read_Qualtrics}}) and reshapes
#' it so that each row corresponds to a single respondent–task–profile. Supports arbitrary
#' ordering of base tasks and a single repeated task per respondent. The repeated base task
#' is inferred from the first base outcome in \code{.outcomes}, and the repeated outcome
#' must be the last element of \code{.outcomes}.
#'
#' @details
#' \strong{Scope and assumptions}
#' \itemize{
#'   \item One set of conjoint tasks with exactly two profiles per task (profiles 1 and 2).
#'   \item For multi-set designs, call \code{reshape_projoint()} once per set and bind the results.
#' }
#'
#' \strong{Expected input (Qualtrics K-codes)}
#' \itemize{
#'   \item Wide columns named \code{K-<task>-<attribute>} (attribute names) and
#'         \code{K-<task>-<profile>-<attribute>} (level names), with \code{<task>} in \code{1..n}
#'         and \code{<profile>} in \code{1,2}.
#'   \item Rows with missing \code{K-1-1} are dropped as empty tables (server hiccup safeguard).
#' }
#'
#' \strong{Outcome columns (\code{.outcomes})}
#' \itemize{
#'   \item List all choice variables in the \emph{order asked}. If you include a repeated task,
#'         its outcome must be the \emph{last element}.
#'   \item For base tasks (all but the last element), the function extracts the base task id by
#'         reading the \emph{digits} in each outcome name (e.g., \code{"choice4"}, \code{"Q4"},
#'         \code{"task04"} -> task 4).
#'   \item The set of base task ids extracted from \code{.outcomes} must exactly match the set of
#'         task ids present in the K-codes; otherwise an error is thrown.
#'   \item The repeated base task is inferred as the digits in the \emph{first base outcome}
#'         (i.e., the first element of \code{.outcomes}, excluding the final repeated outcome).
#' }
#'
#' \strong{Choice parsing}
#' \itemize{
#'   \item The selected profile is parsed from the \emph{last character} of each outcome string
#'         and matched to \code{.choice_labels}. Ensure outcomes end with these labels (e.g.,
#'         \code{"Candidate A"}/\code{"Candidate B"}). If outcomes are numeric or differently
#'         formatted, pre-process or adjust \code{.choice_labels} accordingly.
#' }
#'
#' \strong{Output}
#' \itemize{
#'   \item A \code{projoint_data} object with:
#'     \itemize{
#'       \item \code{$labels}: map from human-readable \code{attribute}/\code{level} to stable ids
#'             (\code{attribute_id = "att1","att2",...}, \code{level_id = "attX:levelY"}).
#'       \item \code{$data}: tibble with one row per \code{id}–\code{task}–\code{profile}, attribute
#'             columns (named \code{att*}) storing \code{level_id}, \code{selected} (1 if that profile
#'             was chosen; 0 otherwise), \code{agree} (1/0/NA for repeated-task agreement after flip logic),
#'             and any \code{.covariates}. \code{id} is coerced to character; attribute columns are factors.
#'     }
#' }
#'
#' \strong{Filling agreement}
#' \itemize{
#'   \item If \code{.fill = TRUE}, \code{agree} is filled within respondent across tasks in task order,
#'         propagating the observed repeated-task agreement to all tasks for that respondent. This assumes
#'         IRR is respondent-specific and independent of table content.
#' }
#'
#' \strong{Diagnostics}
#' \itemize{
#'   \item \code{dplyr::count(reshaped$data, task, profile)} should show exactly two rows per task.
#'   \item If \code{pj_estimate()} later reports “No rows match the specified attribute/level”, construct QoIs
#'         from \code{reshaped$labels} (use the exact \code{attX:levelY} ids).
#' }
#'
#' @param .dataframe A data frame, preferably from \code{\link{read_Qualtrics}}.
#' @param .outcomes Character vector of outcome column names in the \emph{asked order}.
#'   If a repeated task is used, its outcome must be the \emph{last element}.
#' @param .choice_labels Character vector (default \code{c("A","B")}) giving the two labels that
#'   appear at the end of the outcome strings.
#' @param .alphabet Single character (default \code{"K"}) indicating the Qualtrics prefix.
#' @param .idvar Character (default \code{"ResponseId"}) indicating the respondent id column.
#' @param .repeated Logical (default \code{TRUE}) indicating whether a repeated task is present.
#' @param .flipped Logical (default \code{TRUE}) indicating whether the repeated task flips profiles
#'   before agreement is computed.
#' @param .covariates Optional character vector of respondent-level covariate column names to carry through.
#' @param .fill Logical (default \code{FALSE}). If \code{TRUE}, fills \code{agree} within respondent
#'   across tasks as described under “Filling agreement”.
#'
#' @return A \code{projoint_data} object with elements \code{$labels} and \code{$data}; see Details.
#'
#' @seealso \code{\link{make_projoint_data}}, \code{\link{projoint}}
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import rlang
#' @import tidyselect
#' @importFrom readr parse_number
#'
#' @examples
#' \donttest{
#' # Base tasks asked in numeric order; repeated task corresponds to task 1
#' data(exampleData1)
#' outcomes <- c(paste0("choice", 1:8), "choice1_repeated_flipped")
#' reshaped <- reshape_projoint(exampleData1, outcomes)
#' dplyr::count(reshaped$data, task, profile)  # should be 2 per task
#' }
#'
#' @export
reshape_projoint <- function(
    .dataframe,
    .outcomes,
    .choice_labels = c("A", "B"),
    .alphabet = "K",
    .idvar = "ResponseId",
    .repeated = TRUE,
    .flipped = TRUE,
    .covariates = NULL,
    .fill = FALSE
){
  
  # number of tasks (including the repeated task)
  n_tasks_all <- length(.outcomes)
  
  # number of tasks (excluding the repeated task)
  if (.repeated == TRUE){
    n_tasks <- n_tasks_all - 1
  } else {
    n_tasks <- n_tasks_all
  }
  
  # repeated_task recommended
  if(!is.logical(.repeated)){
    stop("The .repeated argument must be either TRUE or FALSE.")
  }
  
  # check the .fill argument
  if(!is.logical(.fill)){
    stop("The .fill argument must be either TRUE or FALSE.")
  }
  
  # initial data cleaning
  df <- .dataframe |>
    # Rename the respondent identifier "id"
    dplyr::rename("id" = all_of(.idvar)) |>
    # Sometimes empty conjoint tables are generated due to server problems.
    # The following line removes tasks with no information (using "-1-1")
    # assuming that all contents are empty if "-1-1" is NA.
    dplyr::filter(!is.na(!!rlang::sym(paste0(.alphabet, "-1-1"))))
  
  # data frame that only includes ID and conjoint-related variables
  temp0 <- df |>
    dplyr::select(id, tidyselect::contains(paste0(.alphabet, "-")))
  
  # number of columns in temp0
  n_col <- ncol(temp0)
  
  # c("id", "task", "attribute", "attribute_name")
  # "task" is extracted from the "code."
  temp1 <- temp0 |>
    tidyr::pivot_longer(names_to = "code", values_to = "name", cols = all_of(2:n_col)) |>
    dplyr::filter(stringr::str_detect(code, paste0(.alphabet, "-\\d+-\\d+$"))) |>
    tidyr::separate(code, into = c("x", "task", "attribute"), sep = "\\-") |>
    dplyr::select(-all_of("x")) |>
    rlang::set_names(c("id", "task", "attribute", "attribute_name"))
  
  # c("id", "task", "profile", "attribute", "level_name")
  # "task" is extracted from the "code."
  temp2 <- temp0 |>
    tidyr::pivot_longer(names_to = "code", values_to = "name", cols = all_of(2:n_col)) |>
    dplyr::filter(stringr::str_detect(code, paste0(.alphabet, "-\\d+-\\d+-\\d+"))) |>
    tidyr::separate(code, into = c("x", "task", "profile", "attribute"), sep = "\\-") |>
    dplyr::select(-all_of("x")) |>
    rlang::set_names(c("id", "task", "profile", "attribute", "level_name"))
  
  # merge temp1 and temp2 and do further wrangling
  attribute_levels_long <- left_join(temp1, temp2,
                                     by = c("id", "task", "attribute"),
                                     multiple = "all") |>
    dplyr::select(-all_of("attribute")) |>
    dplyr::mutate(across(c(task, profile), as.numeric)) |>
    dplyr::rename("attribute" = attribute_name,
                  "level" = level_name) |>
    dplyr::filter(task <= n_tasks)
  
  # make a list of attributes and levels, as well as their IDs
  labels <- attribute_levels_long |>
    dplyr::arrange(attribute, level) |>
    dplyr::select(attribute, level) |>
    dplyr::distinct() |>
    dplyr::group_by(attribute) |>
    dplyr::mutate(attribute_id = dplyr::cur_group_id(),
                  level_id = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(attribute_id = stringr::str_c("att", attribute_id),
                  level_id = stringr::str_c(attribute_id, ":level", level_id))
  
  # make a wide-form data frame with the attribute and level IDs
  attribute_levels_wide <- attribute_levels_long |>
    dplyr::left_join(labels, by = c("attribute", "level")) |>
    dplyr::select(-attribute, -level) |>
    dplyr::rename("attribute" = attribute_id,
                  "level" = level_id) |>
    dplyr::group_by(id, task, profile) |>
    tidyr::pivot_wider(names_from = "attribute", values_from = "level") |>
    dplyr::ungroup()
  
  # Ensure exactly two profiles per base task exist in the design
  chk <- attribute_levels_wide |>
    dplyr::group_by(task) |>
    dplyr::summarise(has_both = setequal(sort(unique(profile)), c(1, 2)), .groups = "drop")
  
  if (!all(chk$has_both)) {
    bad_tasks <- paste(chk$task[!chk$has_both], collapse = ", ")
    stop(sprintf("Expected profiles 1 and 2 for every task in K-codes; missing for task(s): %s.", bad_tasks))
  }
  
  
  ###########################################################################
  
  # The outcome names (excluding the repeated task) must include task IDs.
  # Example: if tasks presented are {2, 1, 3, 4, 5}, then outcomes might be
  # "Q2","Q1","Q3","Q4","Q5","Q2_repeated". The repeated outcome must be last.
  
  taskIDs_in_outcomes <- readr::parse_number(.outcomes)
  
  if (.repeated == TRUE){
    taskIDs_in_outcomes <- taskIDs_in_outcomes[1:(length(taskIDs_in_outcomes)-1)]
  }
  
  # check all numbers in task_order_in_outcomes exist in a sequence of number (tasks)
  # extracted directly from the code.
  
  all_tasks <- unique(attribute_levels_long$task)
  
  if (!isTRUE(identical(sort(taskIDs_in_outcomes), 
                        sort(all_tasks)))){
    stop("The outcome names (excluding the repeated question) should contain the corresponding task IDs.")
  }
  
  repeated_task <- taskIDs_in_outcomes[1]
  
  # keep the repeated task ID
  attribute_levels_repeated <- attribute_levels_wide |>
    dplyr::filter(task == repeated_task)
  
  # wrangle the response variables
  responses <- df |>
    dplyr::select(id, all_of(.outcomes)) |>
    tidyr::pivot_longer(names_to = "outcome_qnum", 
                        values_to = "response", 
                        cols = 2:(n_tasks_all + 1)) |>
    dplyr::mutate(task = readr::parse_number(outcome_qnum)) |>
    
    # tentatively add the order of responses
    dplyr::group_by(id) |>
    dplyr::mutate(response_order = row_number(),
                  task = ifelse(response_order == max(response_order), repeated_task, task)) |>
    dplyr::ungroup()
  
  # Ensure each respondent has the expected number of outcomes
  if (.repeated) {
    bad <- responses |> dplyr::count(id) |> dplyr::filter(n != n_tasks_all)
    if (nrow(bad) > 0) stop("Some respondents are missing the repeated outcome.")
  }
  
  ###########################################################################
  
  # # assign the response numbers
  # for (i in 1:n_tasks_all) {
  #   responses <- responses |>
  #     dplyr::mutate(task = ifelse(outcome_qnum == .outcomes[i], i, task))
  # }
  
  # further cleaning of the response data frame
  response_cleaned <- responses |>
    dplyr::mutate(selected = str_extract(response, ".$"),
                  selected = dplyr::case_when(selected == .choice_labels[1] ~ 1,
                                              selected == .choice_labels[2] ~ 2)) |>
    dplyr::select(-response, -outcome_qnum)
  
  # data frame excluding the repeated task
  out1 <- attribute_levels_wide |>
    left_join(response_cleaned |>
                dplyr::filter(response_order <= n_tasks),
              by = c("id", "task")) |>
    dplyr::select(-response_order) |>
    dplyr::mutate(selected = ifelse(profile == selected, 1, 0))
  
  
  # add the repeated task (if any)
  if (.repeated == TRUE){
    
    # tasks to estimate IRR
    out2 <- attribute_levels_repeated |>
      left_join(response_cleaned |>
                  dplyr::filter(response_order > n_tasks),
                by = c("id", "task")) |>
      dplyr::select(-response_order) |>
      dplyr::mutate(selected = dplyr::case_when(profile == selected & .flipped == FALSE ~ 1,
                                                profile == selected & .flipped == TRUE  ~ 0,
                                                profile != selected & .flipped == FALSE ~ 0,
                                                profile != selected & .flipped == TRUE  ~ 1)) |>
      dplyr::rename("selected_repeated" = selected)
    
    # merge
    suppressMessages(
      out <- dplyr::left_join(out1, out2) |>
        dplyr::mutate(agree = ifelse(selected == selected_repeated, 1, 0))
    )
    
  } else if (.repeated == FALSE){
    
    out <- out1 |>
      dplyr::mutate(agree = NA)
    
  }
  
  # Make a data frame that includes respondent covariates -------------------
  
  covariates <- .dataframe |>
    # Rename the respondent identifier "id"
    dplyr::rename("id" = all_of(.idvar)) |>
    # Select variables needed
    dplyr::select(all_of(c("id", .covariates)))
  
  
  # Make a final data frame -------------------------------------------------
  
  out_final_before_fill <- out |>
    dplyr::left_join(covariates, by = "id") |>
    dplyr::mutate(across(where(is.character), as.factor)) |>
    dplyr::mutate(id = as.character(id)) |>
    dplyr::as_tibble()
  
  if (.fill == TRUE){
    
    out_final <- out_final_before_fill |>
      dplyr::arrange(id, task) |>
      dplyr::group_by(id) |>
      tidyr::fill(agree, .direction = "downup") |> 
      dplyr::ungroup()
    
  } else{
    
    out_final <- out_final_before_fill |>
      dplyr::arrange(id, task, agree)
  }
  
  
  # return the data frame and the variable labels as a list
  return(projoint_data("labels" = labels,
                       "data" = out_final))
  
}
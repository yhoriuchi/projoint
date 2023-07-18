#' Estimate tau when there is no repeated task.
#'
#' This function ...
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import rlang
#' @import tidyselect
#' @import estimatr
#' @import ggplot2
#' @import ggthemes
#' @importFrom stats setNames
#' @importFrom methods new
#' @param .data A `projoint_data` object
#' @param .idvar A character identifying the column name containing respondent IDs
#' @param .title The title of a figure
#' @return A list (a vector of predicted value; ggplot object)
#' @export
#' @examples
#' library(projoint)
#' library(stringr)
#' 
#' ## Example 1: repeated, flipped task
#' data("exampleData1")
#' head(exampleData1)
#'
#' outcomes <- str_c("choice", seq(from = 1, to = 8, by = 1))
#' outcomes <- c(outcomes, "choice1_repeated_flipped")
#' reshaped_data = reshaped_data <- reshape_projoint(
#'   .dataframe = exampleData1, 
#'   .idvar = "ResponseId", 
#'   .outcomes = outcomes,
#'   .outcomes_ids = c("A", "B"),
#'   .alphabet = "K", 
#'   .repeated = TRUE,
#'   .flipped = TRUE)
#'   
#' tau1 <- predict_tau(reshaped_data)
#' tau1
#' 
#' ## Example 2: repeated, unflipped task
#' data("exampleData2")
#' head(exampleData2)
#'
#' outcomes <- str_c("choice", seq(from = 1, to = 8, by = 1))
#' outcomes <- c(outcomes, "choice1_repeated_notflipped")
#' reshaped_data = reshaped_data <- reshape_projoint(
#'   .dataframe = exampleData2, 
#'   .idvar = "ResponseId", 
#'   .outcomes = outcomes,
#'   .outcomes_ids = c("A", "B"),
#'   .alphabet = "K", 
#'   .repeated = TRUE,
#'   .flipped = FALSE)
#'   
#' tau2 <- predict_tau(reshaped_data)
#' tau2
#' 
#' ## Example 3: no repeated task
#' data("exampleData3")
#' head(exampleData3)
#'
#' outcomes <- str_c("choice", seq(from = 1, to = 8, by = 1))
#' outcomes <- c(outcomes)
#' reshaped_data = reshaped_data <- reshape_projoint(
#'   .dataframe = exampleData3, 
#'   .idvar = "ResponseId", 
#'   .outcomes = outcomes,
#'   .outcomes_ids = c("A", "B"),
#'   .alphabet = "K", 
#'   .repeated = FALSE)
#'   
#' tau3 <- predict_tau(reshaped_data)
#' tau3

predict_tau <- function(
    .data,
    .idvar = "id",
    .title = NULL
){

  # data frame
  
  .dataframe <- .data@data
  
  # bind variables locally to the function

  id <- NULL
  task1 <- NULL
  task2 <- NULL
  selected <- NULL
  profile <- NULL
  task <- NULL
  y <- NULL
  att <- NULL
  task1_y <- NULL
  task2_y <- NULL
  t1 <- NULL
  t2 <- NULL
  y_same <- NULL
  x <- NULL
  estimate <- NULL
  std.error <- NULL
  `1` <- NULL
  `2` <- NULL
  att_comb <- NULL
  value <- NULL
  predict <- NULL
  aes <- NULL
  conf.low <- NULL
  conf.high <- NULL
  . <- NULL
  
  # the number of attributes
  
  n_attributes <- names(.dataframe) %>% str_detect(., "att") %>% sum()
  
  # all combinations of tasks (within respondents)
  # There are MANY combinations of tasks for each respondent.
  
  task_combinations <- .dataframe %>% 
    dplyr::select(all_of(.idvar), "task1" = task, "task2" = task) %>% 
    dplyr::distinct() %>% 
    dplyr::group_by(id) %>% 
    tidyr::complete(task1, task2) %>% 
    dplyr::ungroup() %>% 
    # remove the same tasks
    dplyr::filter(task1 != task2) %>% 
    # make unique pairs of tasks
    dplyr::mutate(t1 = ifelse(task1 < task2, task2, task1),
                  t2 = ifelse(task1 > task2, task2, task1)) %>% 
    dplyr::select(-task1, -task2) %>% 
    dplyr::distinct()
  
  # initial cleaning: add y (a profile chosen)
  
  d <- .dataframe %>% 
    dplyr::mutate(y = dplyr::case_when(selected == 1 & profile == 1 ~ 1,
                                       selected == 1 & profile == 2 ~ 2,
                                       selected == 0 & profile == 1 ~ 2,
                                       selected == 0 & profile == 2 ~ 1)) %>% 
    dplyr::select(-selected)
  
  # cleaning for each attribute
  
  d2 <- d %>% 
    dplyr::select(id, task, y) %>% 
    dplyr::distinct()
  
  for (i in 1:n_attributes){
    
    att_quo <- rlang::as_name(str_c("att", i))
    var1 <- str_c("att", i, "_comb") %>% rlang::quo_name()
    var2 <- str_c("att", i, "_same") %>% rlang::quo_name()
    
    temp <- d %>% 
      dplyr::select(id, task, profile, "att" = !!att_quo) %>% 
      dplyr::group_by(id, task) %>% 
      tidyr::pivot_wider(names_from = profile, values_from = att) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(att_comb = stringr::str_c(`1`, ", ", `2`)) %>% 
      dplyr::select(id, task,
                    !!var1 := att_comb) 
    
    d2 <- d2 %>% 
      left_join(temp, by = c("id", "task"))
    
  }
  
  # rename attributes for each task
  
  task1 <- d2 %>% setNames(str_c("task1_", names(d2)))
  task2 <- d2 %>% setNames(str_c("task2_", names(d2)))
  
  # all combinations of tasks within respondents after cleaning the variable names
  
  d3 <- task_combinations %>% 
    dplyr::left_join(task1, by = c("id" = "task1_id", "t1" = "task1_task")) %>% 
    dplyr::left_join(task2, by = c("id" = "task2_id", "t2" = "task2_task")) %>% 
    dplyr::mutate(y_same = ifelse(task1_y == task2_y, 1, 0))
  
  for (i in 1:n_attributes){
    
    var1 <- sym(str_c("task1_att", i, "_comb"))
    var2 <- sym(str_c("task2_att", i, "_comb"))
    var3 <- str_c("att", i, "_comb_same") %>% rlang::quo_name()
    
    d3 <- d3 %>% 
      mutate(!!var3 := ifelse(!!var1 == !!var2, 0, 1)) # Logical: whether two level-combinations are the same between tasks
    
  }
  
  # dependent variable: the same profile was selected for the same set of tasks
  
  y <- d3 %>% 
    dplyr::select(id, t1, t2, y_same) %>% 
    dplyr::distinct()
  
  # independent variables: the number of same attributes within task combinations
  
  x <- d3 %>% 
    dplyr::select(id, t1, t2, tidyselect::contains("comb_same")) %>% 
    tidyr::pivot_longer(cols = 4:ncol(.)) %>% 
    dplyr::group_by(id, t1, t2) %>% 
    dplyr::summarise(x = sum(value), .groups = "drop")
  
  # merge y and x and run regression to estimate tau for each x (the number of same attributes)
  
  out1 <- y %>% 
    dplyr::left_join(x, by = c("id", "t1", "t2")) %>% 
    dplyr::group_by(x) %>% 
    dplyr::reframe(estimatr::tidy(estimatr::lm_robust(y_same ~ 1, 
                                                      data = dplyr::pick(everything()), 
                                                      clusters = id))) %>% 
    
    # remove if estimated tau is 0 or 1
    dplyr::filter(!(estimate %in% c(0, 1))) %>% 
    
    # remove if all attributes are the same within task combinations
    dplyr::filter(x != 0) 
  
  # then run a weighted least square regression
  
  out_reg <-  estimatr::lm_robust(estimate ~ x, data = out1, weights = 1/std.error^2, se_type = "classical") 
  
  # data frame to add predicted values
  
  newdata <- data.frame(x = 0:n_attributes)
  
  # a vector of predicted values
  predicted <- newdata %>% 
    mutate(predicted = predict(out_reg, newdata)) %>% 
    as_tibble()
  
  # prediction of x == 0
  
  prediction <- predicted %>% 
    dplyr::filter(x == 0) %>% 
    dplyr::pull(predicted) 
  
  g <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
    ggplot2::geom_pointrange(data = out1,
                             ggplot2::aes(x = x,
                                          y = estimate,
                                          ymin = conf.low,
                                          ymax = conf.high)) +
    ggplot2::geom_line(data = predicted, 
                       ggplot2::aes(x = x, 
                                    y = predicted),
                       linetype = "dotted",
                       color = "red") +
    ggplot2::geom_point(data = predicted, 
                        ggplot2::aes(x = x, 
                                     y = predicted),
                        size = 4,
                        shape = 5,
                        color = "red") +
    ggplot2::coord_cartesian(ylim = c(0.40, 1.0), 
                             xlim = c(0, n_attributes)) +
    ggplot2::scale_x_continuous(breaks = seq(0, n_attributes, 1)) +
    ggthemes::theme_few() +
    ggplot2::labs(y = "Percent agreement", 
                  x = "Number of attributes with levels that differ between two tasks", 
                  title = stringr::str_c(.title, "Prediction = ", format(round(prediction, digits = 2), nsmall = 2)))
  
  
  # return
  out = projoint_irr(irr = predicted, figure = g)
  return(out)
  
}

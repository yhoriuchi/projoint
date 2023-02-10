#' Predicts \texttt{tau} from a conjoint data set by extrapolation from similar but non-identical tasks.
#'
#' @param .data A conjoint data set.
#' @return An estimated \texttt{tau} with confidence interval.
#' @export
#' @examples
#' library(projoint)
#' 
#' 
#' data("exampleData2")
#' head(exampleData2)
#' reshaped_data = reshape_conjoint(exampleData1, .idvar = "ResponseId",
#'                                  .outcomes = c("Q4.1", "Q5.1", "Q6.1", "Q7.1", "Q8.1","Q9.1"),
#'                                  .alphabet ="A")
#' tau_est = predict_tau(reshaped_data)



predict_tau <- function(.data){
  
  id <- NULL
  task <- profile <- NULL
  selected <- selecteNULL
  
  # The number of attributes
  
  n_attributes <- names(.data) %>% str_detect(., "att") %>% sum()
  
  # All combinations of tasks (within respondents) 
  
  task_combinations <- .data %>% 
    select(id, "task1" = task, "task2" = task) %>% 
    distinct() %>% 
    group_by(id) %>% 
    complete(task1, task2) %>% 
    ungroup() %>% 
    filter(task1 != task2) %>% 
    mutate(t1 = ifelse(task1 < task2, task2, task1),
           t2 = ifelse(task1 > task2, task2, task1)) %>% 
    select(-task1, -task2) %>% 
    distinct()
  
  # Initial cleaning: add y (a profile chosen)
  
  d <- .data %>% 
    mutate(y = case_when(selected == 1 & profile == 1 ~ 1,
                         selected == 1 & profile == 2 ~ 2,
                         selected == 0 & profile == 1 ~ 2,
                         selected == 0 & profile == 2 ~ 1)) 
  
  # Cleaning for each attribute
  
  d2 <- d %>% 
    select(id, task, y) %>% 
    distinct()
  
  for (i in 1:n_attributes){
    
    att_quo <- rlang::as_name(str_c("att", i))
    var1 <- str_c("att", i, "_comb") %>% rlang::quo_name()
    var2 <- str_c("att", i, "_same") %>% rlang::quo_name()
    
    temp <- d %>% 
      select(id, task, profile, "att" = !!att_quo) %>% 
      group_by(id, task) %>% 
      pivot_wider(names_from = profile, values_from = att) %>% 
      ungroup() %>% 
      mutate(att_comb = str_c(`1`, ", ", `2`)) %>% 
      select(id, task,
             !!var1 := att_comb) 
    
    d2 <- d2 %>% 
      left_join(temp, by = c("id", "task"))
    
  }
  
  task1 <- d2 %>% set_names(str_c("task1_", names(d2)))
  task2 <- d2 %>% set_names(str_c("task2_", names(d2)))
  
  d3 <- task_combinations %>% 
    left_join(task1, by = c("id" = "task1_id", "t1" = "task1_task")) %>% 
    left_join(task2, by = c("id" = "task2_id", "t2" = "task2_task")) %>% 
    mutate(y_same = ifelse(task1_y == task2_y, 1, 0))
  
  for (i in 1:n_attributes){
    
    var1 <- sym(str_c("task1_att", i, "_comb"))
    var2 <- sym(str_c("task2_att", i, "_comb"))
    var3 <- str_c("att", i, "_comb_same") %>% rlang::quo_name()
    
    d3 <- d3 %>% 
      # Logical: whether two level-combinations are the same between tasks
      mutate(!!var3 := ifelse(!!var1 == !!var2, 0, 1)) 
    
  }
  
  y <- d3 %>% 
    select(id, t1, t2, y_same) %>% 
    distinct()
  
  x <- d3 %>% 
    select(id, t1, t2, contains("comb_same")) %>% 
    pivot_longer(cols = 4:ncol(.)) %>% 
    group_by(id, t1, t2) %>% 
    summarise(x = sum(value), .groups = "drop")
  
  out <- y %>% 
    left_join(x, by = c("id", "t1", "t2"))
  
  out %>% 
    group_by(x) %>% 
    summarise(tidy(lm_robust(y_same ~ 1, data = cur_data(), clusters = id)), .groups = "drop") %>% 
    mutate(n_attributes = n_attributes)
  
}
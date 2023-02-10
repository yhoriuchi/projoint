#' Calculates the intra-respondent reliability (IRR), average marginal component effect (AMCE), marginal mean (MM), and correcte AMCEs and MMs for a given conjoint data set specified across a given subgroup.
#'
#' @param .data A conjoint data set.
#' @param .diff A dichotomous variable to calculate difference IRR, AMCEs, and MMs across.
#' @param .tau An optional numeric argument allowing the researcher to specify their own value of \texttt{tau}
#' @return A summarized data set describing the results of a conjoint analysis subgroup comparison, corrected for IRR measurement error.
#' @export
#' @examples
#' library(projoint)
#' 
#' data("exampleData1")
#' head(exampleData1)
#' reshaped_data = reshape_conjoint(exampleData1, .idvar = "ResponseId",
#'                                  .outcomes = c("Q4.1", "Q5.1", "Q6.1", "Q7.1", "Q8.1","Q9.1"),
#'                                  .alphabet ="A")
#' reshaped_data$age2 = cut(reshaped_data$age, 2) # Dichotomize age
#' results = pj_diff(reshaped_data, .diff = "age2")



# In the current version, .group_by must be a dichotomous variable with {1, 2}
pj_diff <- function(.data, .diff, .tau = NULL){
  
  data <- .data %>% 
    rename("group_by_var" = !!rlang::sym(.diff)) 
  
  group1 <- data %>% filter(group_by_var == 1 & !is.na(group_by_var))
  group2 <- data %>% filter(group_by_var == 2 & !is.na(group_by_var))
  

  if(is.NULL(.tau)){
    out1 <- pj(group1)
    out2 <- pj(group2)
  }else if(is.numeric(.tau)){
    if(.tau < 1 | .tau > .5){
      out1 <- pj(group1, .tau = .tau)
      out2 <- pj(group2, .tau = .tau))
    }else{
      stop("tau must be between 0.5 and 1, not inclusive")
    }
  }else{
    stop("tau must be numeric between 0.5 and 1, not inclusive")
  }
  
  names(out1) <- str_c("group1:", names(out1))
  names(out2) <- str_c("group2:", names(out2))
  
  out1b <- out1 %>% rename("att_level" = `group1:att_level`)
  out2b <- out2 %>% rename("att_level" = `group2:att_level`)
  
  full_join(out1b, out2b, by = "att_level") %>% 
    mutate(tau = `group2:tau` - `group1:tau`,
           mm = `group2:mm` - `group1:mm`,
           mm_corrected = `group2:mm_corrected` - `group1:mm_corrected`,
           amce = `group2:amce` - `group1:amce`,
           amce_corrected = `group2:amce_corrected` - `group1:amce_corrected`) %>% 
    select(tau, att_level, mm, mm_corrected, amce, amce_corrected)

}

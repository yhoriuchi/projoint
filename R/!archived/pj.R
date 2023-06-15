#' Calculates the intra-respondent reliability (IRR), average marginal component effect (AMCE), marginal mean (MM), and correcte AMCEs and MMs for a given conjoint data set.
#'
#' @param .data A conjoint data set.
#' @param .tau An optional numeric argument allowing the researcher to specify their own value of \texttt{tau}
#' @return A summarized data set describing the results of a conjoint analysis, corrected for IRR measurement error.
#' @export
#' @examples
#' library(projoint)
#' 
#' data("exampleData1")
#' head(exampleData1)
#' reshaped_data = reshape_conjoint(exampleData1, .idvar = "ResponseId",
#'                                  .outcomes = c("Q4.1", "Q5.1", "Q6.1", "Q7.1", "Q8.1","Q9.1"),
#'                                  .alphabet ="A")
#' results = pj(reshaped_data)
#'
#' library(projoint)
#' 
#' data("exampleData2")
#' head(exampleData2)
#' reshaped_data = reshape_conjoint(exampleData1, .idvar = "ResponseId",
#'                                  .outcomes = c("Q4.1", "Q5.1", "Q6.1", "Q7.1", "Q8.1","Q9.1"),
#'                                  .alphabet ="A")
#' results = pj(reshaped_data, .tau = 0.75)



pj <- function(.data, .tau = NULL){
  
  att_level <- NULL

  if(is.NULL(.tau)){  
    .tau <- calculate_tau(.data)
  }else if(!is.NULL(.tau)){
    if(.tau >= 1 | .tau <= .5){
      stop(".tau must be between 0.5 and 1, not inclusive")
    }
  }

  mm_estimates <- calculate_mm(.data) %>% 
    tidyr::separate(att_level, sep = ":", into = c("attribute", "level"))
  
  mm_baselines <- mm_estimates %>% 
    dplyr::filter(str_detect(level, "level1$")) %>% 
    dplyr::select(-level) %>% 
    dplyr::rename("baseline" = mm)
  
  mm_estimates %>% 
    dplyr::left_join(mm_baselines, by = "attribute") %>% 
    dplyr::mutate(tau = .tau,
                  mm_corrected = (mm - tau) / (1 - 2 * tau),
                  amce = ifelse(level != "level1", mm - baseline, NA),
                  amce_corrected = amce / (1 - 2 * tau),
                  att_level = str_c(attribute, ":", level)) %>% 
    dplyr::select(tau, att_level, mm, mm_corrected, amce, amce_corrected) 
  
}
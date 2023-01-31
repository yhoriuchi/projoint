#' Calculates the intra-respondent reliability (IRR), average marginal component effect (AMCE), marginal mean (MM), and correcte AMCEs and MMs for a given conjoint data set.
#'
#' @param .data A conjoint data set.
#' @return A summarized data set describing the results of a conjoint analysis, corrected for IRR measurement error.



pj <- function(.data){
  
  att_level <- NULL
  
  .tau <- calculate_tau(.data)
  
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
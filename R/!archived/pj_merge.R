pj_merge <- function(.estimates_withoutBS, .estimates_withBS, .pvalue = 0.05){
  
  .lower <- .pvalue / 2
  .upper <- (1 - .lower)
  
  tau <- NULL
  mm <- NULL
  mm_corrected <- NULL
  amce <- NULL
  amce_corrected <- NULL
  att_level <- NULL
  
  out1 <- .estimates_withoutBS %>% 
    tidyr::pivot_longer(cols = c(tau, mm, mm_corrected, amce, amce_corrected), 
                        names_to = "estimand",
                        values_to = "mean") 
  
  out2 <- .estimates_withBS %>% 
    tidyr::pivot_longer(cols = c(tau, mm, mm_corrected, amce, amce_corrected), 
                        names_to = "estimand",
                        values_to = "value") %>% 
    dplyr::group_by(att_level, estimand) %>% 
    dplyr::summarise(bs_mean = mean(value, na.rm = TRUE),
                     se = sd(value, na.rm = TRUE),
                     lower = stats::quantile(value, .lower, na.rm = TRUE),
                     upper = stats::quantile(value, .upper, na.rm = TRUE), .groups = "drop") %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(bs_mean = ifelse(is.nan(bs_mean), NA, bs_mean))
  
  dplyr::left_join(out1, out2, 
                   by = c("att_level", "estimand"))
  
}

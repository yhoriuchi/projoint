# Used only within pj_bs()
pj_bs_sample <- function(.sample, .bs_data, .diff = NULL){
  
  sample <- NULL
  
  d <- .bs_data %>% 
    dplyr::filter(sample == .sample)
  
  if (is.null(.diff)){
    
    pj(d) %>% 
      dplyr::mutate(sample = .sample)
    
  } else{
    
    pj_diff(d, .diff) %>% 
      dplyr::mutate(sample = .sample)
    
    
  }
  
  
}
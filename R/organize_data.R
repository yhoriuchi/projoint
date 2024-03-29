#' Organize data before estimation
#'
#' This function converts a full conjoint data set to a data set structured for analyzing a specific attribute of interest and specific level(s) of interest. This function receives input from reshape_data() and its output feeds into pj_estimate().
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import rlang
#' @import tidyselect
#' @param .dataframe A data frame to reorganize -- the second element of the list generated by reshape_conjoint()
#' @param .structure either "choice_level" or "profile_level"
#' @param .att_choose A character column name identifying the attribute of interest (i.e., for the attribute-level or attribute-levels *chosen*).
#' @param .lev_choose  A character vector identifying the level or levels of interest (i.e., for the attribute-level or attribute-levels *chosen*). Its length should be 1 for profile-level analysis and 1+ for choice-level analysis
#' @param .att_notchoose A character column name identifying the attribute of interest (i.e., for the attribute-level or attribute-levels *not chosen*). This argument should be specified only if the `.structure` argument is "choice-level".
#' @param .lev_notchoose  A character vector identifying the level or levels of interest (i.e., for the attribute-level or attribute-levels *not chosen*). Its length should be 1 for profile-level analysis and 1+ for choice-level analysis. This argument should be specified only if the `.structure` argument is "choice-level".
#' @param .remove_ties TRUE if you want to remove ties for the attribute of interest (in profile-level analysis)
#' @keywords internal

organize_data <- function(
    .dataframe,
    .structure,
    .estimand,
    .remove_ties,
    .att_choose,
    .lev_choose,
    .att_notchoose,
    .lev_notchoose
){
  
  # bind variables locally to the function ----------------------------------
  
  id <- NULL
  selected <- NULL
  selected_repeated <- NULL
  selected_1 <- NULL
  selected_2 <- NULL
  task <- NULL
  ties <- NULL
  agree <- NULL
  profile <- NULL
  att <- NULL
  att_1 <- NULL
  att_2 <- NULL
  qoi_choose <- NULL
  qoi_choose_1 <- NULL
  qoi_choose_2 <- NULL
  qoi <- NULL
  qoi_1 <- NULL
  qoi_2 <- NULL
  qoi_notchoose <- NULL
  qoi_notchoose_1 <- NULL
  qoi_notchoose_2 <- NULL
  
  
  # check various settings --------------------------------------------------
  
  structure  <- rlang::arg_match0(.structure, c("choice_level", "profile_level"))
  
  if ((is.null(.remove_ties) | !is.logical(.remove_ties))){
    stop("The .remove_ties argumet must be logical.")
  }
  
  if (structure == "profile_level"){
    if (is.null(.att_choose) | length(.att_choose) != 1){
      stop("The .att_choose argument must be a character with the length of 1.")
    }
    if (is.null(.lev_choose) | length(.lev_choose) != 1){
      stop("The .lev_choose argument must be a character with the length of 1.")
    }
    if (!is.null(.att_notchoose)){
      stop("The .att_notchoose argument must be null.")
    }
    if (!is.null(.lev_notchoose)){
      stop("The .lev_notchoose argument must be null.")
    }
  } else if (structure == "choice_level"){
    if (is.null(.att_choose) | length(.att_choose) != 1){
      stop("The .att_choose argument must be a character with the length of 1.")
    }
    if (is.null(.lev_choose) | length(.lev_choose) < 1){
      stop("The .lev_choose argument must be a character with the length of 1+.")
    }
    if (is.null(.att_notchoose) | length(.att_notchoose) != 1){
      stop("The .att_notchoose argument must be a character with the length of 1.")
    }
    if (is.null(.lev_notchoose) | length(.lev_notchoose) < 1){
      stop("The .lev_notchoose argument must be a character with the length of 1+.")
    }
  } else {
    stop("The .strucutre argument should be either profile-level or choice-level.")
  }
  
  # Organize data to estimate profile-level MM ------------------------------
  
  if (structure == "profile_level"){
    
    # specify the attributes and levels of interest
    attlev_choose   <- stringr::str_c(.att_choose,  ":", .lev_choose)
    
    att_choose <- rlang::sym(.att_choose)
    
    # keep relevant rows only
    out2 <- out1 <- .dataframe %>% 
      dplyr::mutate(qoi_choose = !!att_choose) %>% 
      dplyr::filter(qoi_choose %in% c(attlev_choose)) %>% 
      dplyr::select(-matches("att\\d+$"))
    
    if (.remove_ties == TRUE){
      
      out2 <- out1 %>% 
        dplyr::group_by(id, task) %>% 
        dplyr::mutate(ties = dplyr::n() - 1) %>% 
        dplyr::ungroup() %>% 
        dplyr::filter(ties == 0) %>% 
        dplyr::select(-ties)
      
    }
    
    out <- out2
    
  } else {
    
    # specify the attributes and levels of interest
    attlev_choose    <- stringr::str_c(.att_choose,  ":", .lev_choose)
    attlev_notchoose <- stringr::str_c(.att_notchoose,  ":", .lev_notchoose)
    
    if (.att_choose == .att_notchoose){
      
      out1 <- .dataframe %>% 
        dplyr::mutate(qoi = !!rlang::sym(.att_choose)) %>% 
        dplyr::select(-matches("att\\d+$"))
      
      out2 <- out1 %>% 
        tidyr::pivot_wider(id_cols = c(id, task, agree), 
                           names_from = profile,
                           values_from = c(selected, qoi)) 
      
      out3 <- out2 %>% 
        dplyr::filter(qoi_1 == attlev_choose & qoi_2 == attlev_notchoose |
                        qoi_1 == attlev_notchoose & qoi_2 == attlev_choose) 
      
    } else{
      
      out1 <- .dataframe %>% 
        dplyr::mutate(qoi_choose    = !!rlang::sym(.att_choose),
                      qoi_notchoose = !!rlang::sym(.att_notchoose)) %>% 
        dplyr::select(-matches("att\\d+$")) %>% 
        dplyr::filter(qoi_choose %in% attlev_choose & qoi_notchoose %in% attlev_notchoose)
      
      
      out2 <- out1 %>% 
        tidyr::pivot_wider(id_cols = c(id, task, agree), 
                           names_from = profile,
                           values_from = c(selected, qoi_choose, qoi_notchoose)) 
      
      out3 <- out2 %>% 
        dplyr::filter(qoi_choose_1 == attlev_choose & qoi_notchoose_2 == attlev_notchoose |
                        qoi_choose_1 == attlev_notchoose & qoi_notchoose_2 == attlev_choose) 
      
    }
    
    out <- out3
  }

  # Keep necessary variables only and return --------------------------------
  
  # data frame to estimate IRR
  data1 <- .dataframe %>% 
    dplyr::select(id, agree) %>% 
    dplyr::filter(!is.na(agree)) %>% 
    dplyr::distinct() %>% 
    tibble::as_tibble()
  
  # data frame to estimate MM or AMCE
  data2 <- out
  
  list("data_for_irr" = data1, 
       "data_for_estimand" = data2) %>% 
    return()
  
}

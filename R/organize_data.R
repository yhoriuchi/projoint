#' Organize Data for Estimation
#'
#' Internal helper function to organize data for marginal mean (MM) or average marginal component effect (AMCE) estimation.
#' Called inside \code{pj_estimate()}.
#'
#' @param .dataframe The reshaped conjoint dataset (from \code{reshape_projoint()}).
#' @param .structure Either \code{"profile_level"} or \code{"choice_level"}.
#' @param .estimand Either \code{"mm"} or \code{"amce"}.
#' @param .remove_ties Logical. Whether to remove tied responses (default = TRUE).
#' @param .att_choose Attribute ID for chosen profile.
#' @param .lev_choose Level ID for chosen profile.
#' @param .att_notchoose (Optional) Attribute ID for non-chosen profile (only for choice-level analysis).
#' @param .lev_notchoose (Optional) Level ID for non-chosen profile (only for choice-level analysis).
#'
#' @return A list with two tibbles: \code{data_for_estimand} and \code{data_for_irr}.
#'
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
    stop("The .structure argument should be either profile_level or choice_level.")
  }
  
  # Organize data to estimate profile-level MM ------------------------------
  
  if (structure == "profile_level"){
    
    # specify the attributes and levels of interest
    attlev_choose   <- stringr::str_c(.att_choose,  ":", .lev_choose)
    att_choose <- rlang::sym(.att_choose)
    
    # keep relevant rows only
    out2 <- out1 <- .dataframe |> 
      dplyr::mutate(qoi_choose = !!att_choose) |> 
      dplyr::filter(qoi_choose %in% c(attlev_choose)) |> 
      dplyr::select(-matches("att\\d+$"))
    
    if (.remove_ties == TRUE){
      
      out2 <- out1 |> 
        dplyr::group_by(id, task) |> 
        dplyr::mutate(ties = dplyr::n() - 1) |> 
        dplyr::ungroup() |> 
        dplyr::filter(ties == 0) |> 
        dplyr::select(-ties)
      
    }
    
    out <- out2
    
  } else {
    
    # specify the attributes and levels of interest
    attlev_choose    <- stringr::str_c(.att_choose,  ":", .lev_choose)
    attlev_notchoose <- stringr::str_c(.att_notchoose,  ":", .lev_notchoose)
    
    if (.att_choose == .att_notchoose){
      
      out1 <- .dataframe |> 
        dplyr::mutate(qoi = !!rlang::sym(.att_choose)) |> 
        dplyr::select(-matches("att\\d+$"))
      
      out2 <- out1 |> 
        tidyr::pivot_wider(id_cols = c(id, task, agree), 
                           names_from = profile,
                           values_from = c(selected, qoi)) 
      
      out3 <- out2 |> 
        dplyr::filter(qoi_1 == attlev_choose & qoi_2 == attlev_notchoose |
                        qoi_1 == attlev_notchoose & qoi_2 == attlev_choose) 
      
    } else{
      
      out1 <- .dataframe |> 
        dplyr::mutate(qoi_choose    = !!rlang::sym(.att_choose),
                      qoi_notchoose = !!rlang::sym(.att_notchoose)) |> 
        dplyr::select(-matches("att\\d+$")) |> 
        dplyr::filter(qoi_choose %in% attlev_choose & qoi_notchoose %in% attlev_notchoose)
      
      
      out2 <- out1 |> 
        tidyr::pivot_wider(id_cols = c(id, task, agree), 
                           names_from = profile,
                           values_from = c(selected, qoi_choose, qoi_notchoose)) 
      
      out3 <- out2 |> 
        dplyr::filter(qoi_choose_1 == attlev_choose & qoi_notchoose_2 == attlev_notchoose |
                        qoi_choose_1 == attlev_notchoose & qoi_notchoose_2 == attlev_choose) 
      
    }
    
    out <- out3
  }

  # Keep necessary variables only and return --------------------------------
  
  # data frame to estimate IRR
  data1 <- .dataframe |> 
    dplyr::select(id, agree) |> 
    dplyr::filter(!is.na(agree)) |> 
    dplyr::distinct() |> 
    tibble::as_tibble()
  
  # data frame to estimate MM or AMCE
  data2 <- out
  
  return(
    list(
      "data_for_irr" = data1, 
      "data_for_estimand" = data2
    )
  )
  
  
}

#' Organize data for estimation (internal helper)
#'
#' Prepares tidy inputs for MM/AMCE estimation and IRR handling.
#' Called inside \code{pj_estimate()} after reshaping to respondent–task–profile.
#'
#' @param .dataframe A tibble/data frame from \code{\link{reshape_projoint}()},
#'   containing columns like \code{id}, \code{task}, \code{profile}, \code{selected},
#'   \code{agree} (if repeated), and attribute columns named \code{att1}, \code{att2}, ...
#'   that store \code{level_id}s (e.g., \code{"att1:level2"}).
#' @param .structure Either \code{"profile_level"} or \code{"choice_level"}.
#' @param .estimand Either \code{"mm"} or \code{"amce"}.
#' @param .remove_ties Logical; if \code{TRUE} (default) remove tied responses
#'   in profile-level setups (keeps tasks where exactly one profile is selected).
#' @param .att_choose Attribute ID for the “chosen” side (e.g., \code{"att3"}).
#' @param .lev_choose Level ID(s) for the chosen side (e.g., \code{"level2"} for
#'   profile-level; vector of level IDs for choice-level).
#' @param .att_notchoose Attribute ID for the “not chosen” side (choice-level only).
#' @param .lev_notchoose Level ID(s) for the not-chosen side (choice-level only).
#'
#' @return A named list with two tibbles:
#' \itemize{
#'   \item \code{$data_for_estimand}: rows restricted and reshaped to the
#'         requested estimand/QOI. For profile-level, one row per respondent–task–profile
#'         where the target level is present (ties optionally removed). For choice-level,
#'         one row per respondent–task with paired information for profiles 1 and 2,
#'         restricted to the requested attribute-level pair(s).
#'   \item \code{$data_for_irr}: one row per respondent with columns \code{id},
#'         \code{agree} (if available), and any detected weight/cluster hints
#'         (columns matching \code{"my_weight|my_cluster|^weights?$|^clusters?$"}).
#' }
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
  
  # check various settings --------------------------------------------------
  
  structure  <- rlang::arg_match0(.structure, c("choice_level", "profile_level"))
  
  if ((is.null(.remove_ties) | !is.logical(.remove_ties))){
    stop("The .remove_ties argument must be logical.")
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
  # data1 <- .dataframe |> 
  #   dplyr::select(id, agree) |> 
  #   dplyr::filter(!is.na(agree)) |> 
  #   dplyr::distinct() |> 
  #   tibble::as_tibble()
  
  data1 <- .dataframe |> 
    dplyr::filter(!is.na(agree)) |> 
    dplyr::group_by(id) |> 
    dplyr::slice(1) |>  # take first occurrence per respondent
    dplyr::ungroup() |> 
    dplyr::select(id, agree, matches("my_weight|my_cluster|^weights?$|^clusters?$")) |> 
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

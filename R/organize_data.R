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
) {
  # check various settings --------------------------------------------------

  structure <- rlang::arg_match0(.structure, c("choice_level", "profile_level"))

  if ((is.null(.remove_ties) | !is.logical(.remove_ties))) {
    stop("The .remove_ties argument must be logical.")
  }

  if (structure == "profile_level") {
    if (is.null(.att_choose) | length(.att_choose) != 1) {
      stop("The .att_choose argument must be a character with the length of 1.")
    }
    if (is.null(.lev_choose) | length(.lev_choose) != 1) {
      stop("The .lev_choose argument must be a character with the length of 1.")
    }
    if (!is.null(.att_notchoose)) {
      stop("The .att_notchoose argument must be null.")
    }
    if (!is.null(.lev_notchoose)) {
      stop("The .lev_notchoose argument must be null.")
    }
  } else if (structure == "choice_level") {
    if (is.null(.att_choose) | length(.att_choose) != 1) {
      stop("The .att_choose argument must be a character with the length of 1.")
    }
    if (is.null(.lev_choose) | length(.lev_choose) < 1) {
      stop("The .lev_choose argument must be a character with the length of 1+.")
    }
    if (is.null(.att_notchoose) | length(.att_notchoose) != 1) {
      stop("The .att_notchoose argument must be a character with the length of 1.")
    }
    if (is.null(.lev_notchoose) | length(.lev_notchoose) < 1) {
      stop("The .lev_notchoose argument must be a character with the length of 1+.")
    }
  } else {
    stop("The .structure argument should be either profile_level or choice_level.")
  }


  if (structure == "profile_level") {
    # Organize data to estimate profile-level MM ------------------------------

    # specify the attributes and levels of interest
    attlev_choose <- stringr::str_c(.att_choose, ":", .lev_choose)
    att_choose <- rlang::sym(.att_choose)

    # keep relevant rows only
    out2 <- out1 <- .dataframe |>
      dplyr::mutate(qoi_choose = !!att_choose) |>
      dplyr::filter(qoi_choose %in% attlev_choose) |>
      dplyr::select(-matches("^att\\d+$"))

    if (.remove_ties == TRUE) {
      out2 <- out1 |>
        dplyr::group_by(id, task) |>
        dplyr::mutate(ties = dplyr::n() - 1) |>
        dplyr::ungroup() |>
        dplyr::filter(ties == 0) |>
        dplyr::select(-ties)
    }

    out <- out2
  } else {
    # Organize data to estimate profile-level MM ------------------------------

    # specify the attributes and levels of interest
    attlev_choose <- stringr::str_c(.att_choose, ":", .lev_choose)
    attlev_notchoose <- stringr::str_c(.att_notchoose, ":", .lev_notchoose)

    # Helper: keep only columns that are constant within (id, task) across profiles
    keep_task_constant_cols <- function(df, id_cols) {
      # If id/task missing, fall back to the input
      if (!all(c("id", "task", "profile") %in% names(df))) {
        return(id_cols)
      }

      # Candidates other than the core keys
      core <- intersect(c("id", "task", "agree"), id_cols)
      cand <- setdiff(id_cols, core)
      if (length(cand) == 0L) {
        return(core)
      }

      # A column is "safe" iff it never varies across profiles within any (id, task)
      max_ndist <- df |>
        dplyr::group_by(id, task) |>
        dplyr::summarise(
          dplyr::across(dplyr::all_of(cand), ~ dplyr::n_distinct(.x, na.rm = FALSE)),
          .groups = "drop"
        ) |>
        dplyr::summarise(dplyr::across(dplyr::everything(), ~ max(.x, na.rm = TRUE))) |>
        unlist(use.names = TRUE)

      safe <- names(max_ndist)[max_ndist <= 1]
      unique(c(core, safe))
    }


    if (.att_choose == .att_notchoose) {
      out1 <- .dataframe |>
        dplyr::mutate(qoi = !!rlang::sym(.att_choose)) |>
        dplyr::select(-matches("^att\\d+$"))

      id_cols <- setdiff(names(out1), c("profile", "selected", "selected_repeated", "qoi"))
      id_cols <- keep_task_constant_cols(out1, id_cols)

      out2 <- out1 |>
        tidyr::pivot_wider(
          # id_cols = dplyr::all_of(setdiff(names(out1), c("profile", "selected", "selected_repeated", "qoi"))),
          id_cols = dplyr::all_of(id_cols),
          names_from = profile,
          values_from = c(selected, qoi)
        )

      stopifnot(all(out2 |> dplyr::count(id, task) |> dplyr::pull(n) == 1L))

      out3 <- out2 |>
        dplyr::filter(
          (qoi_1 %in% attlev_choose & qoi_2 %in% attlev_notchoose) |
            (qoi_1 %in% attlev_notchoose & qoi_2 %in% attlev_choose)
        )
    } else {
      out1 <- .dataframe |>
        dplyr::mutate(
          qoi_choose = !!rlang::sym(.att_choose),
          qoi_notchoose = !!rlang::sym(.att_notchoose)
        ) |>
        dplyr::select(-matches("^att\\d+$"))
      # dplyr::filter(qoi_choose %in% attlev_choose & qoi_notchoose %in% attlev_notchoose)

      id_cols <- setdiff(names(out1), c("profile", "selected", "selected_repeated", "qoi_choose", "qoi_notchoose"))
      id_cols <- keep_task_constant_cols(out1, id_cols)

      out2 <- out1 |>
        tidyr::pivot_wider(
          # id_cols = dplyr::all_of(setdiff(names(out1), c("profile", "selected", "selected_repeated", "qoi_choose", "qoi_notchoose"))),
          id_cols = dplyr::all_of(id_cols),
          names_from = profile,
          values_from = c(selected, qoi_choose, qoi_notchoose)
        )

      stopifnot(all(out2 |> dplyr::count(id, task) |> dplyr::pull(n) == 1L))

      # out3 <- out2 |>
      #   dplyr::filter(
      #       (qoi_choose_1 %in% attlev_choose & qoi_notchoose_2 %in% attlev_notchoose) |
      #       (qoi_choose_1 %in% attlev_notchoose & qoi_notchoose_2 %in% attlev_choose)
      #   )

      out3 <- out2 |>
        dplyr::filter(
          (qoi_choose_1 %in% attlev_choose & qoi_notchoose_2 %in% attlev_notchoose) |
            (qoi_choose_2 %in% attlev_choose & qoi_notchoose_1 %in% attlev_notchoose)
        )
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

  # data1 <- .dataframe |>
  #   dplyr::filter(!is.na(agree)) |>
  #   dplyr::group_by(id) |>
  #   dplyr::slice(1) |> # take first occurrence per respondent
  #   dplyr::ungroup() |>
  #   # dplyr::select(id, agree, matches("my_weight|my_cluster|^weights?$|^clusters?$")) |>
  #   dplyr::select(id, agree, matches("my_weight|my_cluster|^weight(s)?$|^cluster(s)?$|^weights?$|^clusters?$")) |>
  #   tibble::as_tibble()

  data1 <- .dataframe |>
    dplyr::filter(!is.na(agree)) |>
    dplyr::group_by(id) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(-matches("^att\\d+$")) |>
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

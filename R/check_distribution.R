#' Check the distribution of levels for each attribute
#'
#' After generating test responses, it is worth checking the distribution of levels for each attribute using this function to ensure balanced randomization.
#'
#' @import dplyr
#' @import rlang
#' @param .data A data frame specifically for conjoint analysis (use reshape_conjoint())
#' @return A data frame
#'

check_distribution <- function(.data){

  attribute <- level <- frequency <- NULL

  out <- NULL

  for (i in seq(from = 4, to = ncol(.data) - 2, by = 1)){

    att <- names(.data)[i]
    var_quo <- rlang::sym(att)

    temp <- count(.data, !!var_quo)%>%
      mutate(attribute = att) %>%
      set_names(c("level", "frequency", "attribute"))

    out <- bind_rows(out, temp)

  }

  out %>%
    select(attribute, level, frequency) %>%
    return()

}


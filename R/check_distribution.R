#' check the distribution of levels for each attribute
#'
#' This functino is perhaps useful using test responses. Check the distibution of levels of each attribute.
#'
#' @import dplyr
#' @param .data A data frame (ashaped for conjoint analysis)
#' @return A data-frame
#' @export
#'

check_distribution <- function(.data){

  levels.Var1 <- levels.Freq <- attribute <- NULL

  out <- NULL

  for (i in seq(from = 4, to = ncol(.data) - 2, by = 1)){

    temp <- data.frame(
      levels = table(.data[i])
    ) %>%
      mutate(attribute = names(.data[i])) %>%
      select(attribute,
             "level" = levels.Var1,
             "freq" = levels.Freq)

    out <- bind_rows(out, temp)

  }

  return(out)
}

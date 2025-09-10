#' Read and apply a reordered attribute/level mapping
#'
#' Reads a CSV containing a revised ordering of attributes and levels and applies it
#' to an existing \code{\link{projoint_data}} object. Typical workflow: first save the
#' current labels to CSV (e.g., with \code{\link{save_labels}}), manually reorder rows
#' (and/or the attribute grouping) in the CSV, then call \code{read_labels()} to apply.
#'
#' @param .data A \code{\link{projoint_data}} object whose labels/data should be reordered.
#' @param .filename Path to the revised labels CSV (originally produced from the package's labels).
#'
#' @return A \code{\link{projoint_data}} object with the same content as \code{.data} but with
#'   attributes and levels reordered to match the CSV. The returned object contains:
#'   \itemize{
#'     \item \code{$labels}: a tibble with new \code{attribute_id} and \code{level_id} reflecting the chosen order
#'     \item \code{$data}: a tibble whose \code{att*} columns have been remapped to the new \code{level_id}s
#'   }
#'
#' @seealso \code{\link{save_labels}}, \code{\link{reshape_projoint}}
#'
#' @import readr
#' @import dplyr
#' @import forcats
#' @import tidyr
#' @import stringr
#' @import tidyselect
#'
#' @examples
#' \donttest{
#' # Create a projoint_data object from the example dataset
#' data(exampleData1)
#' outcomes <- c(paste0("choice", 1:8), "choice1_repeated_flipped")
#' pj <- reshape_projoint(exampleData1, outcomes)
#'
#' # Write current labels to a temporary CSV, adding an 'order' column
#' tmp <- tempfile(fileext = ".csv")
#' pj$labels |>
#'   dplyr::mutate(order = dplyr::row_number()) |>
#'   readr::write_csv(tmp)
#'
#' # (User would reorder rows in 'tmp' manually; we just read it back)
#' pj_reordered <- read_labels(pj, tmp)
#'
#' # Inspect the updated label order
#' head(pj_reordered$labels)
#' }
#'
#' @export
read_labels <- function(
    .data,
    .filename
){
  
  labels_arranged <- readr::read_csv(.filename, 
                                     show_col_types = FALSE) |> 
    dplyr::mutate(attribute_id = stringr::str_extract(level_id, "^.+(?=\\:)"),
                  attribute_id_arranged = forcats::fct_reorder(attribute_id, order),
                  attribute_id_arranged = stringr::str_c("att", as.numeric(attribute_id_arranged))) |> 
    dplyr::arrange(order) |> 
    dplyr::group_by(attribute_id_arranged) |>
    dplyr::mutate(level_id_arranged = dplyr::row_number(),
                  level_id_arranged = stringr::str_c(attribute_id_arranged, ":level", level_id_arranged)) |>
    dplyr::ungroup() 
  
  data_arranged <- .data$data |> 
    tidyr::pivot_longer(cols = tidyselect::contains("att")) |> 
    dplyr::mutate(value = as.character(value)) |> 
    dplyr::left_join(labels_arranged, 
                     by = c("value" = "level_id")) |> 
    dplyr::select(-name, -value, -attribute, -attribute_id, -level, -order) |> 
    tidyr::pivot_wider(names_from = attribute_id_arranged, 
                       values_from = level_id_arranged)
  
  labels_arranged <- labels_arranged |> 
    dplyr::select(-order, -attribute_id, -level_id) |> 
    dplyr::rename(attribute_id = attribute_id_arranged,
                  level_id = level_id_arranged)
  
  # return new projoint_data object
  out <- projoint_data("labels" = labels_arranged, 
                       "data" = data_arranged)
  
  return(out)
  
}
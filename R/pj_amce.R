#' @rdname pj_amce
#' @title Adjust AMCEs based on our method
#' @description Adjust AMCEs based on the estimate/assumed measurement error and return a tidy data frame of results
#'
#' @import dplyr
#' @import cregg
#' @import stats
#' @import survey
#' @param data (Description from the cregg package's amce function) A data frame containing variables specified in \code{formula}. All RHS variables should be factors; the base level for each will be used in estimation and its reported AMCE will be NA (for printing). Optionally, this can instead be an object of class \dQuote{survey.design} returned by \code{\link[survey]{svydesign}}.
#' @param formula (Description from the cregg package's amce function) A formula specifying an AMCE model to be estimated. All variables should be factors; all levels across features should be unique. Two-way constraints can be specified with an asterisk (*) between RHS features. The specific constrained level pairs within these features are then detected automatically. Higher-order constraints are not allowed.
#' @param id
#' @param n_boot The number of bootstrapped samples
#' @param tau The estimated/assumed swapping error
#' @return A data frame of class \dQuote{cj_amce}
#' @export
#'

pj_amce <- function(data, formula, id = ~ 0, n_boot = 100, tau = 0.25, ...){

  id <- NULL
  estimate <- estimate_fixed <- NULL
  std.error <- z <- p <- lower <- upper <- NULL
  feature <- level <- NULL
  level_name <- task <- outcome_qnum <- outcomes <- NULL

  # A list of IDs
  ids <- unique(data[all.vars(id)]) %>% pull()

  # Rename ID
  d <- data %>%
    dplyr::rename(ID = all.vars(id))

  # Bootstrapping
  out <- NULL
  for (i in seq_along(1:n_boot)){

    # respondents' IDs
    boot_ids <- data.frame(ID = sample(ids, length(ids), replace = TRUE))

    # add data
    boot_df <- boot_ids %>% dplyr::left_join(d, by = "ID")

    # Run a model
    boot_out <- cregg::amce(boot_df, formula, id, ...) %>%
      # Suppress the warning message shown when using cregg::amce()
      suppressWarnings() %>%
      # Add the number for each bootstrapped sample
      dplyr::mutate(sample = i)

    # Bind the results and fix the estimates
    out <- dplyr::bind_rows(out, boot_out) %>%
      dplyr::mutate(estimate_fixed = estimate / (1 - 2 * stats::rbinom(1, length(ids), tau) / length(ids)))

  }

  # Calculate the 95% confidence intervals
  out_ci <- out %>%
    dplyr::select(-estimate, -std.error, -z, -p, -lower, -upper) %>%
    dplyr::group_by(feature, level) %>%
    dplyr::summarize(estimate = mean(estimate_fixed),
                     lower = stats::quantile(estimate_fixed, 0.025),
                     upper = stats::quantile(estimate_fixed, 0.975),
                     .groups = "drop")

  # Return a data frame with "cj_amce" class
  structure(out_ci, class = c("cj_amce", "data.frame")) %>%
    return()

}

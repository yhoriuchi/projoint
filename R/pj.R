#' @rdname pj
#' @title Adjust AMCEs or MMs to account for measurement error
#' @description Adjust Average Marginal Component Effects (AMCEs) or Marginal Means (MMs) based on the estimate/assumed measurement error and return a tidy data frame of results
#' @import dplyr
#' @import cregg
#' @param data (Description from the cregg package's amce function) A data frame containing variables specified in \code{formula}. All RHS variables should be factors; the base level for each will be used in estimation and its reported AMCE will be NA (for printing). Optionally, this can instead be an object of class \dQuote{survey.design} returned by \code{\link[survey]{svydesign}}.
#' @param formula (Description from the cregg package's amce function) A formula specifying an AMCE model to be estimated. All variables should be factors; all levels across features should be unique. Two-way constraints can be specified with an asterisk (*) between RHS features. The specific constrained level pairs within these features are then detected automatically. Higher-order constraints are not allowed.
#' @param id A formula with a single right-hand-side variable indicating the respondent-level identifier
#' @param estimand A character string specifying an estimate type. Current options are average marginal component effects (“amce”) or marginal means (“mm”),
#' @param by A formula with a single right-hand-side variable indicating the variable by which to estimate subgroup differences
#' @param n_boot The number of bootstrapped samples. Defaults to 100.
#' @param tau The estimated/assumed swapping error. Defaults to 0.25.
#' @param ... Optional arguments to pass to \code{amce()}. For documentation see the \code{cregg} library.
#' @return A data frame of class \dQuote{cj_amce}
#' @export
#'

pj <- function(data, formula, id = ~ 0, estimand = c("amce", "mm"), by = NULL,
               n_boot = 100, tau = 0.25, ...){

  if (tau < 0 | tau > 1){
    stop("tau must be between 0 and 1")
  }

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

    # Randomly pick a tau from the binomial distribution
    tau <- stats::rbinom(1, length(ids), tau) / length(ids)

    # Run a model
    boot_out <- cregg::cj(boot_df, formula, ~ID, estimate = estimand, by = by, ...) %>%

      # Suppress the warning message shown when using cregg::amce()
      suppressWarnings() %>%

      # Add the number for each bootstrapped sample
      dplyr::mutate(sample = i)

    # Bind the results

    out <- dplyr::bind_rows(
      out,
      boot_out
    )

  }

  # Fix the estimate

  if (estimand == "amce"){

    out_fixed <- out %>%
      dplyr::mutate(estimate_fixed = estimate / (1 - 2 * tau))

  } else if (estimand == "mm"){

    out_fixed <- out %>%
      dplyr::mutate(estimate_fixed = (estimate - tau) / (1 - 2 * tau))

  }

  # Calculate the 95% confidence intervals
  out_ci <- out_fixed %>%
    dplyr::select(-estimate, -std.error, -z, -p, -lower, -upper) %>%
    dplyr::group_by(feature, level) %>%
    dplyr::summarize(estimate = mean(estimate_fixed),
                     lower = stats::quantile(estimate_fixed, 0.025),
                     upper = stats::quantile(estimate_fixed, 0.975),
                     .groups = "drop")

  if (estimand == "amce"){

    # Return a data frame with "cj_amce" class
    structure(out_ci, class = c("cj_amce", "data.frame")) %>%
      return()

  } else if (estimand == "mm"){

    # Return a data frame with "cj_mm" class
    structure(out_ci, class = c("cj_mm", "data.frame")) %>%
      return()
  }


}


% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projoint_level.R
\name{projoint_level}
\alias{projoint_level}
\title{Analyze a conjoint data set and correct for measurement error}
\usage{
projoint_level(
  .data,
  .qoi,
  .structure,
  .estimand,
  .se_method,
  .irr,
  .remove_ties,
  .ignore_position,
  .n_sims,
  .n_boot,
  .weights_1,
  .clusters_1,
  .se_type_1,
  .weights_2,
  .clusters_2,
  .se_type_2
)
}
\arguments{
\item{.data}{A \code{\link{projoint_data}} object}

\item{.qoi}{A \code{\link{projoint_qoi}} object. If \code{NULL}, defaults to producing all MMs and all AMCEs.}

\item{.structure}{Either \code{"profile_level"} (default) or \code{"choice_level"}}

\item{.estimand}{Either \code{"mm"} for marginal mean or \code{"amce"} for average marginal component effect}

\item{.se_method}{By default, \code{c("analytic", "simulation", "bootstrap")} description}

\item{.irr}{\code{NULL} (default) if IRR is to be calculated using the repeated task. Otherwise, a numerical value}

\item{.remove_ties}{Logical: should ties be removed before estimation? Defaults to \code{TRUE}.}

\item{.ignore_position}{TRUE (default) if you ignore the location of profile (left or right). Relevant only if analyzed at the choice level}

\item{.n_sims}{The number of simulations. Relevant only if \code{.se_method == "simulation"}}

\item{.n_boot}{The number of bootstrapped samples. Relevant only if \code{.se_method == "bootstrap"}}

\item{.weights_1}{the weight to estimate IRR (see \code{\link[estimatr]{lm_robust}}): \code{NULL} (default)}

\item{.clusters_1}{the clusters to estimate IRR (see \code{\link[estimatr]{lm_robust}}): \code{NULL} (default)}

\item{.se_type_1}{the standard error type to estimate IRR (see \code{\link[estimatr]{lm_robust}}): \code{"classical"} (default)}

\item{.weights_2}{the weight to estimate MM or AMCE (see \code{\link[estimatr]{lm_robust}}): \code{NULL} (default)}

\item{.clusters_2}{the clusters to estimate MM or AMCE (see \code{\link[estimatr]{lm_robust}}): \code{NULL} (default)}

\item{.se_type_2}{the standard error type to estimate MM or AMCE (see \code{\link[estimatr]{lm_robust}}): \code{"classical"} (default)}
}
\value{
A \code{\link{projoint_results}} object
}
\description{
This is the internal function used to calculate and correct marginal means or average marginal component effects of a conjoint design.
}
\keyword{internal}

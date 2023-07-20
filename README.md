# <img src="man/figures/projoint.png" align="center" width="150" height="150" />

# projoint

### The One-Stop Conjoint Shop

---

**projoint** is a package for more general, more straightforward, and more creative conjoint analysis. It estimates Average Marginal Component Effects (AMCEs) and Marginal Means (MMs) based on a conjoint survey experiment. It produces more reliable estimates after correcting measurement error bias and other problems known in the literature (e.g., having the same levels for the attribute of interest). Furthermore, it presents a more general framework so that researchers can answer a range of substantively important questions more straightforwardly.

Some notes:

* The current version assumes that the outcome variable is a binary forced choice.

* This package is still under construction. We plan to make the first release by the end of July 2023.


## Installation

You can install the development version of **projoint** from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("yhoriuchi/projoint")
```

## Scholarly Article

For the framework and methods based on **projoint**, please read the following article:

Katherine Clayton, Yusaku Horiuchi, Aaron R. Kaufman, Gary King, and Mayya Komisarchik. “Correcting Measurement Error Bias in Conjoint Survey Experiments”. Working Paper. [[Paper](https://gking.harvard.edu/sites/scholar.harvard.edu/files/gking/files/conerr.pdf)] [[Supplementary Appendix](https://gking.harvard.edu/sites/scholar.harvard.edu/files/gking/files/conerr-supp.pdf)]

> **Abstract:** Conjoint survey designs are spreading across the social sciences due to their unusual capacity to estimate many causal effects from a single randomized experiment. Unfortunately, by their ability to mirror complicated real-world choices, these designs often generate substantial measurement error and thus bias. We replicate both the data collection and analysis from eight prominent conjoint studies, all of which closely reproduce published results, and show that a large proportion of observed variation in answers to conjoint questions is effectively random noise. We then discover a common empirical pattern in how measurement error appears in conjoint studies and, with it, introduce an easy-to-use statistical method to correct the bias.




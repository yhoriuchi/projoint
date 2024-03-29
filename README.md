# <img src="man/figures/projoint.png" align="center" width="150" height="150" />

# projoint

## The One-Stop Conjoint Shop

---

**projoint** is a general-purpose R package for conjoint analysis. It produces more reliable estimates of all relevant quantities of interest, after correcting measurement error bias and other problems discussed in the literature. Furthermore, it also implements a more general framework than other approaches, so that researchers can answer a much wider range of substantively important questions.

### Installation

You can install the development version of **projoint** from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("yhoriuchi/projoint")
```

### Example

``` r
# Load the projoint package
library(projoint)

# Reshape data for conjoint analysis
# This example includes the repeated task.
data <- reshape_projoint(exampleData1, 
                         c(paste0("choice", 1:8), "choice1_repeated_flipped"))

# Run conjoint analysis
output <- projoint(data)

# Make a figure
plot(output)

# Show the estimated quantities of interest
summary(output)

```

### Relevant Article

Our framework and methods can be found in this paper:

* Katherine Clayton, Yusaku Horiuchi, Aaron R. Kaufman, Gary King, and Mayya Komisarchik. “Correcting Measurement Error Bias in Conjoint Survey Experiments”. [[Working Paper](https://gking.harvard.edu/conjointE)]
  + **Abstract:** Conjoint survey designs are spreading across the social sciences due to their unusual capacity to estimate many causal effects from a single randomized experiment. Unfortunately, by their ability to mirror complicated real-world choices, these designs often generate substantial measurement errors and thus bias. We replicate both the data collection and analysis from eight prominent conjoint studies, all of which closely reproduce published results, and show that a large proportion of observed variation in answers to conjoint questions is effectively random noise. We then discover a common empirical pattern in how measurement error appears in conjoint studies and, with it, introduce an easy-to-use statistical method to correct the bias.

### Join Our Projoint Community

We encourage you to join our Projoint Community by making a GitHub account, subscribing to our [Announcements](https://github.com/yhoriuchi/projoint/discussions/categories/announcements), and actively joining [Discussions](https://github.com/yhoriuchi/projoint/discussions). We encourage you to post questions, suggest for improvement, and share your research findings using our software. For any problem you find, please report your issue to [Issues](https://github.com/yhoriuchi/projoint/issues). 

### Notes

* The current version assumes that the outcome variable is a binary forced choice.

* This package is still under construction. Forthcoming features include the following:
  + Allow researchers to use weights for features and for respondents.
  + Allow researchers to use other outcome variables, such as rating.

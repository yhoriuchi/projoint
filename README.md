# <img src="man/figures/projoint.png" align="center" width="150" height="150" />

# projoint

### Correcting Measurement Error Bias in Conjoint Survey Experiments

---

**projoint** is a package to estimate Average Marginal Component Effects (AMCEs) or Marginal Means (MMs) based on a conjoint survey experiment. Unlike other packages for conjoint analysis, **projoint** produces the more reliable estimates after correcting measurement error bias. 


Some notes:

* The current version assumes that the outcome variable is a binary forced choice.

* The outcome variables in the CSV file can be either a numerical variable (1 or 2) or a character variable, such as {"Candidate 1", "Candidate 2"} or {"Plan A", "Plan B"}. 


## Installation

You can install the development version of **projoint** from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("yhoriuchi/projoint")
```

## Example

### Case 1: Without a repeated task

The original data set is from *Campus Diversity: The Hidden Consensus* by John Carey, Katie Clayton, and Yusaku Horiuchi (Cambridge University Press, 2020). The complete replication package is available at https://doi.org/10.7910/DVN/KMS5ZY. For more information about the book, see: https://horiuchi.org/diversity/.

```r
library(projoint)
outcomes <- paste0("Q2.", seq(from = 4, to = 31, by = 3))
cjdata <- reshape_conjoint(exampleData1, V1, outcomes)
```
To read the original Qualtrics data (with two rows being used for information about each column), use another function in this package, <code>read_Qualtrics()</code>. For example, 
```r
library(cjdata)
data <- read_Qualtrics("Qualtrics_data_without_a_repeated_task.csv")
cjdata <- reshape_conjoint(data, V1, paste0("Q2.", seq(from = 4, to = 31, by = 3))
```
Note: "Qualtrics_data_without_a_repeated_task.csv" is downloadable from the "data-raw" folder of this GitHub repository.

### Case 2: With a repeated task


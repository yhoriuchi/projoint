<p align="center">
  <img src="man/figures/projoint.png" width="150" height="150" />
</p>

<p align="center">
  <a href="https://github.com/yhoriuchi/projoint/actions/workflows/R-CMD-check.yaml">
    <img src="https://github.com/yhoriuchi/projoint/actions/workflows/R-CMD-check.yaml/badge.svg" alt="R-CMD-check" />
  </a>
  <a href="https://CRAN.R-project.org/package=projoint">
    <img src="https://www.r-pkg.org/badges/version/projoint" alt="CRAN Status" />
  </a>
  <a href="https://cran.r-project.org/package=projoint">
    <img src="https://cranlogs.r-pkg.org/badges/grand-total/projoint" alt="Downloads" />
  </a>
</p>

<p align="center" style="font-size: 32px;"><b>🛠️ The One-Stop Conjoint Shop: projoint</b></p>

---

**projoint** is a general-purpose R package for conjoint analysis. It produces **more reliable estimates** of many under explored quantities of interest (based questions explicitly about **choices**) by **correcting measurement error bias**.

<details>

<summary><b>Understand how projoint structures conjoint data</b></summary>

The **projoint** package can organize survey data into two levels:

- **Choice-level** (RECOMMENDED): Each choice respondents make between profiles

- **Profile-level**: Each profile shown to respondents, including its attributes and levels

[Learn more about data structure requirements](https://yhoriuchi.github.io/projoint/articles/structure.html)

</details>

<details>

<summary><b>Understand how projoint corrects measurement error bias</b></summary>

Traditional conjoint analyses assume perfect reliability in respondent choices. However, respondents often make mistakes or respond inconsistently. **projoint** introduces a correction for this measurement error:

- It estimates an Inter-Rater Reliability (IRR) metric based on repeated tasks.

- It adjusts Marginal Means (MMs) and Average Marginal Component Effects (AMCEs) accordingly.

- The correction ensures more accurate inference, especially when response instability is nontrivial.

[Learn more about the correction methodology](https://yhoriuchi.github.io/projoint/articles/correct.html)

</details>

---

## 🔧 Installation

<details>

```r
# Install the development version from GitHub
devtools::install_github("yhoriuchi/projoint")
```

</details>

---

## 🚀 Quick Start

<details>

<summary style="font-size: 18px;"><b>Design</b> your survey correctly</summary>

- Begin with the [Projoint Survey Designer](https://projoint.aaronrkaufman.com/) and export surveys formatted for Qualtrics.

- Follow the [step-by-step guide](https://yhoriuchi.github.io/projoint/articles/design.html) to learn how to set up your Qualtrics survey.

</details>

<details>

<summary style="font-size: 18px;"><b>Read</b> the results into R</summary>

- Load your **survey responses** into R:

```r
library(projoint)
dat <- read_Qualtrics("your_file.csv")
```

- Follow the [step-by-step guide](https://yhoriuchi.github.io/projoint/articles/read.html) to learn how to read survey data from Qualtrics for conjoint analysis.

</details>

<details>


<summary style="font-size: 18px;"><b>Wrangle</b> the data into structured form</summary>

- Prepare the data for analysis:

```r
dat <- reshape_projoint(
  .dataframe = dat,
  .outcomes = c(paste0("choice", 1:8), "choice1_repeated_flipped")
)
```

- Follow the [step-by-step guide](https://yhoriuchi.github.io/projoint/articles/wrangle.html) to learn how to read and reshape data for conjoint analysis.

</details>

<details>


<summary style="font-size: 18px;"><b>Analyze</b> with automatic measurement error correction</summary>

- Estimate Marginal Means (MMs) or Average Marginal Component Effects (AMCEs) with correction for measurement error:

```r
output <- projoint(dat)
print(output)
summary(output)
```

- Follow the [step-by-step guide](https://yhoriuchi.github.io/projoint/articles/analyze.html) to learn how to estimate and correct marginal means (MMs) or average marginal component effects (AMCEs), including predicting IRR if necessary.

</details>

<details>


<summary style="font-size: 18px;"><b>Visualize</b> your results</summary>

- Visualize your results easily:

```r
plot(output)
```

- Follow the [step-by-step guide](https://yhoriuchi.github.io/projoint/articles/visualize.html) to learn how to visualize the marginal means (MMs) or average marginal component effects (AMCEs).

</details>

<details>


<summary style="font-size: 18px;"><b>Explore</b> and compare further</summary>

- Estimate additional quantities of interest and explore subgroup comparisons using choice-level analysis.

- Follow the [step-by-step guide](https://yhoriuchi.github.io/projoint/articles/explore.html) to learn how to estimate under-investigated quantities of interest and compare subgroups using choice-level analysis.
</details>

---

## 📘 Methodological Background

<details>


The framework and methods behind **projoint** are detailed in:

> Katherine Clayton, Yusaku Horiuchi, Aaron R. Kaufman, Gary King, and Mayya Komisarchik. Forthcoming. *“Correcting Measurement Error Bias in Conjoint Survey Experiments.”* _American Journal of Political Science_. Available at https://tinyurl.com/24btw3dq

[👉 Download BibTeX Reference](doc/projoint_citation.bib)

</details>
---

## 📋 Notes & Limitations

<details>


- Supports **binary forced-choice** outcomes only (for now).

- Package is under **active development**.

</details>

## 📦 Upcoming Features

<details>


- Weighted estimation for features and respondents

- Support for non-binary outcomes (ratings, rankings)

- Advanced visualization for choice-level effects

</details>

---

<p align="center" style="font-size: 18px;">✨ Thank you for using projoint! ✨</p>


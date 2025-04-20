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

**projoint** is a general-purpose R package for conjoint analysis. 

- Produces **more reliable estimates** of key quantities of interest by **correcting measurement error bias**.
- Encourages users to undertake **choice-level** analysis alongside traditional profile-level analysis.

---

## 🔧 Installation

```r
# Install the development version from GitHub
devtools::install_github("yhoriuchi/projoint")
```

---

## 🚀 Quick Start

<details>
<summary style="font-size: 18px;"><b>Design</b> your survey correctly</summary>

Begin with the [Projoint Survey Designer](https://projoint.aaronrkaufman.com/) and export surveys formatted for Qualtrics.

</details>
<details>

<summary style="font-size: 18px;"><b>Read</b> the results into R</summary>

Load your **survey responses** into R:

```r
library(projoint)
dat <- read_Qualtrics("your_file.csv")
```
</details>

<details>
<summary style="font-size: 18px;"><b>Wrangle</b> the data into structured form</summary>

Prepare the data for analysis:

```r
dat <- reshape_projoint(
  .dataframe = dat,
  .outcomes = c(paste0("choice", 1:8), "choice1_repeated_flipped")
)
```
</details>

<details>
<summary style="font-size: 18px;"><b>Analyze</b> with automatic measurement error correction</summary>

Estimate Marginal Means (MMs) or Average Marginal Component Effects (AMCEs) with correction for measurement error:

```r
output <- projoint(dat)
print(output)
summary(output)
```
</details>

<details>
<summary style="font-size: 18px;"><b>Visualize</b> your results</summary>

Visualize your results easily:

```r
plot(output)
```
</details>

---

## 📘 Methodological Background

The framework and methods behind **projoint** are detailed in:

> **Clayton, Horiuchi, Kaufman, King, Komisarchik (Forthcoming).**  
> *Correcting Measurement Error Bias in Conjoint Survey Experiments.*  
> _Forthcoming, American Journal of Political Science._
> [Pre-Print Available](https://gking.harvard.edu/conjointE)

[👉 Download BibTeX Reference](doc/projoint_citation.bib)

---

## 📋 Notes & Limitations

- Supports **binary forced-choice** outcomes only (for now).
- Package is under **active development**.

## 📦 Upcoming Features

- Weighted estimation for features and respondents
- Support for non-binary outcomes (ratings, rankings)
- Advanced visualization for choice-level effects


---

<p align="center" style="font-size: 18px;">✨ Thank you for using projoint! ✨</p>


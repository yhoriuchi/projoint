<div align="center">

<img src="man/figures/projoint.png" width="150" height="150" />

[![R-CMD-check](https://github.com/yhoriuchi/projoint/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yhoriuchi/projoint/actions/workflows/R-CMD-check.yaml)
[![CRAN Status](https://www.r-pkg.org/badges/version/projoint)](https://CRAN.R-project.org/package=projoint)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/projoint)](https://cran.r-project.org/package=projoint)

### 🛠️ The One-Stop Conjoint Shop: **projoint**

</div>

---

Conjoint survey designs are spreading across the social sciences due to their unusual capacity to estimate many quantities of interest from a single randomized experiment. **Projoint is general-purpose software for the design, implementation, and analysis of conjoint surveys.** Its easy-to-use components include a drag-and-drop web-based application for survey design and an R package for analysis.

We created this software for our <a href="https://gking.harvard.edu/conjointE" target="_blank" class="external-link"><em>American Journal of Political Science</em> article</a> that shows how to **correct measurement error** in conjoint surveys,<sup><a href="https://yhoriuchi.github.io/projoint/articles/correct.html" target="_blank" style="text-decoration: none;">&#9432;</a></sup> and so Projoint makes that easy too. This correction is essential because conjoint’s ability to mirror complicated real-world choices, explicitly about survey respondents’ **choices between two options**,<sup><a href="https://yhoriuchi.github.io/projoint/articles/faq.html#what-is-the-history-of-conjoint-analysis-what-is-the-difference-between-profile-level-and-choice-level-data" target="_blank" style="text-decoration: none;">&#9432;</a></sup> often generates substantial measurement error and, without corrections, can lead to substantial bias.

We intend for Projoint to be the easiest and fastest way to correctly field and analyze conjoint surveys. Take the 6 steps below and you should be all set.

---

## 🚀 Tutorials

<!-- =========================
     1. Install
     ========================= -->

<details style="margin-left: 25px; margin-bottom: -10px">
<summary style="font-size: 18px;"><b>1. Install</b> the development version from GitHub</summary>
Open R (or <a href="https://www.r-project.org/" target="_blank">install R</a> if you do not have it), and run the following command in your coding environment.
```r
devtools::install_github("yhoriuchi/projoint")
```
</details>

<!-- =========================
     2. Design
     ========================= -->

<details style="margin-left: 25px; margin-bottom: -10px">
<summary style="font-size: 18px;"><b>2. Design</b> your survey</summary>
Online surveys are frequently written with an online software called Qualtrics. Using our web tool, called the <a href="https://projoint.aaronrkaufman.com/" target="_blank" class="external-link">Projoint Survey Designer</a>, you don't need to learn how to write a survey in Qualtrics.  

* Use the <a href="https://projoint.aaronrkaufman.com/" target="_blank" class="external-link">Projoint Survey Designer</a> and export surveys formatted for Qualtrics.  
* Follow the <a href="https://yhoriuchi.github.io/projoint/articles/design.html" target="_blank">step-by-step guide</a> to learn how to set up your Qualtrics survey.  
</details>

<!-- =========================
     3. Field
     ========================= -->

<details style="margin-left: 25px; margin-bottom: -10px">
<summary style="font-size: 18px;"><b>3. Field</b> your survey</summary>

* Using the .QSF file export from the Projoint Survey Designer, load your survey into Qualtrics.
  * Log into your Qualtrics account.
  * Click "Create a new project"
  * Under "From scratch" select "Survey" and then "Get started"
  * Enter a name and under "How do you want to start your survey" select "Import a QSF file"
  * Click "Choose file" and select your .QSF file.
  * Click "Create project"
* You are free to field your Qualtrics survey through online vendors.
* When you are done fielding your survey, you will now need to export your data from Qualtrics to R.
  * Click “Download Data”.
  * Choose CSV format.
  * Critically, select “Use choice text” rather than coded values.
</details>

<!-- =========================
     4. Read
     ========================= -->

<details style="margin-left: 25px; margin-bottom: -10px">
<summary style="font-size: 18px;"><b>4. Read</b> the results into R and <b>wrangle</b> your data into structured form</summary>

* Load your <strong>survey responses</strong> into R:
```r
library(projoint)
dat <- read_Qualtrics("your_file.csv")
```

* Prepare the data for analysis:
```r
dat <- reshape_projoint(
  .dataframe = dat,
  .outcomes = c(paste0("choice", 1:8), "choice1_repeated_flipped")
)
```

* Follow the <a href="https://yhoriuchi.github.io/projoint/articles/read.html" target="_blank">step-by-step guide</a> to learn how to read and reshape data for conjoint analysis.
</details>

<!-- =========================
     5. Analyze
     ========================= -->

<details style="margin-left: 25px; margin-bottom: -10px">
<summary style="font-size: 18px;"><b>5. Analyze</b> and <b>visualize</b> important <b>Quantities of Interest</b></summary>

* Estimate Marginal Means (MMs) or Average Marginal Component Effects (AMCEs) with correction for measurement error:

> **Note:** The following example illustrates a `profile_level` analysis.  This approach is common in social science and useful as an initial diagnostic, but we encourage researchers to consider `choice_level` analysis (the default for `.structure`). See <a href="https://yhoriuchi.github.io/projoint/articles/structure.html" target="_blank">Choice-Level Analysis</a>. Detailed steps of analysis appear in the <a href="https://yhoriuchi.github.io/projoint/articles/analyze.html" target="_blank">step-by-step guide</a>.

```r
output <- projoint(out1_arranged, .structure = "profile_level")
print(output)
summary(output)
```

* Visualize your results easily:
```r
plot(output)
```
* Estimate additional quantities of interest and explore subgroup comparisons using choice-level analysis.
* Follow the <a href="https://yhoriuchi.github.io/projoint/articles/analyze.html" target="_blank">step-by-step guide</a> to learn how to:
  * Estimate and correct marginal means (MMs) or average marginal component effects (AMCEs), including predicting IRR if necessary.
  * Visualize the marginal means (MMs) or average marginal component effects (AMCEs).

</details>

<!-- =========================
     6. Explore
     ========================= -->

<details style="margin-left: 25px; margin-bottom: -10px">
<summary style="font-size: 18px;"><b>6. Explore</b> additional quantities and compare subgroups</summary>

* Go beyond standard profile-level summaries.  
* Use **choice-level analysis** to:
  - Directly compare trade-offs (e.g., low housing cost vs. low crime).  
  - Collapse multiple levels (e.g., city vs. suburban preferences).  
  - Estimate subgroup differences (e.g., Democrats vs. Republicans).  
* See detailed examples in the <a href="https://yhoriuchi.github.io/projoint/articles/explore.html" target="_blank">Explore and Compare Further</a> vignette.

</details>


---

## 📦 Upcoming Features

<details style="margin-left: 25px; margin-bottom: 5px">
- Weighted estimation for features and respondents
- Support for non-binary outcomes (ratings, rankings)
</details>

<details style="margin-left: 25px; margin-bottom: 5px">
    <summary>Contact</summary>
For comments and suggestions, please open an issue on the <a href="https://github.com/yhoriuchi/projoint/issues" target="_blank">GitHub repository</a>.
</details>

---

<p align="center" style="font-size: 18px;">✨ Thank you for using projoint! ✨</p>


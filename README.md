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

Conjoint survey designs are spreading across the social sciences due to their unusual capacity to estimate many causal effects from a single randomized experiment. Unfortunately, by their ability to mirror complicated real-world choices, these designs often generate substantial **measurement error** and thus **bias**.

**projoint** is a general-purpose R package for conjoint analysis. It produces **more reliable estimates** of the quantities of interest based on questions explicitly about survey respondents’ **choices between two options**.<sup><a href="articles/faq.html#what-is-the-history-of-conjoint-analysis-what-is-the-difference-between-profile-level-and-choice-level-data" target="_blank" style="text-decoration: none;">&#9432;</a></sup>

This method can be used not only by researchers at the design stage but also by those already analyzing the data. As we explain, everything necessary to correct the bias in an application can be estimated via a slight modification of the standard conjoint design, a separate survey run afterward, or sometimes without new data collection at all.

<details>
    <summary>Read our <a href="https://gking.harvard.edu/conjointE" target="_blank" class="external-link">accompanying paper</a> to learn more about our method</summary>
- **Clayton, Horiuchi, Kaufman, King, Komisarchik (Forthcoming).** “Correcting Measurement Error Bias in Conjoint Survey Experiments.”<br><em>Forthcoming, American Journal of Political Science.</em><br><a href="https://gking.harvard.edu/conjointE" target="_blank" class="external-link">Pre-Print Available</a>
- <a href="doc/projoint_citation.bib">👉 Download BibTeX Reference</a>
</details>

---

## 🚀 Tutorials

<details style="margin-left: 25px; margin-bottom: -10px">
<summary style="font-size: 18px;">**1. Install** the development version from GitHub</summary>
Open R (or <a href="https://www.r-project.org/" target="_blank">install R</a> if you do not have it), and run the following command in your coding environment.
```r
devtools::install_github("yhoriuchi/projoint")
```
</details>

<details style="margin-left: 25px; margin-bottom: -10px">
<summary style="font-size: 18px;">**2. Design** your survey</summary>
Online surveys are frequently written with an online software called Qualtrics. Using our web tool, called the <a href="https://projoint.aaronrkaufman.com/" target="_blank" class="external-link">Projoint Survey Designer</a>, you don't need to learn how to write a survey in Qualtrics.  

* Use the <a href="https://projoint.aaronrkaufman.com/" target="_blank" class="external-link">Projoint Survey Designer</a> and export surveys formatted for Qualtrics.  
* Follow the <a href="articles/design.html" target="_blank">step-by-step guide</a> to learn how to set up your Qualtrics survey.  
</details>

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

* Follow the <a href="articles/read.html" target="_blank">step-by-step guide</a> to learn how to read and reshape data for conjoint analysis.
</details>

<details style="margin-left: 25px; margin-bottom: -10px">
<summary style="font-size: 18px;"><b>5. Analyze</b> and <b>visualize</b> important <b>Quantities of Interest</b></summary>

* Estimate Marginal Means (MMs) or Average Marginal Component Effects (AMCEs) with correction for measurement error:

> **Note:** The following example illustrates a *profile_level* analysis.  This approach is common in social science and useful as an initial diagnostic, but we encourage researchers to consider *choice_level* analysis (the default for `.structure`). See <a href="articles/structure.html" target="_blank">Choice-Level Analysis</a>. Detailed steps of analysis appear in the <a href="articles/analyze.html" target="_blank">Step-by-Step Guide</a>.

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
* Follow the <a href="articles/analyze.html" target="_blank">step-by-step guide</a> to learn how to:
  * Estimate and correct marginal means (MMs) or average marginal component effects (AMCEs), including predicting IRR if necessary.
  * Visualize the marginal means (MMs) or average marginal component effects (AMCEs).
  * Estimate under-investigated quantities of interest and compare subgroups using choice-level analysis.
</details>

---

## 📦 Upcoming Features

<details style="margin-left: 25px; margin-bottom: 5px">
- Weighted estimation for features and respondents
- Support for non-binary outcomes (ratings, rankings)
</details>

<details style="margin-left: 25px; margin-bottom: 5px">
    <summary>Contact</summary>
For comments and suggestions, please open an issue on the <a href="https://github.com/yhoriuchi/projoint/issues" target="_blank">Github repository</a>.
</details>

---

<p align="center" style="font-size: 18px;">✨ Thank you for using projoint! ✨</p>


# <img src="man/figures/projoint.png" align="center" width="150" height="150" />

# projoint

## The One-Stop Conjoint Shop

---

**projoint** is a package for more general, more straightforward, and more creative conjoint analysis. It estimates---either *profile-level* or *choice-level*---Marginal Means (MMs) and Average Marginal Component Effects (AMCEs) based on a conjoint survey experiment. It produces more reliable estimates after correcting measurement error bias and other problems known in the literature (e.g., having the same levels for the attribute of interest). Furthermore, it presents a more general framework so that researchers can answer a range of substantively important questions more straightforwardly.

Some notes:

* The current version assumes that the outcome variable is a binary forced choice.

* This package is still under construction. Forthcoming features include the following:
  + Add an article explaining how to set up a Qualtrics survey
  + Add a function for subgroup comparisons.
  + Allow researchers to use weights for features and for respondents.
  + Allow researchers to use other outcome variables, such as rating.

### Installation

You can install the development version of **projoint** from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("yhoriuchi/projoint")
```

### Why **projoint**?

There are some excellent R packages for conjoint analysis, including [cjoint](https://cran.r-project.org/web/packages/cjoint/) (Version: 2.1.0, Published: 2018-10-19) and [cregg](https://thomasleeper.com/cregg/) (Version: 0.4.0, Published: 2020-06-28). But there are some important advantages of using our package:

* Most importantly, unlike the other packages, researchers can fix the measurement error bias ([Clayton et al., working paper](https://gking.harvard.edu/conjointE)).

* The [cjoint](https://cran.r-project.org/web/packages/cjoint/) and [cregg](https://thomasleeper.com/cregg/) packages assume that the unit of analysis is each profile even when researchers design a binary choice experiment. We define more straightforward *choice-level* MMs and AMCEs and allow users to estimate them ([Clayton et al., working paper](https://gking.harvard.edu/conjointE)). 

* The profile-level MMs ([cregg](https://thomasleeper.com/cregg/)) are always attenuated toward 0.5 if a conjoint design includes "ties" for the attribute of interest ([Gander 2021](https://doi.org/10.1017/pan.2021.41)). Consider, for example, that the attribute of your interest is a candidate's party, which has two levels, {Republican, Democrat}. If these levels are randomly assigned to each profile, 50% of profile-pairs include ties. Therefore, even if strongly partisan respondents only care about a candidate's party and always prefer one party to another, they have to choose one of the two tied profiles randomly. Therefore, the (profile-level) marginal mean of Republican for Republican supporters and Democrat for Democrat supporters is 0.75 (= 1 * 0.5 + 0.5 * 0.5). We remove these ties to calculate MMs.

* The profile-level AMCEs ([cjoint](https://cran.r-project.org/web/packages/cjoint/)) may produce counter-intuitive estimates ([Abramson, Kocak, Magazinni, and Strezhnev, working paper](https://osf.io/preprints/socarxiv/xjre9/)). For example, suppose that the attribute of your interest is a candidate's race, which has three levels, {white, Black, and Asian American}. Specifically, you are interested in whether white respondents prefer a Black or Asian American candidate. In the case of head-to-head comparison, suppose that 75% of white respondents choose a Black candidate rather than an Asian American candidate. (This is an example of choice-level marginal mean.) But the profile-level AMCE of choosing a Black candidate using an Asian American candidate may be larger or smaller than 75%, surprisingly. This is because the AMCE's calculation includes *indirect* comparisons of other combinations. Under a set of assumptions, profile-level AMCEs can still be researchers' substantive quantities of interest ([Bansak, Hainmueller, Hopkins, and Yamamoto, forthcoming](https://doi.org/10.1017/pan.2022.16)) but they should not be the only quantities of interest for applied researchers. Therefore, we provide an option to estimate alternative and more intuitive quantities of interest, choice-level MMs and AMCEs.

* Researchers often assume that each profile within a profile-pair needs to be independent. But this is not a critical assumption for conjoint analysis. In fact, researchers should consider a variety of research questions with cross-attribute constraints, such as a profile pair with one incumbent and one challenger, a profile pair with the sum of values being one (e.g., the expected chance of winning a seat between two candidates). Our framework and this software allow researchers to examine a variety of questions in more flexible and innovative manners.

* Last but not least, our package provides the easiest possible way to re-label and re-order attributes and levels.

### Relevant Article

For our framework and methods, please read and cite the following article:

* Katherine Clayton, Yusaku Horiuchi, Aaron R. Kaufman, Gary King, and Mayya Komisarchik. “Correcting Measurement Error Bias in Conjoint Survey Experiments”. Working Paper. [[Paper](https://gking.harvard.edu/sites/scholar.harvard.edu/files/gking/files/conerr.pdf)] [[Supplementary Appendix](https://gking.harvard.edu/sites/scholar.harvard.edu/files/gking/files/conerr-supp.pdf)]
  + **Abstract:** Conjoint survey designs are spreading across the social sciences due to their unusual capacity to estimate many causal effects from a single randomized experiment. Unfortunately, by their ability to mirror complicated real-world choices, these designs often generate substantial measurement errors and thus bias. We replicate both the data collection and analysis from eight prominent conjoint studies, all of which closely reproduce published results, and show that a large proportion of observed variation in answers to conjoint questions is effectively random noise. We then discover a common empirical pattern in how measurement error appears in conjoint studies and, with it, introduce an easy-to-use statistical method to correct the bias.

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](https://yhoriuchi.github.io/projoint/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

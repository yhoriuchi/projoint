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
  + Allow users to use weights for attributes and for respondents.
  + Allow users to use other outcome variables, such as rating.

### Installation

You can install the development version of **projoint** from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("yhoriuchi/projoint")
```

### Why **projoint**?

There are some excellent R packages for conjoint analysis, including [cjoint](https://cran.r-project.org/web/packages/cjoint/) (Version: 2.1.0, Published: 2018-10-19) and [cregg](https://thomasleeper.com/cregg/packages)(Version: 0.4.0, Published: 2020-06-28). But there are some importance advantages of using our packages:

* Unlike the other packages, researchers can fix the measurement error bias ([Clayton et al, working paper](https://gking.harvard.edu/conjointE)).

* The existing packages assume that the unit of analysis is each profile even when researchers design a binary choice experiment. We define more straightforward and more intuitive *choice-level* MMs and AMCEs and allow users to estimate them ([Clayton et al, working paper](https://gking.harvard.edu/conjointE)). 

* The profile-level MMs are known to be attenuated toward 0.5 if a conjoint design includes "ties" for the attribute of interest ([Gander 2021](https://doi.org/10.1017/pan.2021.41)). We remove these ties to calculate MMs.

* The profile-level AMCEs include "irrelevant attributes" in the process of calculation. For this reason, the estimates tend to be counter-intuitive ([Abramson, Kocak, Magazinni, and Strezhnev, working paper](https://osf.io/preprints/socarxiv/xjre9/)). We avoid this problem by focusing on choice-level quantities of interest.

* Last, but not least, our package provide the easiest possible way to re-label and re-order attributes and levels.

### Relevant Articles

#### Methods

For our framework and methods, please read and cite the following article:

* Katherine Clayton, Yusaku Horiuchi, Aaron R. Kaufman, Gary King, and Mayya Komisarchik. “Correcting Measurement Error Bias in Conjoint Survey Experiments”. Working Paper. [[Paper](https://gking.harvard.edu/sites/scholar.harvard.edu/files/gking/files/conerr.pdf)] [[Supplementary Appendix](https://gking.harvard.edu/sites/scholar.harvard.edu/files/gking/files/conerr-supp.pdf)]
  + **Abstract:** Conjoint survey designs are spreading across the social sciences due to their unusual capacity to estimate many causal effects from a single randomized experiment. Unfortunately, by their ability to mirror complicated real-world choices, these designs often generate substantial measurement error and thus bias. We replicate both the data collection and analysis from eight prominent conjoint studies, all of which closely reproduce published results, and show that a large proportion of observed variation in answers to conjoint questions is effectively random noise. We then discover a common empirical pattern in how measurement error appears in conjoint studies and, with it, introduce an easy-to-use statistical method to correct the bias.

#### Applications

Studies that apply our framework and methods include the following:

* John Cho, Mia Costa, and Yusaku Horiuchi. “Trade-Offs in Asian American Representation: Choosing Between Co-Ethnicity, Pan-Ethnicity, and Co-Partisanship." Working Paper.
  + **Abstract:** How do minority voters prioritize shared race, ethnicity, and partisanship for who represents them in Congress? Asian Americans, as the fastest-growing racial group in the United States, present an important case study to analyze trade-offs between descriptive and partisan representation. In addition to considerations about partisanship, Asian Americans face trade-offs between candidates of the same national origin and of the same race. We find that when asked outright about collective presence in the legislature, Asian Americans prioritize increased descriptive representation over partisan representation. However, when choosing between candidates for dyadic representation, Asian Americans often trade off descriptive representation for the sake of shared partisanship. Voters are only willing to cross party lines for a co-ethnic representative, but not for pan-ethnics. The findings have important implications for how ethnic minority voters make decisions about political representation and contribute to understandings about a heavily understudied and heterogeneous group.





# Test an example without having the repeated task

# Initial settings --------------------------------------------------------

library(projoint)
library(tidyverse)

# Load data ---------------------------------------------------------------

outcomes <- paste0("choice", seq(from = 1, to = 8, by = 1))
d <- reshape_projoint(.dataframe = exampleData3, # Data with non repeated task 
                      .outcomes = outcomes,
                      .choice_labels = c("A", "B"),
                      .alphabet = "K", 
                      .idvar = "ResponseId", 
                      .repeated = FALSE)

# Estimate ----------------------------------------------------------------

mm <- projoint(d, 
               .structure = "profile_level", 
               .estimand = "mm", 
               .irr = 0.656)


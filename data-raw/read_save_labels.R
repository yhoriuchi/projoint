# Ref: https://github.com/mvuorre/exampleRPackage/blob/master/data-raw/preprocess.R

library(tidyverse)
library(projoint)

# Data with the flipped repeated tasks

outcomes <- paste0("choice", seq(from = 1, to = 8, by = 1))
outcomes <- c(outcomes, "choice1_repeated_flipped")
out1 <- reshape_projoint(.dataframe = exampleData1, 
                         .idvar = "ResponseId", 
                         .outcomes = outcomes,
                         .outcomes_ids = c("A", "B"),
                         .alphabet = "K", 
                         .repeated = TRUE,
                         .flipped = TRUE, 
                         .fill = TRUE)

save_labels(out1, "data-raw/labels_original.csv")
out1_arranged <- read_labels(out1, "data-raw/labels_arranged.csv")

# Apply pre-processing...
usethis::use_data(out1_arranged, overwrite = TRUE)

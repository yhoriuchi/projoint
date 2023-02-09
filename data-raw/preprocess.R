# Ref: https://github.com/mvuorre/exampleRPackage/blob/master/data-raw/preprocess.R

# Load raw data from .csv file

library(tidyverse)

source("R/read_Qualtrics.R")
exampleData1 <- read_Qualtrics("data-raw/Qualtrics_data_with_a_repeated_task.csv") 
exampleData2 <- read_Qualtrics("data-raw/Qualtrics_data_without_a_repeated_task.csv")

# Apply preprocessing...
# Save the cleaned data in the required R package location
usethis::use_data(exampleData1, overwrite = TRUE)
usethis::use_data(exampleData2, overwrite = TRUE)


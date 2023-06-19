# Ref: https://github.com/mvuorre/exampleRPackage/blob/master/data-raw/preprocess.R

# Load raw data from .csv file

library(tidyverse)

source("R/read_Qualtrics.R")

d <- read_Qualtrics("data-raw/qualtrics_housing_june2021.csv") %>% 
  filter(Finished == TRUE, 
         Q1.2 == "Yes",
         Q2.4_1 == "Every day") %>% 
  mutate(choice1 = ifelse(!is.na(Q4.1), Q4.1, Q5.1),
         choice2 = ifelse(!is.na(Q6.1), Q6.1, Q7.1),
         choice3 = ifelse(!is.na(Q8.1), Q8.1, Q9.1),
         choice4 = ifelse(!is.na(Q10.1), Q10.1, Q11.1),
         choice5 = ifelse(!is.na(Q12.1), Q12.1, Q13.1),
         choice6 = ifelse(!is.na(Q14.1), Q14.1, Q15.1),
         choice7 = ifelse(!is.na(Q16.1), Q16.1, Q17.1),
         choice8 = ifelse(!is.na(Q18.1), Q18.1, Q19.1)) %>% 
  select(ResponseId, 
         "gender" = Q2.2,
         "age" = Q2.3,
         choice1:choice8,
         "choice1_repeated_notflipped" = Q20.1,
         "choice1_repeated_flipped" = Q21.1, 
         contains("K-"))

# data with the flipped repeated tasks
exampleData1 <- d %>% 
  filter(!is.na(choice1_repeated_flipped)) %>% 
  select(-choice1_repeated_notflipped)

# data with the non-flipped repeated tasks
exampleData2 <- d %>% 
  filter(!is.na(choice1_repeated_notflipped)) %>% 
  select(-choice1_repeated_flipped)

# data without the repeated tasks
exampleData3 <- exampleData1 %>% 
  select(-contains("repeated"))


# Apply pre-processing...
# Save the cleaned data in the required R package location
usethis::use_data(exampleData1, overwrite = TRUE)
usethis::use_data(exampleData2, overwrite = TRUE)
usethis::use_data(exampleData3, overwrite = TRUE)


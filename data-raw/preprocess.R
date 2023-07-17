# Ref: https://github.com/mvuorre/exampleRPackage/blob/master/data-raw/preprocess.R

# Load raw data from .csv file

library(tidyverse)

source("R/read_Qualtrics.R")

# Qualtrics survey name: CHKKK: Mummolo and Nall full replication W1 (Lucid - August 2021)

d <- read_Qualtrics("data-raw/mummolo_nall_replication.csv") %>% 
  filter(Finished == TRUE,
         Q1.2 == "Yes",
         Q3.1 == "Assemble") %>% 
  mutate(choice1_repeated_notflipped = case_when(!is.na(Q6.1) ~ Q6.1,
                                                 !is.na(Q5.1) & Q5.1 == "Community A" ~ "Community B",
                                                 !is.na(Q5.1) & Q5.1 == "Community B" ~ "Community A"),
         choice1_repeated_flipped = case_when(choice1_repeated_notflipped == "Community A" ~ "Community B",
                                              choice1_repeated_notflipped == "Community B" ~ "Community A"))

# check
d %>% count(Q5.1, Q6.1, choice1_repeated_notflipped, choice1_repeated_flipped)

d2 <- d %>% 
  mutate(choice1 = Q4.2,
         choice2 = Q4.3,
         choice3 = Q4.4,
         choice4 = Q4.5,
         choice5 = Q4.6,
         choice6 = Q4.7,
         choice7 = Q4.8,
         choice8 = Q4.9,
         "gender" = Q2.2,
         "age" = Q2.3, 
         "race" = Q2.4,
         "party_1" = Q2.5,
         "party_2" = Q2.6,
         "party_3" = Q2.7,
         "party_4" = Q2.8,
         "ideology" = Q2.9,
         "honesty" = Q7.2,
         "comments" = Q7.3) %>% 
  select(ResponseId, 
         choice1:choice8,
         choice1_repeated_notflipped, 
         choice1_repeated_flipped,
         gender:comments, contains("K-"))

# data with the flipped repeated tasks
exampleData1 <- d2 %>% 
  filter(!is.na(choice1_repeated_flipped)) %>% 
  select(-choice1_repeated_notflipped)

write.csv(exampleData1, file = "data-raw/mummolo_nall_replication_cleaned.csv",
          row.names = FALSE)

# data with the non-flipped repeated tasks
exampleData2 <- d2 %>% 
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


library(tidyverse)
library(projoint)

# make data using reshape_projoint()
df <-  reshape_projoint(
  .dataframe = exampleData1, 
  .idvar = "ResponseId", 
  .outcomes = c(paste0("choice", 1:8), "choice1_repeated_flipped"),
  .outcomes_ids = c("A", "B"),
  .alphabet = "K", 
  .repeated = TRUE,
  .flipped = TRUE, 
  .covariates = c("race", "ideology"),
  .fill = FALSE)

# wrangle data to make a labelled tibble
exampleData1_labelled_tibble <- df@data %>% 
  group_by(id, task, profile) %>% 
  pivot_longer(cols = 4:10, 
               names_to = "attribute_id", 
               values_to = "level_id") %>% 
  ungroup() %>% 
  mutate(level_id = as.character(level_id)) %>% 
  left_join(df@labels %>% select(level_id, level), by = "level_id") %>% 
  left_join(df@labels %>% select(attribute_id, attribute) %>% distinct(), by = "attribute_id") %>% 
  select(-attribute_id, -level_id) %>% 
  pivot_wider(id_cols = c(id, task, profile, selected, selected_repeated, race, ideology), 
              names_from = attribute, 
              values_from = level) %>% 
  relocate(race, ideology, .after = ncol(.))

# Apply pre-processing...
usethis::use_data(exampleData1_labelled_tibble, overwrite = TRUE)

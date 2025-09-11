library(tidyverse)
library(projoint)

data("exampleData1_labelled_tibble")

out <- make_projoint_data(
  .dataframe = exampleData1_labelled_tibble,
  .attribute_vars = c(
    "School Quality", "Violent Crime Rate (Vs National Rate)",
    "Racial Composition", "Housing Cost",
    "Presidential Vote (2020)", "Total Daily Driving Time for Commuting and Errands",
    "Type of Place"
  ),
  .id_var = "id",
  .task_var = "task",
  .profile_var = "profile",
  .selected_var = "selected",
  .selected_repeated_var = "selected_repeated",
  .fill = TRUE
)


# out[["labels"]][["level_id"]] <- gsub("lev", "level", out[["labels"]][["level_id"]])
# out[["data"]][["att1"]] <- gsub("lev", "level", out[["data"]][["att1"]])
# out[["data"]][["att2"]] <- gsub("lev", "level", out[["data"]][["att2"]])
# out[["data"]][["att3"]] <- gsub("lev", "level", out[["data"]][["att3"]])
# out[["data"]][["att4"]] <- gsub("lev", "level", out[["data"]][["att4"]])
# out[["data"]][["att5"]] <- gsub("lev", "level", out[["data"]][["att5"]])
# out[["data"]][["att6"]] <- gsub("lev", "level", out[["data"]][["att6"]])
# out[["data"]][["att7"]] <- gsub("lev", "level", out[["data"]][["att7"]])

results <- projoint(out, .structure = "profile_level", .estimand = "amce")
plot(results)
summary(results)


data("exampleData1")

outcomes <- paste0("choice", 1:8)
outcomes1 <- c(outcomes, "choice1_repeated_flipped")

out1 <- reshape_projoint(
  .dataframe = exampleData1,
  .outcomes = outcomes1,
  .choice_labels = c("A", "B"),
  .alphabet = "K",
  .idvar = "ResponseId",
  .repeated = TRUE,
  .flipped = TRUE
)

out1$labels
out1$data


# Check manually ----------------------------------------------------------
# 
# .dataframe = exampleData1_labelled_tibble
# .attribute_vars = c(
#   "School Quality", "Violent Crime Rate (Vs National Rate)",
#   "Racial Composition", "Housing Cost",
#   "Presidential Vote (2020)", "Total Daily Driving Time for Commuting and Errands",
#   "Type of Place"
# )
# .id_var = "id"
# .task_var = "task"
# .profile_var = "profile"
# .selected_var = "selected"
# .selected_repeated_var = "selected_repeated"
# .fill = TRUE



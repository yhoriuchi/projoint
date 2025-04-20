# Test example on the wrangle page

# 2.1 ---------------------------------------------------------------------

library(projoint)

# 2.2 ---------------------------------------------------------------------

outcomes <- paste0("choice", seq(from = 1, to = 8, by = 1))
outcomes1 <- c(outcomes, "choice1_repeated_flipped")
out1 <- reshape_projoint(.dataframe = exampleData1, 
                         .outcomes = outcomes1,
                         .choice_labels = c("A", "B"),
                         .alphabet = "K", 
                         .idvar = "ResponseId", 
                         .repeated = TRUE,
                         .flipped = TRUE, 
                         .covariates = NULL,
                         .fill = FALSE)

summary(out1)
print(out1)

outcomes2 <- c(outcomes, "choice1_repeated_notflipped")
out2 <- reshape_projoint(.dataframe = exampleData2, 
                         .outcomes = outcomes2,
                         .repeated = TRUE,
                         .flipped = FALSE)

out3 <- reshape_projoint(.dataframe = exampleData3, 
                         .outcomes = outcomes,
                         .repeated = FALSE)

# 2.3 ---------------------------------------------------------------------

fill_FALSE <- reshape_projoint(.dataframe = exampleData1, 
                               .outcomes = outcomes1,
                               .fill = FALSE)
fill_TRUE <- reshape_projoint(.dataframe = exampleData1, 
                              .outcomes = outcomes1,
                              .fill = TRUE)

selected_vars <- c("id", "task", "profile", "selected", "selected_repeated", "agree")
fill_FALSE$data[selected_vars]
fill_TRUE$data[selected_vars]
# 2.4 ---------------------------------------------------------------------

data("exampleData1_labelled_tibble", package = "projoint")

options(tibble.width = Inf)
exampleData1_labelled_tibble

attributes <- c("School Quality",
                "Violent Crime Rate (Vs National Rate)",
                "Racial Composition",
                "Housing Cost",
                "Presidential Vote (2020)",
                "Total Daily Driving Time for Commuting and Errands",
                "Type of Place")

out4 <- make_projoint_data(.dataframe = exampleData1_labelled_tibble,
                           .attribute_vars = attributes, 
                           .id_var = "id", # the default name
                           .task_var = "task", # the default name
                           .profile_var = "profile", # the default name
                           .selected_var = "selected", # the default name
                           .selected_repeated_var = "selected_repeated", # the default is NULL
                           .fill = TRUE)

print(out4)

# 2.5 ---------------------------------------------------------------------

save_labels(out1, "data-raw/labels_original.csv")
out1_arranged <- read_labels(out1, "inst/extdata/labels_arranged.csv")
mm <- projoint(out1, .estimand = "mm")
plot(mm)
mm <- projoint(out1_arranged, .estimand = "mm")
plot(mm)


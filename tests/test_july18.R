# Test

# Initial settings --------------------------------------------------------

library(tidyverse)
# devtools::install_github("yhoriuchi/projoint")
# library(projoint)

# Load functions ----------------------------------------------------------

load("data/exampleData1.rda")
load("data/exampleData2.rda")
load("data/exampleData3.rda")

source("R/projoint_data.R")
source("R/projoint_irr.R")
source("R/projoint_qoi.R")
source("R/projoint_results.R")

source("R/reshape_projoint.R")
source("R/predict_tau.R")
source("R/organize_data.R")
source("R/pj_estimate.R")
source("R/set_qoi.R")
source("R/projoint.R")

# Various ways to load data -----------------------------------------------

# Data with the flipped repeated tasks

outcomes <- paste0("choice", seq(from = 1, to = 8, by = 1))
outcomes <- c(outcomes, "choice1_repeated_flipped")
out1 <- reshape_projoint(.dataframe = exampleData1, 
                         .idvar = "ResponseId", 
                         .outcomes = outcomes,
                         .outcomes_ids = c("A", "B"),
                         .alphabet = "K", 
                         .repeated = TRUE,
                         .flipped = TRUE)

# Data with the not-flipped repeated tasks

outcomes <- paste0("choice", seq(from = 1, to = 8, by = 1))
outcomes <- c(outcomes, "choice1_repeated_notflipped")
out2 <- reshape_projoint(.dataframe = exampleData2, 
                         .idvar = "ResponseId", 
                         .outcomes = outcomes,
                         .outcomes_ids = c("A", "B"),
                         .alphabet = "K", 
                         .repeated = TRUE,
                         .flipped = FALSE)

# Data without the repeated tasks

outcomes <- paste0("choice", seq(from = 1, to = 8, by = 1))
out3 <- reshape_projoint(.dataframe = exampleData3, 
                         .idvar = "ResponseId", 
                         .outcomes = outcomes,
                         .outcomes_ids = c("A", "B"),
                         .alphabet = "K", 
                         .repeated = FALSE,
                         .flipped = NULL)

# Predict irr -------------------------------------------------------------

predicted_irr <- predict_tau(out3)
predicted_irr@irr
predicted_irr@figure

# pj_estimate() -----------------------------------------------------------
# Test 1: Profile-level MM

pj_estimate(.data = out1,
            .attribute = "att1",
            .level = "level1",
            .structure = "profile_level",
            .estimand = "mm",
            .se_method = "analytical",
            .irr = NULL,
            .remove_ties = TRUE,
            .repeated_task = TRUE)

# Test 2: Choice-level MM

pj_estimate(.data = out1,
            .attribute = "att1",
            .level = c("level1", "level2"),
            .structure = "choice_level",
            .estimand = "mm",
            .se_method = "analytical",
            .irr = NULL,
            .remove_ties = TRUE,
            .repeated_task = TRUE, 
            .ignore_position = TRUE)

pj_estimate(.data = out1,
            .attribute = "att1",
            .level = c("level1", "level2"),
            .structure = "choice_level",
            .estimand = "mm",
            .se_method = "analytical",
            .irr = NULL,
            .remove_ties = TRUE,
            .repeated_task = TRUE, 
            .ignore_position = FALSE)

# set_qoi() ---------------------------------------------------------------
# Test
qoi_1 <- set_qoi(.structure = "profile_level", 
                 .attribute = "att1", 
                 .level = "level1")

qoi_2 <- set_qoi(.structure = "choice_level", 
                 .attribute = "att1", 
                 .level = c("level1", "level3"))

# Estimate MMs ------------------------------------------------------------

mm0 <- projoint(.data = out1,
                .qoi = NULL,
                .structure = "profile_level",
                .remove_ties = TRUE,
                .repeated_task = TRUE,
                .estimand = "mm",
                .se_method = "analytical",
                .irr = NULL,
                .ignore_position = NULL,
                .n_sims = NULL,
                .n_boot = NULL)

mm1 <- projoint(.data = out1,
                .qoi = qoi_1,
                .structure = "profile_level",
                .remove_ties = TRUE,
                .repeated_task = TRUE,
                .estimand = "mm",
                .se_method = "analytical",
                .irr = NULL,
                .ignore_position = NULL,
                .n_sims = NULL,
                .n_boot = NULL)

mm2 <- projoint(.data = out1,
                .qoi = qoi_2,
                .structure = "choice_level",
                .remove_ties = TRUE,
                .repeated_task = TRUE,
                .estimand = "mm",
                .se_method = "analytical",
                .irr = NULL,
                .ignore_position = FALSE,
                .n_sims = NULL,
                .n_boot = NULL)





# # Test 4 ------------------------------------------------------------------
# # Profile-level AMCE
# 
# temp1 <- organize_data(d, "att1", "level1", "profile_level")
# temp2 <- organize_data(d, "att1", "level3", "profile_level")
# 
# data_for_irr <- bind_rows(
#   temp1$data_for_irr,
#   temp2$data_for_irr
# ) %>% 
#   distinct()
# 
# data_for_estimand <- bind_rows(
#   temp1$data_for_estimand %>% mutate(x = 0),
#   temp2$data_for_estimand %>% mutate(x = 1)
# )
# 
# d_amce1 <- list(
#   "data_for_irr" = data_for_irr,
#   "data_for_estimand" = data_for_estimand
# )
# 
# pj_estimate(d_amce1, "amce", "analytical")
# pj_estimate(d_amce1, "amce", "simulation")
# pj_estimate(d_amce1, "amce", "bootstrap")
# 
# pj_estimate(d_amce1, "amce", "analytical", .irr = 0.75)
# pj_estimate(d_amce1, "amce", "simulation", .irr = 0.75)
# pj_estimate(d_amce1, "amce", "bootstrap", .irr = 0.75)
# 
# 
# # Test 5 ------------------------------------------------------------------
# # Choice-level AMCE
# 
# temp1 <- organize_data(d, "att1", c("level1", "level1"), "choice_level")
# temp2 <- organize_data(d, "att1", c("level1", "level3"), "choice_level")
# 
# data_for_irr <- bind_rows(
#   temp1$data_for_irr,
#   temp2$data_for_irr
# ) %>% 
#   distinct()
# 
# data_for_estimand <- bind_rows(
#   temp1$data_for_estimand %>% mutate(x = 0),
#   temp2$data_for_estimand %>% mutate(x = 1)
# )
# 
# d_amce1 <- list(
#   "data_for_irr" = data_for_irr,
#   "data_for_estimand" = data_for_estimand
# )
# 
# pj_estimate(d_amce1, "amce", "analytical")
# pj_estimate(d_amce1, "amce", "simulation")
# pj_estimate(d_amce1, "amce", "bootstrap")
# 
# pj_estimate(d_amce1, "amce", "analytical", .irr = 0.75)
# pj_estimate(d_amce1, "amce", "simulation", .irr = 0.75)
# pj_estimate(d_amce1, "amce", "bootstrap", .irr = 0.75)




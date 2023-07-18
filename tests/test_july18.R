# Initial settings --------------------------------------------------------

library(tidyverse)
# devtools::install_github("yhoriuchi/projoint")
# library(projoint)

# Load functions ----------------------------------------------------------

load("data/exampleData1.rda")
load("data/exampleData2.rda")
load("data/exampleData3.rda")

source("R/projoint_data.R")
source("R/projoint_qoi.R")
source("R/projoint_irr.R")
source("R/projoint_results.R")

source("R/reshape_projoint.R")
source("R/predict_tau.R")
source("R/organize_data.R")
source("R/pj_estimate.R")
source("R/set_qoi.R")
source("R/projoint.R")

# Various ways to load data -----------------------------------------------

# Data with the flipped repeated tasks

outcomes <- str_c("choice", seq(from = 1, to = 8, by = 1))
outcomes <- c(outcomes, "choice1_repeated_flipped")
out1 <- reshape_projoint(.data = exampleData1, 
                         .idvar = "ResponseId", 
                         .outcomes = outcomes,
                         .outcomes_ids = c("A", "B"),
                         .alphabet = "K", 
                         .repeated = TRUE,
                         .flipped = TRUE)

# Data with the not-flipped repeated tasks

outcomes <- str_c("choice", seq(from = 1, to = 8, by = 1))
outcomes <- c(outcomes, "choice1_repeated_notflipped")
out2 <- reshape_projoint(.data = exampleData2, 
                         .idvar = "ResponseId", 
                         .outcomes = outcomes,
                         .outcomes_ids = c("A", "B"),
                         .alphabet = "K", 
                         .repeated = TRUE,
                         .flipped = FALSE)

# Data without the repeated tasks

outcomes <- str_c("choice", seq(from = 1, to = 8, by = 1))
out3 <- reshape_projoint(.data = exampleData3, 
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
source("R/projoint.R")

pj1 = projoint(.data = out1,
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

pj2 = projoint(.data = out1,
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

pj3 = projoint(.data = out1,
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

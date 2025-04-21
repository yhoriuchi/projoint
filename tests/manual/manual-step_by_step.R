library(projoint)

outcomes <- paste0("choice" 1:8)
outcomes <- c(outcomes "choice1_repeated_flipped")

out1_arranged <- reshape_projoint(
  .dataframe = exampleData1
  .outcomes = outcomes
  .repeated = TRUE
  .flipped = TRUE
)

print(data)

qoi_mm <- set_qoi(
  .structure = "choice_level" # default
  .att_choose = "att1" 
  .lev_choose = "level1" 
  .att_notchoose = "att1" 
  .lev_notchoose = "level3"
)

# projoint
.data <- out1_arranged
.qoi = qoi_mm
.by_var = NULL
.structure = "profile_level"
.estimand = "mm"
.se_method = "analytical"
.irr = NULL
.remove_ties = TRUE
.ignore_position = NULL
.n_sims = NULL
.n_boot = NULL
.weights_1 = NULL
.clusters_1 = NULL
.se_type_1 = "classical"
.weights_2 = NULL
.clusters_2 = NULL
.se_type_2 = "classical"

# projoint_level()


attribute_of_interest  <- .qoi$attribute_of_interest
levels_of_interest     <- .qoi$levels_of_interest

attribute_of_interest_0  <- .qoi$attribute_of_interest_0
levels_of_interest_0     <- .qoi$levels_of_interest_0

attribute_of_interest_baseline <- .qoi$attribute_of_interest_baseline
levels_of_interest_baseline     <- .qoi$levels_of_interest_baseline

attribute_of_interest_0_baseline <- .qoi$attribute_of_interest_0_baseline
levels_of_interest_0_baseline     <- .qoi$levels_of_interest_0_baseline

.att_choose = attribute_of_interest
.lev_choose = levels_of_interest
.att_notchoose = attribute_of_interest_0 
.lev_notchoose = levels_of_interest_0
.att_choose_b = attribute_of_interest_baseline 
.lev_choose_b = levels_of_interest_baseline
.att_notchoose_b = attribute_of_interest_0_baseline 
.lev_notchoose_b = levels_of_interest_0_baseline



# pj_estimate
.structure = structure
.estimand = estimand
.att_choose = attribute
.lev_choose = level
.att_notchoose = NULL 
.lev_notchoose = NULL
.att_choose_b = NULL 
.lev_choose_b = NULL
.att_notchoose_b = NULL 
.lev_notchoose_b = NULL

# organize_data
.dataframe <- .data$data

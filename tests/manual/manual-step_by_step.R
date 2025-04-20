library(projoint)

# Reshape data for conjoint analysis
# Important: Include the repeated task variable for IRR estimation
data <- reshape_projoint(
  exampleData1 
  .outcomes = c(paste0("choice" 1:8) "choice1_repeated_flipped")
)

print(data)

# projoint
.data <- data
.qoi = NULL
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
i <- 1

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

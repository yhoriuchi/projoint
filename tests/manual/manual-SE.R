library(tidyverse)
library(projoint)

data("exampleData1")

outcomes <- paste0("choice", seq(from = 1, to = 8, by = 1))
outcomes <- c(outcomes, "choice1_repeated_flipped")

reshaped_data <- reshape_projoint(
  .dataframe = exampleData1, 
  .outcomes = outcomes)

qoi <- set_qoi(
  .structure = "choice_level",
  .att_choose = "att1",
  .lev_choose = "level3",
  .att_notchoose = "att1",
  .lev_notchoose = "level1"
)

# No clustering
#  If clusters is not specified the options are "HC0", "HC1" (or "stata", the equivalent), 
# "HC2" (default), "HC3", or "classical".
projoint(reshaped_data, qoi, .auto_cluster = FALSE) |> summary() |> as.data.frame() 
projoint(reshaped_data, qoi, .se_type_2 = "HC0") |> summary() |> as.data.frame()
projoint(reshaped_data, qoi, .se_type_2 = "HC1") |> summary() |> as.data.frame()
projoint(reshaped_data, qoi, .se_type_2 = "HC2") |> summary() |> as.data.frame() # default
projoint(reshaped_data, qoi, .se_type_2 = "HC3") |> summary() |> as.data.frame()
projoint(reshaped_data, qoi, .se_type_2 = "stata") |> summary() |> as.data.frame()
projoint(reshaped_data, qoi, .se_type_2 = "none") |> summary() |> as.data.frame()


# Clustering
# If clusters is specified the options are "CR0", "CR2" (default), or "stata". 
# Can also specify "none", which may speed up estimation of the coefficients.
projoint(reshaped_data, qoi, .clusters_2 = id) |> summary() |> as.data.frame()
projoint(reshaped_data, qoi, .clusters_2 = id, .se_type_2 = "CR0") |> summary() |> as.data.frame()
projoint(reshaped_data, qoi, .clusters_2 = id, .se_type_2 = "CR2") |> summary() |> as.data.frame() # default
projoint(reshaped_data, qoi, .clusters_2 = id, .se_type_2 = "stata") |> summary() |> as.data.frame()
projoint(reshaped_data, qoi, .clusters_2 = id, .se_type_2 = "none") |> summary() |> as.data.frame()


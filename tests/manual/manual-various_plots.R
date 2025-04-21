library(tidyverse)
library(projoint)

# profile_mm   <- projoint(out1_arranged, 
#                          .structure = "profile_level") 
# plot(profile_mm)
# 
# profile_amce   <- projoint(out1_arranged, 
#                            .structure = "profile_level",
#                            .estimand = "amce") 
# plot(profile_amce)
# 
qoi <- set_qoi(
  .structure = "choice_level", 
  .att_choose = "att1", 
  .lev_choose = "level1", 
  .att_notchoose = "att1", 
  .lev_notchoose = "level3"
)

choice_mm <- projoint(out1_arranged, 
                      .qoi = qoi,
                      .structure = "choice_level")

x <- choice_mm
.type = "bar"
.estimates = "corrected"
.labels = NULL
.base_size = 12
.base_family = ""
.show_attribute = FALSE 
.xtitle = "Choice-level marginal mean" 
.remove_xaxis = FALSE 
.xlim = c(0, 1)
.title_size = 11 




plot(choice_mm)
plot(choice_mm, .type = "pointrange")

choice_mm <- projoint(out1_arranged, 
                      .qoi = qoi,
                      .structure = "choice_level")

x <- choice_mm


plot(choice_mm, .type = "pointrange")

library(tidyverse)
library(projoint)

qoi <- set_qoi(
  .structure = "choice_level", 
  .att_choose = "att1", 
  .lev_choose = "level1", 
  .att_notchoose = "att1", 
  .lev_notchoose = "level2"
)

choice_mm <- projoint(out1_arranged, 
                      .qoi = qoi,
                      .structure = "choice_level")

x <- choice_mm
.type = "pointrange"
.estimates = "corrected"
.labels = NULL
.show_attribute = TRUE
.remove_xaxis = FALSE 
.xlim = c(0, 1)
.plot.margin = unit(c(top = 1, 
                      left = 2, 
                      bottom = 1, 
                      right = 2), "cm")



plot(choice_mm, .type = "pointrange", .plot.margin = c(1, 3, 0, 10), .show_attribute = FALSE)
plot(choice_mm, .type = "bar")

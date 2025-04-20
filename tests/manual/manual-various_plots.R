library(tidyverse)
library(projoint)

profile_mm   <- projoint(out1_arranged) 
plot(profile_mm)

profile_amce   <- projoint(out1_arranged, .estimand = "amce") 
plot(profile_amce)


qoi <- set_qoi()
choice_mm <- projoint(out1_arranged, 
                      .structure = "choice_level", 
                      .att_choose = "att1")
plot(choice_mm)

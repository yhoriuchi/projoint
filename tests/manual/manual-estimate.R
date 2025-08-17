# 4. Estimate corrected MMs or AMCEs

# 4.1 ---------------------------------------------------------------------

library(projoint)

outcomes <- paste0("choice", seq(from = 1, to = 8, by = 1))
outcomes <- c(outcomes, "choice1_repeated_flipped")
out1 <- reshape_projoint(exampleData1, outcomes)

# 4.2 ---------------------------------------------------------------------

out1$labels

# 4.3 ---------------------------------------------------------------------

mm0 <- projoint(.data = out1,
                .qoi = NULL,
                .by_var = NULL,
                .structure = "profile_level",
                .estimand = "mm",
                .se_method = "analytical",
                .irr = NULL, 
                .remove_ties = TRUE,
                .ignore_position = NULL,
                .n_sims = NULL,
                .n_boot = NULL,
                .weights_1 = NULL,
                .clusters_1 = NULL,
                .se_type_1 = "classical",
                .weights_2 = NULL,
                .clusters_2 = NULL,
                .se_type_2 = "classical")

print(mm0)
summary(mm0)

qoi_1 <- set_qoi(
  .structure = "profile_level",
  .estimand = "mm",
  .att_choose = "att1", 
  .lev_choose = "level1")

mm1 <- projoint(.data = out1,
                .qoi = qoi_1)

print(mm1)

mm1b <- projoint(.data = out1,
                 .qoi = qoi_1,
                 .irr = 0.75)

print(mm1b)

qoi_2 <- set_qoi(
  .structure = "choice_level",
  .att_choose = "att1", 
  .lev_choose = "level3",
  .att_notchoose = "att1", 
  .lev_notchoose = "level1"
) 

mm2 <- projoint(.data = out1,
                .qoi = qoi_2,
                .structure = "choice_level",
                .ignore_position = FALSE)

print(mm2)


# 4.4 ---------------------------------------------------------------------

amce0 <- projoint(.data = out1,
                  .estimand = "amce")

print(amce0)

qoi_3 <- set_qoi(
  .structure = "profile_level",
  .estimand = "amce",
  .att_choose = "att1", 
  .lev_choose = "level3",
  .att_choose_b = "att1",
  .lev_choose_b = "level1"
)

amce1 <- projoint(.data = out1,
                  .qoi = qoi_3,
                  .estimand = "amce")

print(amce1)

amce1b <- projoint(.data = out1,
                   .qoi = qoi_3,
                   .estimand = "amce",
                   .irr = 0.75)

print(amce1b)
summary(amce1b)







# 6.1 ---------------------------------------------------------------------

library(projoint)
library(dplyr)
library(ggplot2)


# 6.2 ---------------------------------------------------------------------

outcomes <- paste0("choice", seq(from = 1, to = 8, by = 1))
outcomes <- c(outcomes, "choice1_repeated_flipped")

# Pre-processing
df <- exampleData1 %>% 
  mutate(white = ifelse(race == "White", 1, 0))

df_0 <- df %>% 
  filter(white == 0) %>% 
  reshape_projoint(outcomes)

df_1 <- df %>% 
  filter(white == 1) %>% 
  reshape_projoint(outcomes)

df_d <- df %>% 
  reshape_projoint(outcomes, .covariates = "white") 

df_0 <- read_labels(df_0, "inst/extdata/labels_arranged.csv")
df_1 <- read_labels(df_1, "inst/extdata/labels_arranged.csv")
df_d <- read_labels(df_d, "inst/extdata/labels_arranged.csv")

out_0 <- projoint(df_0)
out_1 <- projoint(df_1)
out_d <- projoint(df_d, .by_var = "white")

out_d$estimates
out_d$tau

plot_0 <- plot(out_0)
plot_1 <- plot(out_1)
plot_d <- plot(out_d, .by_var = TRUE)

g_0 <- plot_0 +
  coord_cartesian(xlim = c(0.2, 0.8)) +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.7)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Non-white", 
       x = "AMCE")

g_1 <- plot_1 + 
  coord_cartesian(xlim = c(0.2, 0.8)) +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.7)) +
  theme(axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "White",
       x = "AMCE")

g_d <- plot_d + 
  coord_cartesian(xlim = c(-0.4, 0.4)) +
  scale_x_continuous(breaks = c(-0.25, 0, 0.25)) +
  theme(axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Difference")

library(patchwork)
g_0 + g_1 + g_d 


# 6.3 ---------------------------------------------------------------------

df_D <- exampleData1 %>% 
  filter(party_1 == "Democrat") %>% 
  reshape_projoint(outcomes)

df_R <- exampleData1 %>% 
  filter(party_1 == "Republican") %>% 
  reshape_projoint(outcomes)

df_0 <- exampleData1 %>% 
  filter(party_1 %in% c("Something else", "Independent")) %>% 
  reshape_projoint(outcomes)

qoi <- set_qoi(
  .structure = "choice_level",
  .estimand = "mm",
  .att_choose = "att2", # Presidential Vote (2020)
  .lev_choose = "level3", # 70% Democrat, 30% Republican 
  .att_notchoose = "att2", 
  .lev_notchoose = "level1", # 30% Democrat, 70% Republican
)

out_D <- projoint(df_D, qoi)
out_R <- projoint(df_R, qoi)
out_0 <- projoint(df_0, qoi)

out_merged <- bind_rows(
  out_D$estimates %>% mutate(party = "Democrat"),
  out_R$estimates %>% mutate(party = "Republican"),
  out_0$estimates %>% mutate(party = "Independent")
) %>% 
  filter(estimand == "mm_corrected")

ggplot(out_merged, 
       aes(y = party,
           x = estimate)) +
  geom_vline(xintercept = 0.5,
             linetype = "dashed", 
             color = "gray") +
  geom_pointrange(aes(xmin = conf.low,
                      xmax = conf.high)) +
  geom_text(aes(label = format(round(estimate, digits = 2), nsmall = 2)),
            vjust = -1) +
  labs(y = NULL,
       x = "Choice-level marginal mean", 
       title = "Choose an area with 70% Democrat\n as opposed to an area with 30% Democrat") +
  theme_classic()


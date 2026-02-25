rm(list=ls())

library(tidyverse)
library(ggh4x)
source("../helper/mytheme.R")

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))

options(mc.cores = 4L)

estimates <- read.csv("data/predictions_plotting_long.csv", sep = "\t",
                        dec = ",") %>%
  mutate(Experiment = factor(Experiment, levels = c("Expts. 1 and 2",
                                  "Experiment 3",
                                  "Simulations: 2 features, half match",
                                  "Simulations: 2 features, full match",
                                  "Simulations: 3 features, half match",
                                  "Simulations: 3 features, full match"
                                  )),
         Effect = factor(Effect),
         Effect = fct_recode(Effect, "Attraction in match vs.\nnonsyncretic mismatch" = "Attraction in match vs. nonsyncretic mismatch",
                                     "Attraction in match vs.\nsyncretic mismatch" = "Attraction in match vs. syncretic mismatch",
                                     "Match vs.\nnonsyncretic mismatch" = "Match vs. nonsyncretic mismatch",
                                     "Match vs.\nsyncretic mismatch" = "Match vs. syncretic mismatch"))

pos = position_dodge(width = 0.4)

cols <- c("Expts. 1 and 2" = "firebrick", 
          "Experiment 3" = "firebrick4", 
          "Simulations: 2 features, half match" = "grey83",
          "Simulations: 2 features, full match" = "grey68", 
          "Simulations: 3 features, half match" = "grey50",
          "Simulations: 3 features, full match" = "grey32")


estimates %>%
  ggplot(aes(x = Median, y = Effect, color = Experiment)) +
  geom_pointrange(aes(xmin = Lower, xmax = Upper), 
                  linewidth = 1.5, size = 0.5, position = pos) +
  facet_wrap2(~Grammaticality, axes = "y",
              scales = "free", remove_labels = "x"
  ) +
  facetted_pos_scales(
    y = list(
      Grammaticality == "Ungrammatical" ~ scale_y_discrete(position = "right")
    )) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_color_manual(values = cols) + mytheme +
  theme(axis.title.x = element_text()) +
  xlab("Time [ms]") + ylab("") +
  xlim(-72, 72) + 
  guides(col = guide_legend(override.aes = list(size=1), nrow=2))
ggsave('img/Estimate_comparison.pdf', width = 19, height = 8)


# Predictions only:
estimates <- read.csv("data/predictions_plotting_long.csv", sep = "\t",
                      dec = ",") %>%
  drop_na()%>%
  mutate(Experiment = factor(Experiment, levels = c("Simulations: 2 features, half match",
                                                    "Simulations: 2 features, full match",
                                                    "Simulations: 3 features, half match",
                                                    "Simulations: 3 features, full match"
  ) ),
  Experiment = fct_recode(Experiment,
                          "2 features, half match" = "Simulations: 2 features, half match",
                          "2 features, full match" = "Simulations: 2 features, full match",
                          "3 features, half match" = "Simulations: 3 features, half match",
                          "3 features, full match" = "Simulations: 3 features, full match"),
  Effect = factor(Effect),
  Effect = fct_recode(Effect, "Attraction in match vs.\nnonsyncretic mismatch" = "Attraction in match vs. nonsyncretic mismatch",
                      "Attraction in match vs.\nsyncretic mismatch" = "Attraction in match vs. syncretic mismatch",
                      "Match vs.\nnonsyncretic mismatch" = "Match vs. nonsyncretic mismatch",
                      "Match vs.\nsyncretic mismatch" = "Match vs. syncretic mismatch"))

cols <- c(
          "2 features, half match" = "grey83",
          "2 features, full match" = "grey68", 
          "3 features, half match" = "grey50",
          "3 features, full match" = "grey32")

estimates %>%
  ggplot(aes(x = Median, y = Effect, color = Experiment)) +
  geom_pointrange(aes(xmin = Lower, xmax = Upper), 
                  linewidth = 1.5, size = 0.5, position = pos) +
  facet_wrap2(~Grammaticality, axes = "y",
              scales = "free", remove_labels = "x"
  ) +
  facetted_pos_scales(
    y = list(
      Grammaticality == "Ungrammatical" ~ scale_y_discrete(position = "right")
    )) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_color_manual(values = cols) + mytheme +
  theme(axis.title.x = element_text()) +
  xlab("Time [ms]") + ylab("") +
  xlim(-72, 72) + 
  guides(col = guide_legend(override.aes = list(size=1), nrow=2))
ggsave('img/Predictions.pdf', width = 18, height = 7)
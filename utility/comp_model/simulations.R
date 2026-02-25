rm(list=ls())

# plot prior predictions:
library(tidyverse)
library(tidybayes)  # for getting attraction effect estimates
source("../helper/mytheme.R")

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))

options(mc.cores = 4L)

################ Running the model #################
# run Reproduce-prediction-samples/Retrieval-only.R


################# Condition means ###################
load(file='ACTR_predictions_2features_full_match_mp0.15.Rda')
mp_2full <- all_chains_par %>%
  mutate(assumptions = "Syncretic:\nfull match",
         features = "2 features")

load(file='ACTR_predictions_2features_half_match_mp0.15.Rda')
mp_2half <- all_chains_par %>%
  mutate(assumptions = "Syncretic:\nhalf match",
         features = "2 features")

sims <- rbind(#mp_3full, mp_3half
   mp_2full, mp_2half
  ) %>%
  select(-c("weight", "likelihood", "imp_density", "ESS"))%>%
  rename('a' = xsim_condA,
         'b' = xsim_condB,
         'c' = xsim_condC,
         'd' = xsim_condD,
         'e' = xsim_condE,
         'f' = xsim_condF
        ) %>%
  pivot_longer(cols= c('a', 'b', 'c', 'd', 'e', 'f'), 
               names_to = "Condition", values_to = "time") %>%
  mutate(Verb = ifelse(Condition %in% c('a', 'b', 'c'),
                             "Grammatical", "Ungrammatical"),
         Attractor = case_when(
           Condition %in% c('a', 'd') ~ "Matching\ngender", 
           Condition %in% c('b', 'e') ~ "Mismatching\nnon-syncretic",
           Condition %in% c('c', 'f') ~ "Mismatching\nsyncretic"
           ),
         time = time + 250, # for response planning and button pressing
         logtime = log(time)
  ) 



pos=position_dodge(width = 0.1)
f_labels = data.frame(assumptions = c("Syncretic:\nfull match",
                                      "Syncretic:\nhalf match")
)


# ms scale
sims %>%
  group_by(Verb, Attractor, assumptions) %>%
  summarize(ymin = min(time),
            ymax = max(time),
            mean = mean(time)) %>%
  ggplot(aes(y = mean, x = Attractor, group = Verb, color = Verb)) +
  geom_pointrange(aes(ymin = ymin, ymax = ymax), 
                  linewidth = 1.5, size = 1.5, position = pos) +
  facet_grid(cols = vars(Attractor), 
             scales = "free_x", rows=vars(assumptions)) +
  scale_color_grey() + mytheme +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("Predicted time [ms]") +
  ylim(200, 550) + 
  guides(col = guide_legend(override.aes = list(size=2), nrow=2))

ggsave('img/simulations_2features.pdf', width = 18, height = 10)


# Predicted ungrammaticality effects: 
predicted <- sims %>%
  group_by(sample_id, assumptions) %>%
  select(- c(Verb, Attractor, logtime)) %>%
  pivot_wider(names_from = Condition,
              values_from = time) %>%
  mutate(matching = d-a,
         mismatch_nonsyncretic = e-b,
         mismatch_syncretic = f-c) %>%
  group_by(assumptions) %>%
  mean_qi(matching, mismatch_nonsyncretic, mismatch_syncretic) %>% 
  mutate_if(is.numeric, round) %>%
  select(-c(`.width`, `.point`, `.interval`)) %>%
  pivot_longer(2:10)

predicted

# Predicted attraction effects in ungrammatical sentences: 
predicted2 <- sims %>%
  group_by(sample_id, assumptions) %>%
  select(- c(Verb, Attractor, logtime)) %>%
  pivot_wider(names_from = Condition,
              values_from = time) %>%
  mutate(matching = d-a,
         mismatch_nonsyncretic = e-b,
         mismatch_syncretic = f-c,
         diff1 = matching - mismatch_nonsyncretic,
         diff2 = matching - mismatch_syncretic)%>%
  group_by(assumptions) %>%
  mean_qi(diff1, diff2) %>% 
  mutate_if(is.numeric, round) %>%
  select(-c(`.width`, `.point`, `.interval`)) 
predicted2

# # Predicted inhibitory interference in grammatical sentences: 
inh_int <- sims %>%
  group_by(sample_id, assumptions) %>%
  select(- c(Verb, Attractor, logtime)) %>%
  pivot_wider(names_from = Condition,
              values_from = time) %>%
  mutate(matching_vs_mismatch_nonsync = a-b,
         matching_vs_mismatch_sync = a-c) %>%
  group_by(assumptions) %>%
  mean_qi(matching_vs_mismatch_nonsync, matching_vs_mismatch_sync) %>% 
  mutate_if(is.numeric, round) %>%
  select(-c(`.width`, `.point`, `.interval`)) %>%
  pivot_longer(2:7)

inh_int

#### for three cues and features
load(file='ACTR_predictions_3features_full_match_mp0.15.Rda')
mp_3full <- all_chains_par %>%
  mutate(assumptions = "Syncretic:\nfull match",
         features = "3 features")

load(file='ACTR_predictions_3features_half_match_mp0.15.Rda')
mp_3half <- all_chains_par %>%
  mutate(assumptions = "Syncretic:\nhalf match",
         features = "3 features")

sims <- rbind(mp_3full, mp_3half
) %>%
  select(-c("weight", "likelihood", "imp_density", "ESS"))%>%
  rename('a' = xsim_condA,
         'b' = xsim_condB,
         'c' = xsim_condC,
         'd' = xsim_condD,
         'e' = xsim_condE,
         'f' = xsim_condF
  ) %>%
  pivot_longer(cols= c('a', 'b', 'c', 'd', 'e', 'f'), 
               names_to = "Condition", values_to = "time") %>%
  mutate(Verb = ifelse(Condition %in% c('a', 'b', 'c'),
                       "Grammatical", "Ungrammatical"),
         Attractor = case_when(
           Condition %in% c('a', 'd') ~ "Matching\ngender", 
           Condition %in% c('b', 'e') ~ "Mismatching\nnon-syncretic",
           Condition %in% c('c', 'f') ~ "Mismatching\nsyncretic"
         ),
         time = time + 250, # for response planning and button pressing
         logtime = log(time)
  ) 

# ms scale
sims %>%
  group_by(Verb, Attractor, assumptions) %>%
  summarize(ymin = min(time),
            ymax = max(time),
            mean = mean(time)) %>%
  ggplot(aes(y = mean, x = Attractor, group = Verb, color = Verb)) +
  geom_pointrange(aes(ymin = ymin, ymax = ymax), 
                  linewidth = 1.5, size = 1.5, position = pos) +
  facet_grid(cols = vars(Attractor), 
             scales = "free_x", rows=vars(assumptions)) +
  scale_color_grey() + mytheme +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("Predicted time [ms]") +
  ylim(200, 550) + 
  guides(col = guide_legend(override.aes = list(size=2), nrow=2))

ggsave('img/simulations_3features.pdf', width = 18, height = 10)

# Predicted ungrammaticality effects: 
predicted <- sims %>%
  group_by(sample_id, assumptions) %>%
  select(- c(Verb, Attractor, logtime)) %>%
  pivot_wider(names_from = Condition,
              values_from = time) %>%
  mutate(matching = d-a,
         mismatch_nonsyncretic = e-b,
         mismatch_syncretic = f-c) %>%
  group_by(assumptions) %>%
  mean_qi(matching, mismatch_nonsyncretic, mismatch_syncretic) %>% 
  mutate_if(is.numeric, round) %>%
  select(-c(`.width`, `.point`, `.interval`)) %>%
  pivot_longer(2:10)

predicted

# Predicted attraction efefcts in ungrammatical sentences: 
predicted2 <- sims %>%
  group_by(sample_id, assumptions) %>%
  select(- c(Verb, Attractor, logtime)) %>%
  pivot_wider(names_from = Condition,
              values_from = time) %>%
  mutate(matching = d-a,
         mismatch_nonsyncretic = e-b,
         mismatch_syncretic = f-c,
         diff1 = matching - mismatch_nonsyncretic,
         diff2 = matching - mismatch_syncretic)%>%
  group_by(assumptions) %>%
  mean_qi(diff1, diff2) %>% 
  mutate_if(is.numeric, round) %>%
  select(-c(`.width`, `.point`, `.interval`)) 
predicted2

# # Predicted inhibitory interference in grammatical sentences: 
inh_int <- sims %>%
  group_by(sample_id, assumptions) %>%
  select(- c(Verb, Attractor, logtime)) %>%
  pivot_wider(names_from = Condition,
              values_from = time) %>%
  mutate(matching_vs_mismatch_nonsync = a-b,
         matching_vs_mismatch_sync = a-c) %>%
  group_by(assumptions) %>%
  mean_qi(matching_vs_mismatch_nonsync, matching_vs_mismatch_sync) %>% 
  mutate_if(is.numeric, round) %>%
  select(-c(`.width`, `.point`, `.interval`)) %>%
  pivot_longer(2:7)

inh_int
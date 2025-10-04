# config <- yaml::read_yaml("scripts/config.yml")
# load(config$exp1$out$dataPrep)
# config <- yaml::read_yaml("scripts/config.yml")

# DATA -------

exp2.df <- read_experimental_data(config$exp2$data, subj_offset = 2000, item_offset = 2000) 

exp2.df %<>% mutate(exp_condition = case_when(
  exp_condition == "filler" & item_num <= 120 ~ "filler_ung",
  exp_condition == "filler" & item_num >= 121 ~ "filler_g",
  exp_condition == "practice" ~ "practice",
  exp_condition == "condition_b" ~ "condition_b",
  exp_condition == "condition_a" ~ "condition_a",
  exp_condition == "condition_c" ~ "condition_c",
  exp_condition == "condition_d" ~ "condition_d"
))


exp2.conditions <- data.frame(
  exp_condition = c("practice", "condition_a", "condition_b", "condition_c", "condition_d", "filler_ung", "filler_g"),
  experiment =    c("practice", "AgrAttr",     "AgrAttr",     "AgrAttr",     "AgrAttr",     "filler",     "filler"),
  condition =     c("practice", "a",           "b",           "c",           "d",           "filler_ung", "filler_g"),
  grammatical =   c("practice", "ungram",      "gram",        "ungram",      "gram",        "ungram",     "gram"),
  verb_num =      c("practice", "pl",          "sg",          "pl",          "sg",          "sg",         "pl"),
  attractor_num = c("practice", "pl",          "pl",          "sg",          "sg",          'filler',     'filler'),
  match =         c("practice", "mismatch",    "mismatch",    "match",       "match",       'filler',     'filler'),
  stringsAsFactors = T
)

exp2.df %<>% left_join(exp2.conditions, by = "exp_condition")

exp2.no.practice <- exp2.df %>%
                          subset(exp_condition != "practice")

exp2.clean <- exclude_bad_subjects(
  exp2.df,
  accuracy_threshold = config$exp2$thresholds$accuracy,
  rt_below = config$exp2$thresholds$rt_below,
  rt_upper = config$exp2$thresholds$rt_upper
)

exp2.clean %<>% no_null_no_practice(.)

stopifnot(exp2.clean %>% subset(is.na(response_yes)) %>% nrow() == 0)


# DIFF DATA =====
exp2.diff <- dplyr::anti_join(exp2.df, exp2.clean) %>%
  filter(exp_condition != "practice")

exp2.clean$isGram <- ifelse(exp2.clean$grammatical == "ungram", F, T)
exp2.clean$p_acc <- with(exp2.clean, response_yes & isGram)
exp2.clean %<>% 
  mutate(ResponseCorrect = (response_yes == (grammatical == "gram")))

# Merge

df_merge_exp2 <- exp2.clean %>% ungroup() %>% 
                      dplyr::select(source=experiment, 
                                    grammatical, 
                                    attractor_num,
                                    match,
                                    # condition,
                                    subject, 
                                    trial_no,
                                    item,
                                    response_yes, 
                                    RT,
                                    ResponseCorrect)
df_merge_exp2$experiment <- "Experiment 2"
df_merge_exp2$grammatical %<>% dplyr::recode(gram="grammatical", ungram="ungrammatical")
df_merge_exp2$attractor_num %<>% dplyr::recode(pl="plural", sg="singular")

df_merge_exp2$item %<>% as.factor()
df_merge_exp2$subject %<>% as.character()

exp2.merged <- dplyr::bind_rows(df_merge_exp2, exp1.merged)
exp2.merged$subject %<>% as.factor()
exp2.merged$item %<>% as.factor()


# AVERAGES =====

exp2.avg <- get_averages(
    subset(exp2.merged, experiment == "Experiment 2"), 
    grouping = c('experiment', 'grammatical', 'attractor_num', 'match')
) %>% as.data.table()


exp2.avgs = bind_rows(
  subset(exp2.avg, resp.match != "filler"),
  exp1.avgs
)

#avg_g_resp <- avg_g[, .SD, .SDcols = names(avg_g) %like% "resp"]
#avg_u_resp <- avg_u[, .SD, .SDcols = names(avg_u) %like% "resp"]

#avg_g_rt <- avg_g[, .SD, .SDcols = names(avg_g) %like% "rt_correct"]
#avg_u_rt <- avg_u[, .SD, .SDcols = names(avg_u) %like% "rt_correct"]


# FILLER AVERAGES =====

exp2.f.avgs = bind_rows(
  subset(exp2.avg, resp.match == "filler"),
  exp1.f.avgs
)

# I want accuracy, not the response yes
exp2.f.avgs[resp.grammatical == "ungrammatical" & resp.experiment == "Experiment 2"]$resp.M %<>% "-"(1,.)

# TEXT INPUTS ========

exp2.nsubj <- quantify_subjects(exp2.df)

exp2.nsubj.nontr <- exp2.df %>%
  subset(natturk == "nat_non_turk") %>%
  .$subject %>%
  unique() %>%
  length()

exp2.nsubj.threshold <- 2

exp2.deletion <- quantify_preprocessing(exp2.no.practice , exp2.clean)

exp2.meanage <- mean(asi(exp2.clean$age)) %>% round()
exp2.maxage <- max(asi(exp2.clean$age))
exp2.minage <- min(asi(exp2.clean$age))

# FILLER AVERAGES
FGM2 <- exp2.f.avgs[resp.grammatical == "grammatical" & resp.experiment == "Experiment 2"]$resp.M %>% round(2)
FUM2 <- exp2.f.avgs[resp.grammatical == "ungrammatical" & resp.experiment == "Experiment 2"]$resp.M %>% round(2)
FGSE2 <- exp2.f.avgs[resp.grammatical == "grammatical" & resp.experiment == "Experiment 2"]$resp.SE %>% round(2)
FUSE2 <- exp2.f.avgs[resp.grammatical == "ungrammatical" & resp.experiment == "Experiment 2"]$resp.SE %>% round(2)



# EXP AVERAGES
# SOURCE - GRAM - ATTNUM - mean or se
GPM2 <- round(exp2.avgs[resp.experiment == "Experiment 2" &  resp.grammatical == "grammatical" & resp.attractor_num == "plural"]$resp.M,2)

GPSE2 <- round(exp2.avgs[resp.experiment == "Experiment 2" &  resp.grammatical == "grammatical" & resp.attractor_num == "plural"]$resp.SE,2)

GSM2 <- round(exp2.avgs[resp.experiment == "Experiment 2" &  resp.grammatical == "grammatical" & resp.attractor_num == "singular"]$resp.M,2)

GSSE2 <- round(exp2.avgs[resp.experiment == "Experiment 2" &  resp.grammatical == "grammatical" & resp.attractor_num == "singular"]$resp.SE,2)

UPM2 <- round(exp2.avgs[resp.experiment == "Experiment 2" &  resp.grammatical == "ungrammatical" & resp.attractor_num == "plural"]$resp.M,2)

UPSE2 <- round(exp2.avgs[resp.experiment == "Experiment 2" &  resp.grammatical == "ungrammatical" & resp.attractor_num == "plural"]$resp.SE,2)

USM2 <- round(exp2.avgs[resp.experiment == "Experiment 2" &  resp.grammatical == "ungrammatical" & resp.attractor_num == "singular"]$resp.M,2)

USSE2 <- round(exp2.avgs[resp.experiment == "Experiment 2" &  resp.grammatical == "ungrammatical" & resp.attractor_num == "singular"]$resp.SE,2)


# RT
GSME2RT <- round(exp2.avgs[resp.experiment == "Experiment 2" & resp.grammatical == "grammatical" & resp.attractor_num == "singular"]$rt_correct.M,2)

GSSE2RT <- round(exp2.avgs[resp.experiment == "Experiment 2" & resp.grammatical == "grammatical" & resp.attractor_num == "singular"]$rt_correct.SE,2)

GPME2RT <- round(exp2.avgs[resp.experiment == "Experiment 2" & resp.grammatical == "grammatical" & resp.attractor_num == "plural"]$rt_correct.M,2)

GPSE2RT <- round(exp2.avgs[resp.experiment == "Experiment 2" & resp.grammatical == "grammatical" & resp.attractor_num == "plural"]$rt_correct.SE,2)

USME2RT <- round(exp2.avgs[resp.experiment == "Experiment 2" & resp.grammatical == "ungrammatical" & resp.attractor_num == "singular"]$rt_correct.M,2)

USSE2RT <- round(exp2.avgs[resp.experiment == "Experiment 2" & resp.grammatical == "ungrammatical" & resp.attractor_num == "singular"]$rt_correct.SE,2)

UPME2RT <- round(exp2.avgs[resp.experiment == "Experiment 2" & resp.grammatical == "ungrammatical" & resp.attractor_num == "plural"]$rt_correct.M,2)

UPSE2RT <- round(exp2.avgs[resp.experiment == "Experiment 2" & resp.grammatical == "ungrammatical" & resp.attractor_num == "plural"]$rt_correct.SE,2)

# save.image(file = config$exp2$out$dataPrep)

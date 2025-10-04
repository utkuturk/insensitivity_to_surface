# config <- yaml::read_yaml("scripts/config.yml")
# load(config$exp1$out$dataPrep)
# config <- yaml::read_yaml("scripts/config.yml")

# DATA -------



exp2b.df <- read_experimental_data(config$exp2b$data, subj_offset = 2500, item_offset = 2500) 

exp2b.df %<>% mutate(exp_condition = case_when(
  exp_condition == "filler" & item_num <= 120 ~ "filler_ung",
  exp_condition == "filler" & item_num >= 121 ~ "filler_g",
  exp_condition == "practice" ~ "practice",
  exp_condition == "condition_gen_b" ~ "condition_gen_b",
  exp_condition == "condition_gen_a" ~ "condition_gen_a",
  exp_condition == "condition_gen_c" ~ "condition_gen_c",
  exp_condition == "condition_gen_d" ~ "condition_gen_d",
  exp_condition == "condition_rc_b" ~ "condition_rc_b",
  exp_condition == "condition_rc_a" ~ "condition_rc_a",
  exp_condition == "condition_rc_c" ~ "condition_rc_c",
  exp_condition == "condition_rc_d" ~ "condition_rc_d"
))


exp2b.conditions <- data.frame(
  exp_condition = c("practice", "condition_gen_a", "condition_gen_b", "condition_gen_c", "condition_gen_d", "condition_rc_a", "condition_rc_b", "condition_rc_c", "condition_rc_d", "filler_ung", "filler_g"),
  experiment =    c("practice", "AgrAttr", "AgrAttr", "AgrAttr", "AgrAttr", "AgrAttr", "AgrAttr", "AgrAttr", "AgrAttr", "filler", "filler"),
  condition =     c("practice", "gen_a", "gen_b", "gen_c", "gen_d", "rc_a", "rc_b", "rc_c", "rc_d", "filler_ung", "filler_g"),
  grammatical =   c("practice", "ungram", "gram", "ungram", "gram", "ungram", "gram", "ungram", "gram", "ungram","gram"),
  verb_num =      c("practice", "pl", "sg", "pl", "sg", "pl", "sg", "pl", "sg", "sg", "pl"),
  attractor_num = c("practice", "pl", "pl", "sg", "sg", "pl", "pl", "sg", "sg", 'filler', 'filler'),
  match =         c("practice", "mismatch", "mismatch", "match", "match", "mismatch", "mismatch", "match", "match", 'filler', 'filler'),
  att_type =      c("practice", rep("gen", 4), rep("rc",4), "filler", "filler"),
  stringsAsFactors = T
)

exp2b.df %<>% left_join(exp2b.conditions, by = "exp_condition")

exp2b.no.practice <- exp2b.df %>%
                          subset(exp_condition != "practice")

exp2b.clean <- exclude_bad_subjects_8(
  exp2b.df,
  accuracy_threshold = config$exp2b$thresholds$accuracy,
  rt_below = config$exp2b$thresholds$rt_below,
  rt_upper = config$exp2b$thresholds$rt_upper
)

exp2b.clean %<>% no_null_no_practice(.)

stopifnot(exp2b.clean %>% subset(is.na(response_yes)) %>% nrow() == 0)


# DIFF DATA =====
exp2b.diff <- dplyr::anti_join(exp2b.df, exp2b.clean) %>%
  filter(exp_condition != "practice")

exp2b.clean$isGram <- ifelse(exp2b.clean$grammatical == "ungram", F, T)
exp2b.clean$p_acc <- with(exp2b.clean, response_yes & isGram)
exp2b.clean %<>% 
  mutate(ResponseCorrect = (response_yes == (grammatical == "gram")))

# Merge

# df_merge_exp2b <- exp2b.clean %>% ungroup() %>% 
#                       dplyr::select(source=experiment, 
#                                     grammatical, 
#                                     attractor_num,
#                                     match,
#                                     # condition,
#                                     subject, 
#                                     trial_no,
#                                     item,
#                                     response_yes, 
#                                     RT,
#                                     ResponseCorrect)
# df_merge_exp2b$experiment <- "Experiment 2"
# df_merge_exp2b$grammatical %<>% dplyr::recode(gram="grammatical", ungram="ungrammatical")
# df_merge_exp2b$attractor_num %<>% dplyr::recode(pl="plural", sg="singular")

# df_merge_exp2b$item %<>% as.factor()
# df_merge_exp2b$subject %<>% as.character()

# exp2b.merged <- dplyr::bind_rows(df_merge_exp2b, exp1.merged)
# exp2b.merged$subject %<>% as.factor()
# exp2b.merged$item %<>% as.factor()


# AVERAGES =====

exp2b.avg <- get_averages(
    subset(exp2b.clean), 
    grouping = c('experiment', 'grammatical', 'attractor_num', 'match', 'att_type')
) %>% as.data.table()


exp2b.avgs = subset(exp2b.avg, resp.match != "filler")

#avg_g_resp <- avg_g[, .SD, .SDcols = names(avg_g) %like% "resp"]
#avg_u_resp <- avg_u[, .SD, .SDcols = names(avg_u) %like% "resp"]

#avg_g_rt <- avg_g[, .SD, .SDcols = names(avg_g) %like% "rt_correct"]
#avg_u_rt <- avg_u[, .SD, .SDcols = names(avg_u) %like% "rt_correct"]


# FILLER AVERAGES =====

exp2b.f.avgs = subset(exp2b.avg, resp.match == "filler")

# I want accuracy, not the response yes
exp2b.f.avgs[resp.grammatical == "ungram"]$resp.M %<>% "-"(1,.)

# TEXT INPUTS ========

exp2b.nsubj <- quantify_subjects(exp2b.df)

exp2b.nsubj.nontr <- exp2b.df %>%
  subset(natturk == "nat_non_turk") %>%
  .$subject %>%
  unique() %>%
  length()

exp2b.nsubj.threshold <- 3

exp2b.deletion <- quantify_preprocessing(exp2b.no.practice , exp2b.clean)

exp2b.meanage <- mean(asi(exp2b.clean$age)) %>% round()
exp2b.maxage <- max(asi(exp2b.clean$age))
exp2b.minage <- min(asi(exp2b.clean$age))

# FILLER AVERAGES
FGM2B <- exp2b.f.avgs[resp.grammatical == "gram" ]$resp.M %>% round(2)
FUM2B <- exp2b.f.avgs[resp.grammatical == "ungram"]$resp.M %>% round(2)
FGSE2B <- exp2b.f.avgs[resp.grammatical == "gram"]$resp.SE %>% round(2)
FUSE2B <- exp2b.f.avgs[resp.grammatical == "gram"]$resp.SE %>% round(2)



# EXP AVERAGES
# SOURCE - GRAM - ATTNUM - mean or se
GPM2BG <- round(exp2b.avgs[resp.grammatical == "gram" & resp.attractor_num == "pl" & resp.att_type == "gen"]$resp.M,2)
GPSE2BG <- round(exp2b.avgs[resp.grammatical == "gram" & resp.attractor_num == "pl" & resp.att_type == "gen"]$resp.SE,2)
GSM2BG <- round(exp2b.avgs[resp.grammatical == "gram" & resp.attractor_num == "sg" & resp.att_type == "gen"]$resp.M,2)
GSSE2BG <- round(exp2b.avgs[resp.grammatical == "gram" & resp.attractor_num == "sg" & resp.att_type == "gen"]$resp.SE,2)
UPM2BG <- round(exp2b.avgs[resp.grammatical == "ungram" & resp.attractor_num == "pl" & resp.att_type == "gen"]$resp.M,2)
UPSE2BG <- round(exp2b.avgs[resp.grammatical == "ungram" & resp.attractor_num == "pl" & resp.att_type == "gen"]$resp.SE,2)
USM2BG <- round(exp2b.avgs[resp.grammatical == "ungram" & resp.attractor_num == "sg" & resp.att_type == "gen"]$resp.M,2)
USSE2BG <- round(exp2b.avgs[resp.grammatical == "ungram" & resp.attractor_num == "sg" & resp.att_type == "gen"]$resp.SE,2)


GPM2BR <- round(exp2b.avgs[resp.grammatical == "gram" & resp.attractor_num == "pl" & resp.att_type == "rc"]$resp.M,2)
GPSE2BR <- round(exp2b.avgs[resp.grammatical == "gram" & resp.attractor_num == "pl" & resp.att_type == "rc"]$resp.SE,2)
GSM2BR <- round(exp2b.avgs[resp.grammatical == "gram" & resp.attractor_num == "sg" & resp.att_type == "rc"]$resp.M,2)
GSSE2BR <- round(exp2b.avgs[resp.grammatical == "gram" & resp.attractor_num == "sg" & resp.att_type == "rc"]$resp.SE,2)
UPM2BR <- round(exp2b.avgs[resp.grammatical == "ungram" & resp.attractor_num == "pl" & resp.att_type == "rc"]$resp.M,2)
UPSE2BR <- round(exp2b.avgs[resp.grammatical == "ungram" & resp.attractor_num == "pl" & resp.att_type == "rc"]$resp.SE,2)
USM2BR <- round(exp2b.avgs[resp.grammatical == "ungram" & resp.attractor_num == "sg" & resp.att_type == "rc"]$resp.M,2)
USSE2BR <- round(exp2b.avgs[resp.grammatical == "ungram" & resp.attractor_num == "sg" & resp.att_type == "rc"]$resp.SE,2)

# RT
GSME2BGRT <- round(exp2b.avgs[resp.grammatical == "gram" & resp.attractor_num == "sg" & resp.att_type == "gen"]$rt_correct.M,2)
GSSE2BGRT <- round(exp2b.avgs[resp.grammatical == "gram" & resp.attractor_num == "sg" & resp.att_type == "gen"]$rt_correct.SE,2)
GPME2BGRT <- round(exp2b.avgs[resp.grammatical == "gram" & resp.attractor_num == "pl" & resp.att_type == "gen"]$rt_correct.M,2)
GPSE2BGRT <- round(exp2b.avgs[resp.grammatical == "gram" & resp.attractor_num == "pl" & resp.att_type == "gen"]$rt_correct.SE,2)
USME2BGRT <- round(exp2b.avgs[resp.grammatical == "ungram" & resp.attractor_num == "sg" & resp.att_type == "gen"]$rt_correct.M,2)
USSE2BGRT <- round(exp2b.avgs[resp.grammatical == "ungram" & resp.attractor_num == "sg" & resp.att_type == "gen"]$rt_correct.SE,2)
UPME2BGRT <- round(exp2b.avgs[resp.grammatical == "ungram" & resp.attractor_num == "pl" & resp.att_type == "gen"]$rt_correct.M,2)
UPSE2BGRT <- round(exp2b.avgs[resp.grammatical == "ungram" & resp.attractor_num == "pl" & resp.att_type == "gen"]$rt_correct.SE,2)

GSME2BRRT <- round(exp2b.avgs[resp.grammatical == "gram" & resp.attractor_num == "sg" & resp.att_type == "rc"]$rt_correct.M,2)
GSSE2BRRT <- round(exp2b.avgs[resp.grammatical == "gram" & resp.attractor_num == "sg" & resp.att_type == "rc"]$rt_correct.SE,2)
GPME2BRRT <- round(exp2b.avgs[resp.grammatical == "gram" & resp.attractor_num == "pl" & resp.att_type == "rc"]$rt_correct.M,2)
GPSE2BRRT <- round(exp2b.avgs[resp.grammatical == "gram" & resp.attractor_num == "pl" & resp.att_type == "rc"]$rt_correct.SE,2)
USME2BRRT <- round(exp2b.avgs[resp.grammatical == "ungram" & resp.attractor_num == "sg" & resp.att_type == "rc"]$rt_correct.M,2)
USSE2BRRT <- round(exp2b.avgs[resp.grammatical == "ungram" & resp.attractor_num == "sg" & resp.att_type == "rc"]$rt_correct.SE,2)
UPME2BRRT <- round(exp2b.avgs[resp.grammatical == "ungram" & resp.attractor_num == "pl" & resp.att_type == "rc"]$rt_correct.M,2)
UPSE2BRRT <- round(exp2b.avgs[resp.grammatical == "ungram" & resp.attractor_num == "pl" & resp.att_type == "rc"]$rt_correct.SE,2)

# save.image(file = config$exp2b$out$dataPrep)

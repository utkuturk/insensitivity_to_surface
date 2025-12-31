# HSP Nested Model (Slash Syntax)
library(tidyverse)
library(magrittr)
library(brms)
library(glue)
library(posterior)
library(ggplot2)

source("002_functions.R")

# 1. LOAD AND FILTER DATA
exp2 <- read_experimental_data(
    "../data/results_8cond.txt",
    subj_offset = 2500,
    item_offset = 2500
)

exp2 %<>%
    mutate(
        exp_condition = case_when(
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
        )
    )

exp2.conditions <- data.frame(
    exp_condition = c(
        "practice",
        "condition_gen_a",
        "condition_gen_b",
        "condition_gen_c",
        "condition_gen_d",
        "condition_rc_a",
        "condition_rc_b",
        "condition_rc_c",
        "condition_rc_d",
        "filler_ung",
        "filler_g"
    ),
    experiment = c(
        "practice",
        "AgrAttr",
        "AgrAttr",
        "AgrAttr",
        "AgrAttr",
        "AgrAttr",
        "AgrAttr",
        "AgrAttr",
        "AgrAttr",
        "filler",
        "filler"
    ),
    condition = c(
        "practice",
        "gen_a",
        "gen_b",
        "gen_c",
        "gen_d",
        "rc_a",
        "rc_b",
        "rc_c",
        "rc_d",
        "filler_ung",
        "filler_g"
    ),
    grammatical = c(
        "practice",
        "ungram",
        "gram",
        "ungram",
        "gram",
        "ungram",
        "gram",
        "ungram",
        "gram",
        "ungram",
        "gram"
    ),
    verb_num = c(
        "practice",
        "pl",
        "sg",
        "pl",
        "sg",
        "pl",
        "sg",
        "pl",
        "sg",
        "sg",
        "pl"
    ),
    attractor_num = c(
        "practice",
        "pl",
        "pl",
        "sg",
        "sg",
        "pl",
        "pl",
        "sg",
        "sg",
        "filler",
        "filler"
    ),
    match = c(
        "practice",
        "mismatch",
        "mismatch",
        "match",
        "match",
        "mismatch",
        "mismatch",
        "match",
        "match",
        "filler",
        "filler"
    ),
    att_type = c("practice", rep("gen", 4), rep("rc", 4), "filler", "filler"),
    stringsAsFactors = T
)

exp2 %<>% left_join(exp2.conditions, by = "exp_condition")

exp2.clean <- exclude_bad_subjects_8(
    exp2,
    accuracy_threshold = 0.25,
    rt_below = 200,
    rt_upper = 4999
)

exp2.clean %<>% no_null_no_practice(.)
exp2.clean$isGram <- ifelse(exp2.clean$grammatical == "ungram", F, T)
exp2.clean %<>%
    mutate(ResponseCorrect = (response_yes == (grammatical == "gram")))

exp2.clean %<>%
    ungroup() %>%
    dplyr::select(
        source = experiment,
        grammatical,
        attractor_num,
        att_type,
        match,
        subject,
        item,
        response_yes
    )
exp2.clean %<>% filter(attractor_num != "filler")
exp2.clean %<>% drop.levels()
exp2.clean$grammatical %<>%
    dplyr::recode(gram = "grammatical", ungram = "ungrammatical")
exp2.clean$attractor_num %<>% dplyr::recode(pl = "plural", sg = "singular")
exp2.clean$att_type %<>% dplyr::recode(gen = "gen", rc = "rc")
exp2.clean$item %<>% as.factor()
exp2.clean$subject %<>% as.character()

# 2. CONTRASTS
contrasts(exp2.clean$grammatical) <- contr.sum(2)
contrasts(exp2.clean$attractor_num) <- contr.sum(2)
contrasts(exp2.clean$att_type) <- contr.treatment(2)

# 3. FIT MODEL
cat("\nFitting Nested Model ...\n")

m.nested <- brm(
    bf(
        response_yes ~ 0 +
            att_type / (grammatical * attractor_num) +
            (1 + grammatical * attractor_num | subject) +
            (1 + grammatical * attractor_num | item),
        decomp = "QR"
    ),
    data = exp2.clean,
    family = bernoulli(link = "logit"),
    threads = threading(4),
    chains = 4,
    iter = 4000,
    warmup = 2000,
    seed = 1,
    file = "../models/m.nested"
)

# 4. PLOT COEFFICIENTS
draws <- as_draws_df(m.nested)

summarize_draws <- function(d, name) {
    p_gt0 <- mean(d > 0)
    tibble(
        mean = mean(d),
        l95 = quantile(d, 0.025),
        u95 = quantile(d, 0.975),
        p_gt0 = p_gt0
    ) %>%
        mutate(term = name)
}

# The coefficients will be expanded for each level of att_type.
# e.g. att_typegen, att_typegen:grammatical1, etc.
# Let's map them to readable names.

# Get valid column names
cols <- names(draws)

# Helper to find col
get_col <- function(pattern) cols[grepl(pattern, cols, fixed = TRUE)][1]

# Gen terms
c_gen_g <- get_col("att_typegen:grammatical1")
c_gen_a <- get_col("att_typegen:attractor_num1")
c_gen_ga <- get_col("att_typegen:grammatical1:attractor_num1")

# RC terms
c_rc_g <- get_col("att_typerc:grammatical1")
c_rc_a <- get_col("att_typerc:attractor_num1")
c_rc_ga <- get_col("att_typerc:grammatical1:attractor_num1")

coeffs <- bind_rows(
    summarize_draws(draws[[c_gen_g]], "Gen: Grammaticality"),
    summarize_draws(draws[[c_gen_a]], "Gen: AttrNum"),
    summarize_draws(draws[[c_gen_ga]], "Gen: Gram x AttrNum"),

    summarize_draws(draws[[c_rc_g]], "RC: Grammaticality"),
    summarize_draws(draws[[c_rc_a]], "RC: AttrNum"),
    summarize_draws(draws[[c_rc_ga]], "RC: Gram x AttrNum")
)

coeffs %<>%
    mutate(
        label = glue("{term}\nP(>0)={round(p_gt0, 2)}")
    )

p_nested_coef <- coeffs %>%
    mutate(label = factor(label, levels = rev(label))) %>%
    ggplot(aes(y = label, x = mean)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
    geom_point(size = 3) +
    geom_errorbarh(
        aes(xmin = l95, xmax = u95),
        height = 0.15,
        linewidth = 0.7
    ) +
    xlab("Coefficient Estimate (Log-Odds)") +
    ylab(NULL) +
    labs(title = "Nested Model Coefficients") +
    theme_minimal(base_family = "Times") +
    theme(
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 10)
    )

ggsave("../figures/hsp_nested_plot.png", p_nested_coef, width = 6, height = 4)

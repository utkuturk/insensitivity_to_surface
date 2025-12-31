#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: setup

set.seed(01110011)
library(tidyverse)
library(brms)
library(data.table)
library(gdata)
library(magrittr)
library(DescTools)
select <- dplyr::select
library(lingglosses)
#
#
#
#
#| label: functions

read_experimental_data <- function(fname, subj_offset = 0, item_offset = 0, verbose = F) {
    data <- read.csv(fname,
        header = F,
        comment.char = "#",
        encoding = "UTF-8",
        col.names = paste0("V", seq_len(11)),
        fill = TRUE,
        stringsAsFactors = FALSE
    )
    colnames(data) <- c("Time", "MD5", "ControllerType", "SentenceNoInStimFile", "Element", "exp_condition", "item", "Sentence", "Question", "Answer", "RT")

    subject_id <- with(data, {
        as.integer(as.factor(paste(Time, MD5)))
    })
    data$item[data$exp_condition == "intro" | data$exp_condition == "practice"] <- 0
    data$item_num <- as.integer(data$item)
    data$subject <- sprintf("S[%d]", subject_id + subj_offset)
    data$item <- sprintf("I[%d]", data$item_num + item_offset)

    df_forms <- data %>%
        subset(ControllerType != "DashedAcceptabilityJudgment") %>%
        gdata::drop.levels()
    data %<>% subset(ControllerType == "DashedAcceptabilityJudgment")

    age <- df_forms %>%
        dplyr::filter(Sentence == "age") %>%
        dplyr::select(subject, age = Question)
    natturk <- df_forms %>%
        dplyr::filter(Sentence == "natturk") %>%
        dplyr::select(subject, natturk = Question) %T>% {
            .$natturk %<>% recode(male = "nat_turk", female = "nat_non_turk")
        }
    forms <- dplyr::left_join(age, natturk, by = "subject")

    stopifnot(nrow(data) %% 2 == 0)
    rows_stim <- data[c(T, F), ]
    rows_resp <- data[c(F, T), ]
    stopifnot(all(is.na(rows_stim$RT)))

    data <- rows_resp %>%
        left_join(forms) %>%
        dplyr::select(-MD5, -Time, -ControllerType, -Sentence, -Element) %>%
        dplyr::rename(ResponseCorrect = Answer, Response = Question) %>%
        dplyr::select(-ResponseCorrect)
    data %<>% group_by(subject) %>% mutate(trial_no = seq(subject))
    data %<>% mutate(late_response = (Response == "NULL"), Response = ifelse(late_response, NA, as.character(Response)))

    responses <- c(yes = "İYİ (P'ye basınız)", no = "KÖTÜ (Q'ya basınız)")
    data$Response %<>% as.character() %>% enc2native()
    stopifnot(all(data$Response %in% responses | is.na(data$Response)))

    data$response_yes <- ifelse(grepl("P'ye", data$Response), T,
        ifelse(grepl("Q'ya", data$Response), F, NA)
    )
    if (verbose) {
        print(with(data, table(Response, response_yes)))
    }
    data %<>% dplyr::select(-Response)
    data
}


exclude_bad_subjects <- function(data_to_clean, accuracy_threshold = 0.25, rt_below = 200, rt_upper = 4999, verbose = F) {
    avg_by_subj <- data_to_clean %>%
        group_by(
            subject, experiment, condition,
            grammatical, verb_num, attractor_num
        ) %>%
        summarize(
            avRT = mean(RT),
            p_yes = mean(response_yes, na.rm = T),
            N = sum(!is.na(response_yes))
        )

    avg_by_subj_wide <- avg_by_subj %>%
        mutate(expcond = paste(experiment, condition, sep = "_")) %>%
        ungroup() %>%
        dplyr::select(
            -experiment, -condition, -avRT, -N,
            -grammatical, -verb_num, -attractor_num
        ) %>%
        tidyr::spread(expcond, p_yes) %>%
        mutate(delta_dc = AgrAttr_d - AgrAttr_c)

    bad_subjects <- subset(avg_by_subj_wide, delta_dc <= accuracy_threshold) %>% .$subject
    data_clean <- data_to_clean %>% subset(!subject %in% bad_subjects)

    data_clean %<>% filter(RT < rt_upper & rt_below < RT)
    if ("natturk" %in% colnames(data_clean)) {
        data_clean %<>% subset(natturk == "nat_turk")
    }

    if (verbose) {
        print(with(data_clean, table(exp_condition, response_yes)))
        print(sprintf("number of bad subjects: %f", length(bad_subjects)))
    }

    data_clean
}

no_null_no_practice <- function(data_to_clean) {
    data_to_clean %<>% subset(exp_condition != "practice") %>% subset(!is.na(response_yes))
}

asi <- function(x) {
    as.integer(x)
}
asf <- function(x) {
    as.factor(x)
}
asc <- function(x) {
    as.character(x)
}

get_value <- function(df, col, ...) {
    vals <- df %>%
        filter(...) %>%
        pull({{ col }})
    if (is.numeric(vals)) {
        vals <- round(vals, 2)
    } else {
        vals <- as.character(vals)
    }

    vals
}

exclude_bad_subjects_8 <- function(data_to_clean, accuracy_threshold = 0.25, rt_below = 200, rt_upper = 4999, verbose = F) {
    avg_by_subj <- data_to_clean %>%
        group_by(
            subject, experiment, condition,
            grammatical, verb_num, attractor_num, att_type
        ) %>%
        summarize(
            avRT = mean(RT),
            p_yes = mean(response_yes, na.rm = T),
            N = sum(!is.na(response_yes))
        )

    avg_by_subj_wide <- avg_by_subj %>%
        mutate(expcond = paste(experiment, condition, sep = "_")) %>%
        ungroup() %>%
        dplyr::select(
            -experiment, -condition, -avRT, -N,
            -grammatical, -verb_num, -attractor_num, -att_type
        ) %>%
        tidyr::spread(expcond, p_yes) %>%
        mutate(delta_gen_dc = AgrAttr_gen_d - AgrAttr_gen_c, delta_rc_dc = AgrAttr_rc_d - AgrAttr_rc_c)

    bad_subjects_gen <- subset(avg_by_subj_wide, delta_gen_dc <= 0.25) %>% .$subject
    bad_subjects_rc <- subset(avg_by_subj_wide, delta_rc_dc <= 0.25) %>% .$subject
    data_clean <- data_to_clean %>% subset(!subject %in% bad_subjects_gen | !subject %in% bad_subjects_rc)

    data_clean %<>% filter(RT < rt_upper & rt_below < RT)
    if ("natturk" %in% colnames(data_clean)) {
        data_clean %<>% subset(natturk == "nat_turk")
    }
    if (verbose) {
        print(with(data_clean, table(exp_condition, response_yes)))
        print(sprintf("number of bad subjects: %f", length(bad_subjects_gen) + length(bad_subjects_rc)))
    }

    data_clean
}

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: exp2-data-prep

exp2 <- read_experimental_data("../data/results_8cond.txt", subj_offset = 2500, item_offset = 2500)

exp2 %<>% mutate(exp_condition = case_when(
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


exp2.conditions <- data.frame(
    exp_condition = c("practice", "condition_gen_a", "condition_gen_b", "condition_gen_c", "condition_gen_d", "condition_rc_a", "condition_rc_b", "condition_rc_c", "condition_rc_d", "filler_ung", "filler_g"),
    experiment = c("practice", "AgrAttr", "AgrAttr", "AgrAttr", "AgrAttr", "AgrAttr", "AgrAttr", "AgrAttr", "AgrAttr", "filler", "filler"),
    condition = c("practice", "gen_a", "gen_b", "gen_c", "gen_d", "rc_a", "rc_b", "rc_c", "rc_d", "filler_ung", "filler_g"),
    grammatical = c("practice", "ungram", "gram", "ungram", "gram", "ungram", "gram", "ungram", "gram", "ungram", "gram"),
    verb_num = c("practice", "pl", "sg", "pl", "sg", "pl", "sg", "pl", "sg", "sg", "pl"),
    attractor_num = c("practice", "pl", "pl", "sg", "sg", "pl", "pl", "sg", "sg", "filler", "filler"),
    match = c("practice", "mismatch", "mismatch", "match", "match", "mismatch", "mismatch", "match", "match", "filler", "filler"),
    att_type = c("practice", rep("gen", 4), rep("rc", 4), "filler", "filler"),
    stringsAsFactors = T
)

exp2 %<>% left_join(exp2.conditions, by = "exp_condition")

exp2.no.practice <- exp2 %>% subset(exp_condition != "practice")

# accuracy: 0.25
# rt_below: 200
# rt_upper: 4999
exp2.clean <- exclude_bad_subjects_8(
    exp2,
    accuracy_threshold = 0.25,
    rt_below = 200,
    rt_upper = 4999
)

exp2.clean %<>% no_null_no_practice(.)

stopifnot(exp2.clean %>% subset(is.na(response_yes)) %>% nrow() == 0)

# DIFF DATA =====
exp2.diff <- dplyr::anti_join(exp2, exp2.clean) %>%
    filter(exp_condition != "practice")

exp2.clean$isGram <- ifelse(exp2.clean$grammatical == "ungram", F, T)
exp2.clean$p_acc <- with(exp2.clean, response_yes & isGram)
exp2.clean %<>% mutate(ResponseCorrect = (response_yes == (grammatical == "gram")))

# Merge

exp2.clean %<>% ungroup() %>%
    dplyr::select(
        source = experiment,
        grammatical,
        attractor_num,
        att_type,
        match,
        age,
        # condition,
        subject,
        trial_no,
        item,
        response_yes,
        RT,
        ResponseCorrect
    )
exp2.clean$experiment <- "Experiment 1"
exp2.clean$grammatical %<>% dplyr::recode(gram = "grammatical", ungram = "ungrammatical")
exp2.clean$attractor_num %<>% dplyr::recode(pl = "plural", sg = "singular")
exp2.clean$att_type %<>% dplyr::recode(gen = "gen", rc = "rc")
exp2.clean$item %<>% as.factor()
exp2.clean$subject %<>% as.character()

#
#
#
#| label: exp2-avgs


exp2.avgs <- exp2.clean %>%
    filter(match != "filler") %>%
    group_by(grammatical, attractor_num, att_type) %>%
    summarise(
        successes = sum(response_yes, na.rm = TRUE),
        N = sum(!is.na(response_yes)),
        .groups = "drop"
    ) %>%
    mutate(ci_mat = purrr::pmap(
        list(successes, N),
        ~ DescTools::BinomCI(x = ..1, n = ..2, conf.level = 0.95, method = "clopper-pearson")
    )) %>%
    mutate(ci_mat = purrr::map(ci_mat, ~ as_tibble(.))) %>%
    unnest(ci_mat) %>%
    mutate(
        p_hat = successes / N,
        lwr   = lwr.ci,
        upr   = upr.ci
    ) %>%
    select(grammatical, attractor_num, att_type, successes, N, p_hat, lwr, upr)

exp2.avgs.filler <- exp2.clean %>%
    filter(match == "filler") %>%
    group_by(grammatical, attractor_num, att_type) %>%
    summarise(
        successes = sum(ResponseCorrect == TRUE, na.rm = TRUE),
        N = sum(!is.na(ResponseCorrect)),
        .groups = "drop"
    ) %>%
    mutate(ci_mat = purrr::pmap(
        list(successes, N),
        ~ DescTools::BinomCI(x = ..1, n = ..2, conf.level = 0.95, method = "clopper-pearson")
    )) %>%
    mutate(ci_mat = purrr::map(ci_mat, ~ as_tibble(.))) %>%
    unnest(ci_mat) %>%
    mutate(
        p_hat = successes / N,
        lwr   = lwr.ci,
        upr   = upr.ci
    ) %>%
    select(grammatical, attractor_num, att_type, successes, N, p_hat, lwr, upr)


#
#
#
#| label: process-turklogacev2024
#| output: FALSE
source("../analysis/turklogacev24.R")
#
#
#
#| label: exp2-text-inputs

# I want accuracy, not the response yes
exp2.avgs.filler %<>%
    mutate(old.lwr = lwr, old.upr = upr) %>%
    mutate(
        p_hat = if_else(grammatical == "ungrammatical", 1 - p_hat, p_hat),
        lwr = if_else(grammatical == "ungrammatical", 1 - old.upr, old.lwr),
        upr = if_else(grammatical == "ungrammatical", 1 - old.lwr, old.upr)
    ) %>%
    select(-old.lwr, -old.upr)


exp2.nsubj <- exp2$subject %>%
    unique() %>%
    length()

exp2.nsubj.nontr <- exp2 %>%
    subset(natturk == "nat_non_turk") %>%
    .$subject %>%
    unique() %>%
    length()

exp2.nsubj.threshold <- 3

exp2.deletion <- round(100 * ((nrow(exp2.no.practice) - nrow(exp2.clean)) / nrow(exp2.no.practice)), 2)


exp2.meanage <- mean(asi(exp2.clean$age)) %>% round()
exp2.maxage <- max(asi(exp2.clean$age))
exp2.minage <- min(asi(exp2.clean$age))

# FILLER AVERAGES

exp2.avgs.filler %<>% mutate(text = paste0("M = ", round(p_hat, 2), ", CI = [", round(lwr, 2), ",", round(upr, 2), "]"))

exp2.avgs %<>% mutate(text = paste0("M = ", round(p_hat, 2), ", CI = [", round(lwr, 2), ",", round(upr, 2), "]"))


# Bind and set the desired x-axis order: rc → gen (current) → gen-tl (T&L 2024)
tl24.avgs$att_type <- "gen-tl"
all.avgs <- bind_rows(tl24.avgs, exp2.avgs) %>%
    dplyr::mutate(
        att_type = factor(att_type, levels = c("rc", "gen", "gen-tl"))
    )


#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: exp2-models

options(mc.cores = parallel::detectCores())
theme_set(theme_bw(base_family = "Times"))


exp2.dfModel <- exp2.clean %>% subset(match != "filler")

exp2.dfModel %<>% mutate(exp = "current") %>% droplevels()
tl24.gen <- tl24.clean %>%
    subset(match != "filler") %>%
    mutate(att_type = "gen-tl") %>%
    droplevels()

exp2.all <- bind_rows(exp2.dfModel, tl24.gen)


exp2.all %<>%
    mutate(
        grammatical = factor(grammatical,
            levels = c("grammatical", "ungrammatical"),
            labels = c("Grammatical", "Ungrammatical")
        ),
        attractor_num = factor(attractor_num, # or attractor_num if that’s your column
            levels = c("singular", "plural"),
            labels = c("Singular", "Plural")
        ),
        att_type = factor(att_type, # or attractor_num if that’s your column
            levels = c("gen", "gen-tl", "rc"),
            labels = c("Gen-Current", "Gen-TL24", "RC")
        ),
    )

# Sum-code ±0.5 and give readable column names
C2 <- contr.sum(2) / 2

Cg <- C2
colnames(Cg) <- "Gram_minus_Ungram" # β > 0 ⇒ Ungram > Gram (in log-odds of YES)
Ca <- C2
colnames(Ca) <- "Plural_minus_Singular" # β > 0 ⇒ Plural > Singular (log-odds of YES)
C3 <- matrix(
    c(
        # RC vs both Gens
        -1, -1, 2, # contrast 1
        # Gen-Current vs Gen-TL24
        1, -1, 0 # contrast 2
    ),
    ncol = 2
)

# Normalize to mean-centered (sum to 0, length-scaled)
C3 <- apply(C3, 2, function(x) x / sum(abs(x)) * 2 / 3)

colnames(C3) <- c("RC_vs_Gens", "GenCurrent_vs_GenTL24")
rownames(C3) <- c("Gen-Current", "Gen-TL24", "RC")

# C3
contrasts(exp2.all$att_type) <- C3


contrasts(exp2.all$grammatical) <- Cg
contrasts(exp2.all$attractor_num) <- -Ca


make_priors_generic <- function(
    f_mean = 0, f_sd = 1,
    intercept_mean = 0.85, intercept_sd = 0.70,
    exp_rate = 1, lkj_eta = 2) {
    c(
        # Intercept
        set_prior(sprintf("normal(%g, %g)", intercept_mean, intercept_sd), class = "Intercept"),
        set_prior(sprintf("normal(%g, %g)", f_mean, f_sd), class = "b"),
        # Random-effect scales and correlations
        set_prior(sprintf("exponential(%g)", exp_rate), class = "sd"),
        set_prior(sprintf("lkj(%g)", lkj_eta), class = "cor")
    )
}

priors_uninformative <- make_priors_generic(
    f_mean = 0, f_sd = 1,
    exp_rate = 1,
    lkj_eta = 2
)



m.exp2.all <- brm(
    bf(ResponseCorrect ~ grammatical * attractor_num * att_type +
        (1 + grammatical * attractor_num * att_type | subject) +
        (1 + grammatical * attractor_num * att_type | item), decomp = "QR"),
    data = exp2.all,
    family = bernoulli(link = "logit"),
    prior = priors_uninformative,
    threads = threading(4),
    chains = 4, iter = 3000, warmup = 1000,
    init = 0, file = "../models/m.exp2.all",
    seed = 1
)

m.exp2.all <- brm(
    bf(response_yes ~ grammatical * attractor_num * att_type +
        (1 + grammatical * attractor_num * att_type | subject) +
        (1 + grammatical * attractor_num * att_type | item), decomp = "QR"),
    data = exp2.all,
    family = bernoulli(link = "logit"),
    prior = priors_uninformative,
    threads = threading(4),
    chains = 4, iter = 3000, warmup = 1000,
    init = 0, file = "../models/m.exp2.all.yes",
    seed = 1
)

#
#
#
#| label: model-output-2

library(posterior)
library(glue)

summ_brms <- function(fit, par) {
    s <- posterior_summary(fit, pars = par)[1, ]
    c(est = unname(s["Estimate"]), l95 = unname(s["Q2.5"]), u95 = unname(s["Q97.5"]))
}
fmt <- function(x, d = 2) sprintf(paste0("%.", d, "f"), x)
trip <- function(v, d = 2) glue("{fmt(v['est'], d)} [{fmt(v['l95'], d)}, {fmt(v['u95'], d)}]")

p_gt0 <- function(fit, par) {
    sanitize_b <- function(par) paste0("", sub("b_", "", par))

    h <- hypothesis(fit, paste0(sanitize_b(par), " > 0"))
    as.numeric(h$hypothesis$Post.Prob)
}
get_col <- function(draws, name) {
    if (!name %in% names(draws)) {
        stop(sprintf("Column '%s' not found. Available: %s", name, paste(head(names(draws), 10), collapse = ", ")))
    }
    draws[[name]]
}

coef_names2 <- list(
    # main effects
    gram = "b_grammaticalGram_minus_Ungram",
    attr = "b_attractor_numPlural_minus_Singular",
    type_rc_gen = "b_att_typeRC_vs_Gens",
    type_genpair = "b_att_typeGenCurrent_vs_GenTL24",

    # two-way interactions
    gram_attr = "b_grammaticalGram_minus_Ungram:attractor_numPlural_minus_Singular",
    gram_type_rc = "b_grammaticalGram_minus_Ungram:att_typeRC_vs_Gens",
    gram_type_gen = "b_grammaticalGram_minus_Ungram:att_typeGenCurrent_vs_GenTL24",
    attr_type_rc = "b_attractor_numPlural_minus_Singular:att_typeRC_vs_Gens",
    attr_type_gen = "b_attractor_numPlural_minus_Singular:att_typeGenCurrent_vs_GenTL24",

    # three-way interactions
    way3_rc = "b_grammaticalGram_minus_Ungram:attractor_numPlural_minus_Singular:att_typeRC_vs_Gens",
    way3_gen = "b_grammaticalGram_minus_Ungram:attractor_numPlural_minus_Singular:att_typeGenCurrent_vs_GenTL24"
)

post_int2 <- lapply(coef_names2, \(nm) summ_brms(m.exp2.all, nm))
txt2 <- lapply(post_int2, trip)
txt_p2 <- lapply(coef_names2, \(nm) fmt(p_gt0(m.exp2.all, nm), 2))



# Extract posterior draws
draws <- as_draws_df(m.exp2.all)


# Coefficient names
b_ga_name <- "b_grammaticalGram_minus_Ungram:attractor_numPlural_minus_Singular"
b_gat_rc_name <- "b_grammaticalGram_minus_Ungram:attractor_numPlural_minus_Singular:att_typeRC_vs_Gens"
b_gat_gen_name <- "b_grammaticalGram_minus_Ungram:attractor_numPlural_minus_Singular:att_typeGenCurrent_vs_GenTL24"

b_ga <- get_col(draws, b_ga_name)
b_gat_rc <- get_col(draws, b_gat_rc_name)
b_gat_gen <- get_col(draws, b_gat_gen_name)

# Compute effects per att_type level (Helmert-coded)
# Levels: RC_vs_Gens (+ for RC, - for both Gens); GenCurrent_vs_GenTL24 (+ for GenCurrent, - for GenTL24)
# RC: +0.5 on RC_vs_Gens, 0 on GenCurrent_vs_GenTL24
# Gen-Current: -0.25 on RC_vs_Gens, +0.5 on GenCurrent_vs_GenTL24
# Gen-TL24: -0.25 on RC_vs_Gens, -0.5 on GenCurrent_vs_GenTL24

eff_rc <- b_ga + (1/3)  * b_gat_rc + 0      * b_gat_gen
eff_gencurrent <- b_ga + (-1/6) * b_gat_rc + (1/3)  * b_gat_gen
eff_gentl24 <- b_ga + (-1/6) * b_gat_rc + (-1/3) * b_gat_gen
prob_gt0 <- function(x) mean(x > 0)


# Summarize
predicted <- tibble(
    Condition = factor(c("RC", "Gen-Current", "Gen-TL24"),
        levels = c("RC", "Gen-Current", "Gen-TL24")
    ),
    mean = c(mean(eff_rc), mean(eff_gencurrent), mean(eff_gentl24)),
    l95 = c(quantile(eff_rc, 0.025), quantile(eff_gencurrent, 0.025), quantile(eff_gentl24, 0.025)),
    u95 = c(quantile(eff_rc, 0.975), quantile(eff_gencurrent, 0.975), quantile(eff_gentl24, 0.975)),
    P_gt0 = c(prob_gt0(eff_rc), prob_gt0(eff_gencurrent), prob_gt0(eff_gentl24)),
    P_lt0 = c(1 - prob_gt0(eff_rc), 1 - prob_gt0(eff_gencurrent), 1 - prob_gt0(eff_gentl24))
)

predicted <- predicted %>%
    mutate(
        # format p values: "<0.01", ">0.99", or rounded
        p_formatted = case_when(
            P_lt0 < 0.01 ~ "<0.01",
            P_lt0 > 0.99 ~ ">0.99",
            TRUE ~ paste0("=", sprintf("%.2f", round(P_lt0, 2)))
        ),

        # compose text string
        text = paste0(
            "M = ", round(mean, 2),
            ", CI = [", round(l95, 2), ", ", round(u95, 2), "]",
            ", P(<0) ", p_formatted
        )
    )


h <- hypothesis(
    m.exp2.all,
    c(
        # RC
        "(1*grammaticalGram_minus_Ungram:attractor_numPlural_minus_Singular
      + (1/3)*grammaticalGram_minus_Ungram:attractor_numPlural_minus_Singular:att_typeRC_vs_Gens
      + 0*grammaticalGram_minus_Ungram:attractor_numPlural_minus_Singular:att_typeGenCurrent_vs_GenTL24) < 0",
        # Gen-Current
        "(1*grammaticalGram_minus_Ungram:attractor_numPlural_minus_Singular
      + (-1/6)*grammaticalGram_minus_Ungram:attractor_numPlural_minus_Singular:att_typeRC_vs_Gens
      + (1/3)*grammaticalGram_minus_Ungram:attractor_numPlural_minus_Singular:att_typeGenCurrent_vs_GenTL24) < 0",
        # Gen-TL24
        "(1*grammaticalGram_minus_Ungram:attractor_numPlural_minus_Singular
      + (-1/6)*grammaticalGram_minus_Ungram:attractor_numPlural_minus_Singular:att_typeRC_vs_Gens
      + (-1/3)*grammaticalGram_minus_Ungram:attractor_numPlural_minus_Singular:att_typeGenCurrent_vs_GenTL24) < 0",
        # GenCurrent − GenTL24 difference
        "((2/3)*grammaticalGram_minus_Ungram:attractor_numPlural_minus_Singular:att_typeGenCurrent_vs_GenTL24) < 0"
    )
)

exp2_atts <- as_tibble(h$hypothesis) %>%
    transmute(
        contrast = c("RC", "Gen-Current", "Gen-TL24", "GenCurrent − GenTL24"),
        mean = Estimate, l95 = CI.Lower, u95 = CI.Upper,
        prob_lt0 = Post.Prob,
        text = paste0(
            "M = ", round(mean, 2),
            ", CI = [", round(l95, 2), ", ", round(u95, 2), "]",
            ", P(<0) = ",
            ifelse(is.na(prob_lt0), "NA",
                ifelse(prob_lt0 < 0.01, "<0.01",
                    ifelse(prob_lt0 > 0.99, ">0.99", round(prob_lt0, 2))
                )
            )
        )
    )

predicted <- exp2_atts %>%
    filter(contrast %in% c("RC", "Gen-Current", "Gen-TL24")) %>%
    transmute(
        Condition = recode(
            contrast,
            "RC" = "Attraction: Verbal\n(Current)",
            "Gen-Current" = "Attraction: Nominal\n(Current)",
            "Gen-TL24" = "Attraction: Nominal\n(Türk & Logačev 2024)"
        ),
        mean, l95, u95
    )

## 2) Pull the overall acceptability difference (Gen-Current vs Gen-TL24) from the model
fix <- posterior_summary(m.exp2.all, pars = "^b_") %>%
    as_tibble(rownames = "term")

# main effect of att_type GenCurrent_vs_GenTL24
genpair_row <- fix %>%
    filter(str_detect(term, "^b_att_type.*GenCurrent_vs_GenTL24$")) %>%
    slice(1)

coef_name <- genpair_row$term
# If this is empty, run: rownames(fixef(m.exp2.all)) and copy the exact name.

# 2) Compute P(<0) from draws
dr <- as_draws_df(m.exp2.all)
stopifnot(coef_name %in% names(dr))
prob_lt0 <- mean(dr[[coef_name]] < 0)

# 3) Build overall_df with prob and text
overall_df <- tibble(
    Condition = "Overall Acceptability:\nGen-Current − Gen-TL24",
    mean = genpair_row$Estimate,
    l95 = genpair_row$Q2.5,
    u95 = genpair_row$Q97.5,
)

## 3) Gen vs Gen difference in *attraction* from your exp2_atts (row 4)
diff_attr_df <- exp2_atts %>%
    filter(contrast == "GenCurrent − GenTL24") %>%
    transmute(
        Condition = "Attraction Difference:\nGen-Current − Gen-TL24",
        mean, l95, u95
    )

predicted_between <- bind_rows(diff_attr_df, overall_df, predicted)

overall_df <- overall_df %>%
    mutate(prob_lt0 = prob_lt0) %>%
    mutate(
        text = paste0(
            "M = ", round(mean, 2),
            ", CI = [", round(l95, 2), ", ", round(u95, 2), "]",
            ", P(<0) = ",
            ifelse(is.na(prob_lt0), "NA",
                ifelse(prob_lt0 < 0.01, "<0.01",
                    ifelse(prob_lt0 > 0.99, ">0.99", round(prob_lt0, 2))
                )
            )
        )
    )

predicted_between <- predicted_between %>%
    mutate(
        Condition = factor(
            Condition,
            levels = rev(c(
                "Attraction: Verbal\n(Current)", # A in RC
                "Attraction: Nominal\n(Current)", # A in Gen-Current
                "Attraction: Nominal\n(Türk & Logačev 2024)", # A in TL24
                "Attraction Difference:\nGen-Current − Gen-TL24", # Diff between Gens
                "Overall Acceptability:\nGen-Current − Gen-TL24" # Overall acceptability
            ))
        )
    )

p_between <- ggplot(predicted_between, aes(y = Condition, x = mean)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = l95, xmax = u95), height = 0.15, linewidth = 0.7) +
    xlab(expression(paste("Effect Size (", beta, ")"))) +
    ylab(NULL) +
    theme_minimal(base_family = "Times") +
    theme(
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()
    )

#
#
#
#
#
#
#
#
#
#
#| label: fig-exp2-condition-means
#| fig-cap: "Mean proportion of 'acceptable' responses by grammaticality, attractor number and attractor type. Error bars show 95% Clopper–Pearson confidence intervals. "
#| fig-width: 6
#| fig-height: 3

exp2.gram.label <- c(
    "grammatical"   = "Grammatical\n(Singular Verb)",
    "ungrammatical" = "Ungrammatical\n(Plural Verb)"
)
exp2.att_type.label <- c(
    "rc"     = "Verbal\n(Current Paper)",
    "gen"    = "Nominal\n(Current Paper)",
    "gen-tl" = "Nominal\n(TL2024)"
)

exp2.avgs %<>% droplevels()
# Plot
# 1. Create the dummy data frame with the correct facet levels and desired limits (in proportion).
facet_limits <- data.frame(
    grammatical = c(
        "grammatical",
        "grammatical",
        "ungrammatical",
        "ungrammatical"
    ),
    p_hat = c(0.75, 1.0, 0.0, 0.25)
)
# Plot: X = Attractor Type (ordered & labeled), facet = Grammaticality
all.avgs %>%
    ggplot(aes(
        x = att_type, y = p_hat,
        linetype = attractor_num, group = attractor_num
    )) +
    geom_point(position = position_dodge(0.3)) +
    geom_blank(data = facet_limits, aes(y = p_hat), inherit.aes = FALSE) +
    # geom_line() +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0, position = position_dodge(0.3)) +
    facet_wrap(~grammatical, labeller = as_labeller(exp2.gram.label), scale = "free_y") +
    scale_x_discrete(labels = exp2.att_type.label, drop = FALSE) +
    xlab("Attractor Type") +
    ylab("Percentage 'acceptable'") +
    scale_y_continuous(labels = scales::percent) +
    scale_linetype_discrete(
        name = "Attractor Number",
        labels = c("Singular", "Plural")
    ) +
    theme_minimal(base_family = "Times") +
    theme(
        legend.position = "bottom",
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 9),
    )

ggsave(
  filename = "fig-exp2-condition-means-1.pdf",
  plot = last_plot(),
  width = 6, 
  height = 3, 
  units = "in", 
  dpi = 600,       # 600 dpi for very high resolution
  bg = "white"     # Prevents transparent background
)

#
#
#
#
#
#
#
#| label: fig-exp2-fixed-effects
#| fig-cap: "Posterior summaries of attraction-related effects. Points indicate posterior means, and horizontal bars show 95% credible intervals on the log-odds (β) scale. Attraction was estimated as the interaction between grammaticality and attractor number within each attractor type. Negative values indicate stronger attraction (a reduced ungrammaticality penalty in plural-attractor conditions). Dashed line denotes zero (no effect)."
#| fig-width: 6
#| fig-height: 2

p_between
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: exp1-data-prep

exp1 <- read_experimental_data("../data/results.txt", subj_offset = 2000, item_offset = 2000)

exp1 %<>% mutate(exp_condition = case_when(
  exp_condition == "filler" & item_num <= 120 ~ "filler_ung",
  exp_condition == "filler" & item_num >= 121 ~ "filler_g",
  exp_condition == "practice" ~ "practice",
  exp_condition == "condition_b" ~ "condition_b",
  exp_condition == "condition_a" ~ "condition_a",
  exp_condition == "condition_c" ~ "condition_c",
  exp_condition == "condition_d" ~ "condition_d"
))


exp1.conditions <- data.frame(
  exp_condition = c("practice", "condition_a", "condition_b", "condition_c", "condition_d", "filler_ung", "filler_g"),
  experiment =    c("practice", "AgrAttr",     "AgrAttr",     "AgrAttr",     "AgrAttr",     "filler",     "filler"),
  condition =     c("practice", "a",           "b",           "c",           "d",           "filler_ung", "filler_g"),
  grammatical =   c("practice", "ungram",      "gram",        "ungram",      "gram",        "ungram",     "gram"),
  verb_num =      c("practice", "pl",          "sg",          "pl",          "sg",          "sg",         "pl"),
  attractor_num = c("practice", "pl",          "pl",          "sg",          "sg",          'filler',     'filler'),
  match =         c("practice", "mismatch",    "mismatch",    "match",       "match",       'filler',     'filler'),
  stringsAsFactors = T
)

exp1 %<>% left_join(exp1.conditions, by = "exp_condition")

exp1.no.practice <- exp1 %>% subset(exp_condition != "practice")

# accuracy: 0.25
# rt_below: 200
# rt_upper: 4999
exp1.clean <- exclude_bad_subjects(
  exp1,
  accuracy_threshold = 0.25,
  rt_below = 200,
  rt_upper = 4999
)

exp1.clean %<>% no_null_no_practice(.)

stopifnot(exp1.clean %>% subset(is.na(response_yes)) %>% nrow() == 0)

# DIFF DATA =====
exp1.diff <- dplyr::anti_join(exp1, exp1.clean) %>%
  filter(exp_condition != "practice")

exp1.clean$isGram <- ifelse(exp1.clean$grammatical == "ungram", F, T)
exp1.clean$p_acc <- with(exp1.clean, response_yes & isGram)
exp1.clean %<>% mutate(ResponseCorrect = (response_yes == (grammatical == "gram")))

# Merge

exp1.clean %<>% ungroup() %>%
                      dplyr::select(source=experiment,
                                    grammatical,
                                    attractor_num,
                                    match,
                                    age,
                                    # condition,
                                    subject,
                                    trial_no,
                                    item,
                                    response_yes,
                                    RT,
                                    ResponseCorrect)
exp1.clean$experiment <- "Experiment 1"
exp1.clean$grammatical %<>% dplyr::recode(gram="grammatical", ungram="ungrammatical")
exp1.clean$attractor_num %<>% dplyr::recode(pl="plural", sg="singular")
exp1.clean$item %<>% as.factor()
exp1.clean$subject %<>% as.character()

#
#
#
#| label: exp1-avgs

exp1.avgs <- exp1.clean %>%
  filter(match != "filler") %>%
  group_by(grammatical, attractor_num, match) %>%
  summarise(
    successes = sum(response_yes, na.rm = TRUE),
    N         = sum(!is.na(response_yes)),
    .groups = "drop"
  ) %>%
  mutate(ci_mat = purrr::pmap(
    list(successes, N),
    ~ DescTools::BinomCI(x = ..1, n = ..2, conf.level = 0.95, method = "clopper-pearson")
  )) %>%
  mutate(ci_mat = purrr::map(ci_mat, ~ as_tibble(.))) %>%
  unnest(ci_mat) %>%
  mutate(
    p_hat = successes / N,
    lwr   = lwr.ci,
    upr   = upr.ci
  ) %>%
  select(grammatical, attractor_num, match, successes, N, p_hat, lwr, upr)

exp1.avgs.filler <- exp1.clean %>%
  filter(match == "filler") %>%
  group_by(grammatical, attractor_num, match) %>%
  summarise(
    successes = sum(ResponseCorrect == TRUE, na.rm = TRUE),
    N         = sum(!is.na(ResponseCorrect)),
    .groups = "drop"
  ) %>%
  mutate(ci_mat = purrr::pmap(
    list(successes, N),
    ~ DescTools::BinomCI(x = ..1, n = ..2, conf.level = 0.95, method = "clopper-pearson")
  )) %>%
  mutate(ci_mat = purrr::map(ci_mat, ~ as_tibble(.))) %>%
  unnest(ci_mat) %>%
  mutate(
    p_hat = successes / N,
    lwr   = lwr.ci,
    upr   = upr.ci
  ) %>%
  select(grammatical, attractor_num, match, successes, N, p_hat, lwr, upr)


#
#
#
#| label: exp1-text-inputs

# I want accuracy, not the response yes
# exp1.avgs.filler %<>%
#   mutate(old.lwr = lwr, old.upr = upr) %>%
#   mutate(
#   p_hat = if_else(grammatical == "ungrammatical", 1 - p_hat, p_hat),
#   lwr = if_else(grammatical == "ungrammatical", 1 - old.upr, old.lwr),
#   upr = if_else(grammatical == "ungrammatical", 1 - old.lwr, old.upr))  %>%
#   select(-old.lwr, -old.upr)


exp1.nsubj <- exp1$subject %>% unique() %>% length()

exp1.nsubj.nontr <- exp1 %>%
  subset(natturk == "nat_non_turk") %>%
  .$subject %>%
  unique() %>%
  length()

exp1.nsubj.threshold <- 2

exp1.deletion <- round(100*((nrow(exp1.no.practice)-nrow(exp1.clean))  / nrow(exp1.no.practice)),2)


exp1.meanage <- mean(asi(exp1.clean$age)) %>% round()
exp1.maxage <- max(asi(exp1.clean$age))
exp1.minage <- min(asi(exp1.clean$age))

# FILLER AVERAGES

exp1.avgs.filler %<>% mutate(text = paste0("M = ", round(p_hat,2), ", CI = [", round(lwr,2) , ",", round(upr,2) ,"]"))

exp1.avgs %<>% mutate(text = paste0("M = ", round(p_hat,2), ", CI = [", round(lwr,2) , ",", round(upr,2) ,"]"))

#
#
#
#
#
#
#
#
#
#| label: exp1-models

options(mc.cores = parallel::detectCores())
theme_set(theme_bw(base_family = "Times"))


exp1.dfModel <- exp1.clean %>% subset(match != "filler")


exp1.dfModel %<>%
    mutate(
        grammatical = factor(grammatical,
            levels = c("grammatical", "ungrammatical"),
            labels = c("Grammatical", "Ungrammatical")
        ),
        attractor_num = factor(attractor_num, # or attractor_num if that’s your column
            levels = c("singular", "plural"),
            labels = c("Singular", "Plural")
        )
    )

# Sum-code ±0.5 and give readable column names
C2 <- contr.sum(2) / 2

Cg <- C2
colnames(Cg) <- "Gram_minus_Ungram" # β > 0 ⇒ Ungram > Gram (in log-odds of YES)
Ca <- C2
colnames(Ca) <- "Plural_minus_Singular" # β > 0 ⇒ Plural > Singular (log-odds of YES)

contrasts(exp1.dfModel$grammatical) <- Cg
contrasts(exp1.dfModel$attractor_num) <- -Ca


make_priors <- function(
    inter_ga_mean = 0.0, inter_ga_sd = 0.10, # Gram × Attr (classic attraction term)
    main_g_mean = 1.0, main_g_sd = 0.50, # Grammaticality main effect (your previous spec)
    main_a_mean = 0.30, main_a_sd = 0.40, # Attractor Number main effect
    intercept_mean = 0.85, intercept_sd = 0.70,
    exp_rate = 1, lkj_eta = 2) {
    c(
        # Intercept
        set_prior(sprintf("normal(%g, %g)", intercept_mean, intercept_sd), class = "Intercept"),

        # Main effects
        set_prior(sprintf("normal(%g, %g)", main_g_mean, main_g_sd),
            class = "b", coef = "grammaticalGram_minus_Ungram"
        ),
        set_prior(sprintf("normal(%g, %g)", main_a_mean, main_a_sd),
            class = "b", coef = "attractor_numPlural_minus_Singular"
        ),
        # Two-way interactions
        set_prior(sprintf("normal(%g, %g)", inter_ga_mean, inter_ga_sd),
            class = "b", coef = "grammaticalGram_minus_Ungram:attractor_numPlural_minus_Singular"
        ),
        # Random-effect scales and correlations
        set_prior(sprintf("exponential(%g)", exp_rate), class = "sd"),
        set_prior(sprintf("lkj(%g)", lkj_eta), class = "cor")
    )
}

priors_uninformative <- make_priors(
    inter_ga_mean   = 0, inter_ga_sd   = 1,
    main_g_mean     = 0, main_g_sd     = 1,
    main_a_mean     = 0, main_a_sd     = 1,
    intercept_mean  = 0, intercept_sd  = 1,
    exp_rate        = 1,
    lkj_eta         = 2
)


m.exp1 <- brm(
    response_yes ~ grammatical * attractor_num +
        (1 + grammatical * attractor_num | subject) +
        (1 + grammatical * attractor_num | item),
    data = exp1.dfModel,
    family = bernoulli(link = "logit"),
    prior = priors_uninformative,
    sample_prior = "yes", file = "../models/m.exp1",
    save_pars = save_pars(all = TRUE),
    chains = 4, iter = 10000, warmup = 2500, seed = 1
)



#
#
#
#| label: model-output


library(posterior)
library(glue)

# --- helpers ---
coef_names <- list(
    gram  = "b_grammaticalGram_minus_Ungram",
    attr  = "b_attractor_numPlural_minus_Singular",
    inter = "b_grammaticalGram_minus_Ungram:attractor_numPlural_minus_Singular"
)

post_int <- list(
    gram  = summ_brms(m.exp1, coef_names$gram),
    attr  = summ_brms(m.exp1, coef_names$attr),
    inter = summ_brms(m.exp1, coef_names$inter)
)
txt <- list(
    gram  = trip(post_int$gram),
    attr  = trip(post_int$attr),
    inter = trip(post_int$inter)
)


txt_p <- list(
    gram  = fmt(p_gt0(m.exp1, coef_names$gram), 2),
    attr  = fmt(p_gt0(m.exp1, coef_names$attr), 2),
    inter = fmt(p_gt0(m.exp1, coef_names$inter), 2)
)

#
#
#
#| label: nested-models


grammaticals <- exp1.dfModel %>% filter(grammatical == "Grammatical")

ungrammaticals <- exp1.dfModel %>% filter(grammatical == "Ungrammatical")

make_priors_g <- function(inter_mean = 0.4, inter_sd = 0.1,
                        exp_rate = 1, lkj_eta = 2) {
    c(
        set_prior("normal(0.85, 0.7)", class = "Intercept"),
        set_prior("normal(0.30, 0.40)",
            class = "b",
            coef = "attractor_numPlural_minus_Singular"
        ),
        set_prior(sprintf("exponential(%g)", exp_rate), class = "sd"),
        set_prior(sprintf("lkj(%g)", lkj_eta), class = "cor")
    )
}

m.exp1.g <- brm(
    response_yes ~ attractor_num +
        (1 + attractor_num | subject) +
        (1 + attractor_num | item),
    data = grammaticals,
    family = bernoulli(link = "logit"),
    prior = make_priors_g(),
    sample_prior = "yes", file = "../models/m.exp1.g",
    save_pars = save_pars(all = TRUE),
    chains = 4, iter = 4000, warmup = 2000, seed = 1
)


m.exp1.u <- brm(
    response_yes ~ attractor_num +
        (1 + attractor_num | subject) +
        (1 + attractor_num | item),
    data = ungrammaticals,
    family = bernoulli(link = "logit"),
    prior = make_priors_g(),
    sample_prior = "yes", file = "../models/m.exp1.u",
    save_pars = save_pars(all = TRUE),
    chains = 4, iter = 4000, warmup = 2000, seed = 1
)

post_int_g <- list(
    attr_g  = summ_brms(m.exp1.g, coef_names$attr),
    attr_u  = summ_brms(m.exp1.u, coef_names$attr)
)

txt_g <- list(
    attr_g = trip(post_int_g$attr_g),
    attr_u = trip(post_int_g$attr_u)
)

txt_g_p <- list(
    attr_g = fmt(p_gt0(m.exp1.g, coef_names$attr), 2),
    attr_u = fmt(p_gt0(m.exp1.u, coef_names$attr), 2)
)



#
#
#
#
#
#
#
#
#
#| label: fig-exp1-condition-means
#| fig-cap: "Mean proportion of 'acceptable' responses by grammaticality and attractor number. Error bars show 95% Clopper–Pearson confidence intervals. "
#| fig-width: 6
#| fig-height: 3.5

exp1.gram.label <- c(
    grammatical = "Grammatical\n(Singular Verb)",
    ungrammatical = "Ungrammatical\n(Plural Verb)"
)

facet_limits <- data.frame(
    grammatical = c(
        "grammatical",
        "grammatical",
        "ungrammatical",
        "ungrammatical"
    ),
    p_hat = c(0.75, 1.0, 0.0, 0.25)
)

# responses

exp1.avgs %>%
    ggplot(aes(grammatical, p_hat,
        linetype = attractor_num,
        group = attractor_num
    )) +
    geom_point(position = position_dodge(0.3)) +
    geom_blank(data = facet_limits, aes(y = p_hat), inherit.aes = FALSE) +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0, position = position_dodge(0.3)) +
    facet_wrap(~grammatical, labeller = as_labeller(exp1.gram.label), scale = "free") +
    ylab("Percentage 'acceptable'") +
    scale_y_continuous(labels = scales::percent) +
    scale_linetype_discrete(
        name = "Attractor Number",
        labels = c("Plural", "Singular")
    ) +
    theme_minimal(base_family = "Times") +
    theme(
        legend.position = "bottom",
        strip.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank()
    )

ggsave(
  filename = "fig-exp1-condition-means-1.pdf",
  plot = last_plot(),
  width = 6, 
  height = 3, 
  units = "in", 
  dpi = 600,       # 600 dpi for very high resolution
  bg = "white"     # Prevents transparent background
)

#
#
#
#
#
#
#
#| label: fig-exp1-fixed-effects
#| fig-cap: "Posterior means and 95% credible intervals for fixed effects in the two Bayesian models. The x-axis shows the posterior mean (log-odds scale). The blue intervals correspond to the model in which a positive interaction was assumed, and the orange intervals to the model in which it was not. "
#| fig-width: 6
#| fig-height: 2

fixef_whiskers <- function(fit, label) {
    posterior_summary(fit, pars = "^b_") %>%
        as_tibble(rownames = "term") %>%
        filter(term != "b_Intercept") %>%
        transmute(
            term,
            est = Estimate,
            l95 = Q2.5,
            u95 = Q97.5,
            model = label
        )
}

lab_no_int <- "Not assumed\nN(0,0.25)"
lab_int <- "Assumed\nN(0.4,0.25)"

df_plot <- bind_rows(
    fixef_whiskers(m.exp1, "Uninformative")
) %>%
    mutate(
        term_clean = case_when(
            term == "b_grammaticalGram_minus_Ungram" ~ "Grammaticality",
            term == "b_attractor_numPlural_minus_Singular" ~ "Attractor",
            term == "b_grammaticalGram_minus_Ungram:attractor_numPlural_minus_Singular" ~ "Interaction",
            TRUE ~ term
        ),
        term_clean = factor(term_clean,
            levels = rev(c("Grammaticality", "Attractor", "Interaction"))
        )
    )

ggplot(df_plot, aes(x = est, y = term_clean)) +
    geom_vline(xintercept = 0, linetype = 3) +
    geom_errorbarh(aes(xmin = l95, xmax = u95),
        position = position_dodge(width = 0.5), height = 0.2
    ) +
    geom_point(position = position_dodge(width = 0.5), size = 2.4) +
    labs(
        x = "Posterior (log-odds)",
        y = NULL,
        color = "Interaction",
    ) +
    theme_minimal(base_size = 10, base_family = "Times") +
    theme(panel.grid.minor = element_blank())

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

#
#
#| label: exp1-data-prep

exp1 <- read_experimental_data("../../utility/data/results_8cond.txt", subj_offset = 2500, item_offset = 2500)

exp1 %<>% mutate(exp_condition = case_when(
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


exp1.conditions <- data.frame(
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

exp1 %<>% left_join(exp1.conditions, by = "exp_condition")

exp1.no.practice <- exp1 %>% subset(exp_condition != "practice")

# accuracy: 0.25
# rt_below: 200
# rt_upper: 4999
exp1.clean <- exclude_bad_subjects_8(
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
exp1.clean$experiment <- "Experiment 1"
exp1.clean$grammatical %<>% dplyr::recode(gram = "grammatical", ungram = "ungrammatical")
exp1.clean$attractor_num %<>% dplyr::recode(pl = "plural", sg = "singular")
exp1.clean$att_type %<>% dplyr::recode(gen = "gen", rc = "rc")
exp1.clean$item %<>% as.factor()
exp1.clean$subject %<>% as.character()

#
#
#
#| label: exp1-avgs


exp1.avgs <- exp1.clean %>%
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

exp1.avgs.filler <- exp1.clean %>%
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
source("../../utility/hsp/turklogacev24.R")
#
#
#
#| label: exp1-text-inputs

# I want accuracy, not the response yes
exp1.avgs.filler %<>%
    mutate(old.lwr = lwr, old.upr = upr) %>%
    mutate(
        p_hat = if_else(grammatical == "ungrammatical", 1 - p_hat, p_hat),
        lwr = if_else(grammatical == "ungrammatical", 1 - old.upr, old.lwr),
        upr = if_else(grammatical == "ungrammatical", 1 - old.lwr, old.upr)
    ) %>%
    select(-old.lwr, -old.upr)


exp1.acc.threshold <- 0.25

exp1.subj_screen <- exp1 %>%
    group_by(subject, experiment, condition, grammatical, verb_num, attractor_num, att_type) %>%
    summarise(
        p_yes = mean(response_yes, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(expcond = paste(experiment, condition, sep = "_")) %>%
    dplyr::select(
        -experiment,
        -condition,
        -grammatical,
        -verb_num,
        -attractor_num,
        -att_type
    ) %>%
    tidyr::pivot_wider(
        names_from = expcond,
        values_from = p_yes
    ) %>%
    mutate(
        delta_gen_dc = AgrAttr_gen_d - AgrAttr_gen_c,
        delta_rc_dc = AgrAttr_rc_d - AgrAttr_rc_c
    )

exp1.bad_subjects_gen <- exp1.subj_screen %>%
    filter(delta_gen_dc <= exp1.acc.threshold) %>%
    pull(subject) %>%
    unique()

exp1.bad_subjects_rc <- exp1.subj_screen %>%
    filter(delta_rc_dc <= exp1.acc.threshold) %>%
    pull(subject) %>%
    unique()

exp1.bad_subjects_both <- intersect(exp1.bad_subjects_gen, exp1.bad_subjects_rc)
exp1.bad_subjects_any <- union(exp1.bad_subjects_gen, exp1.bad_subjects_rc)

exp1.n_bad_gen <- length(exp1.bad_subjects_gen)
exp1.n_bad_rc <- length(exp1.bad_subjects_rc)
exp1.n_bad_both <- length(exp1.bad_subjects_both)
exp1.n_bad_any <- length(exp1.bad_subjects_any)

exp1.nsubj.raw <- n_distinct(exp1$subject)
exp1.nsubj <- exp1.nsubj.raw
exp1.nsubj.analysis <- n_distinct(exp1.clean$subject)
exp1.p_subj_excluded <- round(100 * exp1.n_bad_any / exp1.nsubj.raw, 2)

exp1.nsubj.nontr <- exp1 %>%
    subset(natturk == "nat_non_turk") %>%
    .$subject %>%
    unique() %>%
    length()

exp1.after_subj <- exp1 %>%
    filter(!subject %in% exp1.bad_subjects_any)
exp1.n_rows_removed_subject <- nrow(exp1) - nrow(exp1.after_subj)
exp1.after_subj_nonpractice <- exp1.after_subj %>%
    filter(exp_condition != "practice")
exp1.n_rows_removed_subject_nonpractice <- nrow(exp1.no.practice) - nrow(exp1.after_subj_nonpractice)

exp1.n_rt_fast <- exp1.after_subj_nonpractice %>% filter(RT <= 200) %>% nrow()
exp1.n_rt_slow <- exp1.after_subj_nonpractice %>% filter(RT >= 4999) %>% nrow()
exp1.n_removed_rt <- exp1.n_rt_fast + exp1.n_rt_slow
exp1.p_removed_rt <- round(100 * exp1.n_removed_rt / nrow(exp1.after_subj_nonpractice), 2)

exp1.after_subj_rt_nonpractice <- exp1.after_subj_nonpractice %>%
    filter(RT < 4999 & RT > 200)

exp1.n_trials.analysis <- nrow(exp1.clean)
exp1.n_trials.raw <- nrow(exp1.no.practice)
exp1.n_trials.removed_total <- exp1.n_trials.raw - exp1.n_trials.analysis
exp1.p_trials.removed_total <- round(100 * exp1.n_trials.removed_total / exp1.n_trials.raw, 2)
exp1.n_missing_trials <- exp1.after_subj_rt_nonpractice %>% filter(is.na(response_yes)) %>% nrow()

exp1.deletion <- round(100 * ((nrow(exp1.no.practice) - nrow(exp1.clean)) / nrow(exp1.no.practice)), 2)

exp1.meanage <- mean(asi(exp1.clean$age)) %>% round()
exp1.maxage <- max(asi(exp1.clean$age))
exp1.minage <- min(asi(exp1.clean$age))

# FILLER AVERAGES

exp1.avgs.filler %<>% mutate(text = paste0("M = ", round(p_hat, 2), ", CI = [", round(lwr, 2), ",", round(upr, 2), "]"))

exp1.avgs %<>% mutate(text = paste0("M = ", round(p_hat, 2), ", CI = [", round(lwr, 2), ",", round(upr, 2), "]"))


# Bind and set the desired x-axis order: rc → gen (current) → gen-tl (T&L 2024)
tl24.avgs$att_type <- "gen-tl"
all.avgs <- bind_rows(tl24.avgs, exp1.avgs) %>%
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
#
#
#
#
#
#| label: exp1-models

options(mc.cores = parallel::detectCores())
theme_set(theme_bw(base_family = "Times"))


exp1.dfModel <- exp1.clean %>% subset(match != "filler")

exp1.dfModel %<>% mutate(exp = "current") %>% droplevels()
tl24.gen <- tl24.clean %>%
    subset(match != "filler") %>%
    mutate(att_type = "gen-tl") %>%
    droplevels()

exp1.all <- bind_rows(exp1.dfModel, tl24.gen)


exp1.all %<>%
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
contrasts(exp1.all$att_type) <- C3


contrasts(exp1.all$grammatical) <- Cg
contrasts(exp1.all$attractor_num) <- -Ca


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

m.exp1.all <- brm(
    bf(response_yes ~ grammatical * attractor_num * att_type +
        (1 + grammatical * attractor_num * att_type | subject) +
        (1 + grammatical * attractor_num * att_type | item), decomp = "QR"),
    data = exp1.all,
    family = bernoulli(link = "logit"),
    prior = priors_uninformative,
    sample_prior = "yes",
    save_pars = save_pars(all = TRUE),
    threads = threading(4),
    chains = 4, iter = 12000, warmup = 2000,
    init = 0, file = "../../utility/models/m.exp1.all.yes",
    seed = 1
)

#
#
#
#| label: model-output-2

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

post_int2 <- lapply(coef_names2, \(nm) summ_brms(m.exp1.all, nm))
txt2 <- lapply(post_int2, trip)
txt_p2 <- lapply(coef_names2, \(nm) fmt(p_gt0(m.exp1.all, nm), 2))



# Extract posterior draws
draws <- as_draws_df(m.exp1.all)


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
    m.exp1.all,
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
        # GenCurrent - GenTL24 difference
        "((2/3)*grammaticalGram_minus_Ungram:attractor_numPlural_minus_Singular:att_typeGenCurrent_vs_GenTL24) < 0"
    )
)

exp1_atts <- as_tibble(h$hypothesis) %>%
    transmute(
        contrast = c("RC", "Gen-Current", "Gen-TL24", "GenCurrent - GenTL24"),
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

predicted <- exp1_atts %>%
    filter(contrast %in% c("RC", "Gen-Current", "Gen-TL24")) %>%
    transmute(
        Condition = recode(
            contrast,
            "RC" = "Attraction: Verbal\n(Current)",
            "Gen-Current" = "Attraction: Nominal\n(Current)",
            "Gen-TL24" = "Attraction: Nominal\n(Turk & Logacev 2024)"
        ),
        mean, l95, u95
    )

## 2) Pull the overall acceptability difference (Gen-Current vs Gen-TL24) from the model
fix <- posterior_summary(m.exp1.all, pars = "^b_") %>%
    as_tibble(rownames = "term")

# main effect of att_type GenCurrent_vs_GenTL24
genpair_row <- fix %>%
    filter(str_detect(term, "^b_att_type.*GenCurrent_vs_GenTL24$")) %>%
    slice(1)

coef_name <- genpair_row$term
# If this is empty, run: rownames(fixef(m.exp1.all)) and copy the exact name.

# 2) Compute P(<0) from draws
dr <- as_draws_df(m.exp1.all)
stopifnot(coef_name %in% names(dr))
prob_lt0 <- mean(dr[[coef_name]] < 0)

# 3) Build overall_df with prob and text
overall_df <- tibble(
    Condition = "Overall Acceptability:\nGen-Current - Gen-TL24",
    mean = genpair_row$Estimate,
    l95 = genpair_row$Q2.5,
    u95 = genpair_row$Q97.5,
)

## 3) Gen vs Gen difference in *attraction* from your exp1_atts (row 4)
diff_attr_df <- exp1_atts %>%
    filter(contrast == "GenCurrent - GenTL24") %>%
    transmute(
        Condition = "Attraction Difference:\nGen-Current - Gen-TL24",
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
                "Attraction: Nominal\n(Turk & Logacev 2024)", # A in TL24
                "Attraction Difference:\nGen-Current - Gen-TL24", # Diff between Gens
                "Overall Acceptability:\nGen-Current - Gen-TL24" # Overall acceptability
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
#| label: fig-exp1-condition-means
#| fig-cap: "Mean proportion of 'acceptable' responses by grammaticality, attractor number and attractor type. Error bars show 95% Clopper–Pearson confidence intervals. "
#| fig-width: 6
#| fig-height: 3

exp1.gram.label <- c(
    "grammatical"   = "Grammatical\n(Singular Verb)",
    "ungrammatical" = "Ungrammatical\n(Plural Verb)"
)
exp1.att_type.label <- c(
    "rc"     = "Verbal\n(Current Paper)",
    "gen"    = "Nominal\n(Current Paper)",
    "gen-tl" = "Nominal\n(TL2024)"
)

exp1.avgs %<>% droplevels()
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
    facet_wrap(~grammatical, labeller = as_labeller(exp1.gram.label), scale = "free_y") +
    scale_x_discrete(labels = exp1.att_type.label, drop = FALSE) +
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
  filename = "../../utility/figures/fig-exp1-condition-means.pdf",
  plot = last_plot(),
  width = 6, 
  height = 3, 
  units = "in", 
  dpi = 600,
  bg = "white"
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
#| label: fig-exp1-fixed-effects
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
#| label: exp1-bayes-factor
#| eval: false

# For verbal attractors (RC), we need to test the cell-specific attraction effect
# The attraction effect for RC is computed from the posterior draws
# We use the hypothesis() approach to get the effect and test it

# Compute BF for the three-way interaction involving RC
# This tests whether RC differs from other conditions in attraction
bf_three_way <- bayestestR::bayesfactor_parameters(
    m.exp1.all,
    null = 0,
    effects = "fixed"
)

# Extract BF for the three-way interaction with RC
bf_3way_df <- bf_three_way %>%
    as.data.frame() %>%
    filter(grepl("Gram_minus_Ungram:attractor_numPlural_minus_Singular:att_typeRC_vs_Gens", Parameter))

bf_3way_value <- bf_3way_df$BF[1]
bf01_3way <- 1 / bf_3way_value  # Convert to BF01

# Also test the main attraction effect averaged across conditions
bf_2way_df <- bf_three_way %>%
    as.data.frame() %>%
    filter(grepl("^grammaticalGram_minus_Ungram:attractor_numPlural_minus_Singular$", Parameter))

bf_2way_value <- bf_2way_df$BF[1]
bf01_2way <- 1 / bf_2way_value

# Format for reporting
format_bf <- function(bf) {
    ifelse(bf > 100, ">100", 
           ifelse(bf < 0.01, "<0.01", sprintf("%.2f", bf)))
}

bf01_3way_txt <- format_bf(bf01_3way)
bf01_2way_txt <- format_bf(bf01_2way)

# Interpretation
interpret_bf <- function(bf) {
    case_when(
        bf > 100 ~ "extreme evidence for null",
        bf > 30 ~ "very strong evidence for null",
        bf > 10 ~ "strong evidence for null",
        bf > 3 ~ "moderate evidence for null",
        bf > 1 ~ "anecdotal evidence for null",
        bf > 1/3 ~ "anecdotal evidence against null",
        bf > 1/10 ~ "moderate evidence against null",
        TRUE ~ "strong evidence against null"
    )
}

bf_3way_interp <- interpret_bf(bf01_3way)

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

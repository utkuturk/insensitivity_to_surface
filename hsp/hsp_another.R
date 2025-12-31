# HSP Analysis (Experiment 2: Verbal Attractors)
library(tidyverse)
library(magrittr)
library(brms)
library(ggplot2)
library(desc)
library(posterior)
library(glue)
library(DescTools)

source("002_functions.R")
library(dplyr)

# 1. LOAD AND FILTER DATA
exp2 <- read_experimental_data(
    "../data/results.txt",
    subj_offset = 2500,
    item_offset = 2500
)

# Based on paper.qmd and hsp.R structure:
# condition_a: Ungrammatical, Plural Attractor (mismatch)
# condition_b: Grammatical, Plural Attractor (mismatch)
# condition_c: Ungrammatical, Singular Attractor (match)
# condition_d: Grammatical, Singular Attractor (match)

exp2 %<>%
    mutate(
        exp_condition = case_when(
            exp_condition == "filler" & item_num <= 120 ~ "filler_ung",
            exp_condition == "filler" & item_num >= 121 ~ "filler_g",
            exp_condition == "practice" ~ "practice",
            exp_condition == "condition_a" ~ "condition_a",
            exp_condition == "condition_b" ~ "condition_b",
            exp_condition == "condition_c" ~ "condition_c",
            exp_condition == "condition_d" ~ "condition_d"
        )
    )

exp2.conditions <- data.frame(
    exp_condition = c(
        "practice",
        "condition_a",
        "condition_b",
        "condition_c",
        "condition_d",
        "filler_ung",
        "filler_g"
    ),
    experiment = c(
        "practice",
        "AgrAttr",
        "AgrAttr",
        "AgrAttr",
        "AgrAttr",
        "filler",
        "filler"
    ),
    condition = c(
        "practice",
        "a",
        "b",
        "c",
        "d",
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
        "gram"
    ),
    verb_num = c(
        "practice",
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
        "filler",
        "filler"
    ),
    match = c(
        "practice",
        "mismatch",
        "mismatch",
        "match",
        "match",
        "filler",
        "filler"
    ),
    stringsAsFactors = TRUE
)

exp2 %<>% left_join(exp2.conditions, by = "exp_condition")

# Clean subjects
# Note: exclude_bad_subjects function in 002_functions.R seems appropriate for 4-condition design
# Check 002_functions.R content again if needed, but 'exclude_bad_subjects' (not _8) is likely the one.
# However, hsp.R used exclude_bad_subjects_8.
# Let's use exclude_bad_subjects since we have conditions a,b,c,d which form one set.
# The exclude_bad_subjects function calculates delta_dc = AgrAttr_d - AgrAttr_c (Gram-Match vs Ungram-Match)
exp2.clean <- exclude_bad_subjects(
    exp2,
    accuracy_threshold = 0.25,
    rt_below = 200,
    rt_upper = 4999
)

exp2.clean %<>% no_null_no_practice(.)

exp2.clean$isGram <- ifelse(exp2.clean$grammatical == "ungram", FALSE, TRUE)
exp2.clean %<>%
    mutate(ResponseCorrect = (response_yes == (grammatical == "gram")))

# Prepare for analysis/plotting
exp2.clean %<>%
    ungroup() %>%
    dplyr::select(
        source = experiment,
        grammatical,
        attractor_num,
        match,
        age,
        subject,
        item,
        response_yes,
        RT,
        ResponseCorrect
    )

exp2.clean$grammatical %<>%
    dplyr::recode(gram = "grammatical", ungram = "ungrammatical")
exp2.clean$attractor_num %<>% dplyr::recode(pl = "plural", sg = "singular")
exp2.clean$item %<>% as.factor()
exp2.clean$subject %<>% as.character()


# 2. PLOT DESCRIPTIVE
# Calculate averages
exp2.avgs <- exp2.clean %>%
    dplyr::filter(match != "filler") %>%
    group_by(grammatical, attractor_num) %>%
    summarise(
        successes = sum(response_yes == TRUE, na.rm = TRUE),
        N = sum(!is.na(response_yes)),
        .groups = "drop"
    ) %>%
    mutate(
        ci_mat = purrr::pmap(
            list(successes, N),
            ~ DescTools::BinomCI(
                x = ..1,
                n = ..2,
                conf.level = 0.95,
                method = "clopper-pearson"
            )
        )
    ) %>%
    mutate(ci_mat = purrr::map(ci_mat, ~ as_tibble(.))) %>%
    unnest(ci_mat) %>%
    mutate(
        p_hat = successes / N,
        lwr = lwr.ci,
        upr = upr.ci
    )

# Labels
exp2.gram.label <- c(
    "grammatical" = "Grammatical\n(Singular Verb)",
    "ungrammatical" = "Ungrammatical\n(Plural Verb)"
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
    p_hat = c(0.8, 1.0, 0.0, 0.2)
)

# 2. Plotting code
p_desc <- exp2.avgs %>%
    ggplot(aes(
        x = attractor_num,
        y = p_hat,
        ymin = lwr,
        ymax = upr
    )) +
    geom_point(size = 3) +
    geom_errorbar(
        width = 0.1
    ) +
    geom_blank(data = facet_limits, aes(y = p_hat), inherit.aes = FALSE) +
    facet_wrap(
        ~grammatical,
        labeller = as_labeller(exp2.gram.label),
        scale = "free_y"
    ) +
    xlab("Attractor Number") +
    ylab("Proportion 'Yes' Responses") +
    scale_y_continuous(labels = scales::percent) +
    theme_bw() +
    theme(
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)
    )

ggsave("../figures/hsp_another_desc_plot.png", p_desc, width = 6, height = 4)


# 3. FIT BRMS MODEL
exp2.dfModel <- exp2.clean %>% subset(match != "filler") %>% droplevels()

# Set simple sum contrasts (centered)
contrasts(exp2.dfModel$grammatical) <- contr.sum(2)
contrasts(exp2.dfModel$attractor_num) <- contr.sum(2)

# Fit model
m.exp2 <- brm(
    bf(
        response_yes ~ grammatical *
            attractor_num +
            (1 + grammatical * attractor_num | subject) +
            (1 + grammatical * attractor_num | item),
        decomp = "QR"
    ),
    data = exp2.dfModel,
    family = bernoulli(link = "logit"),
    threads = threading(4),
    chains = 4,
    iter = 4000,
    warmup = 2000,
    seed = 1,
    file = "../models/m.hsp_another"
)

# 4. PLOT MODEL COEFFICIENTS
draws <- as_draws_df(m.exp2)

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

# Explicit names
# Note: Interpreting main effects with Sum coding (-1, 1):
coeffs <- bind_rows(
    summarize_draws(draws$b_grammatical1, "Grammaticality"),
    summarize_draws(draws$b_attractor_num1, "Attractor Number"),
    summarize_draws(draws$`b_grammatical1:attractor_num1`, "Interaction")
)


makeppp <- function(p) {
    label = ifelse(p == 0, "<0.001", paste0("=", round(p, 2)))
    label = ifelse(p == 1, ">0.99", paste0("=", round(p, 2)))
    label
}

# Format label with P(>0)
coeffs %<>%
    mutate(
        label = glue("{term}\nP(>0){makeppp(p_gt0)}")
    )

p_model <- coeffs %>%
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
    theme(
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10)
    )

ggsave("../figures/hsp_another_model_plot.png", p_model, width = 4, height = 2)

print("Analysis Complete. Plots saved.")

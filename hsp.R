# HSP Analysis
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
    "./data/results_8cond.txt",
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

# Clean subjects
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

# Prepare for analysis/plotting
exp2.clean %<>%
    ungroup() %>%
    dplyr::select(
        source = experiment,
        grammatical,
        attractor_num,
        att_type,
        match,
        age,
        subject,
        item,
        response_yes,
        RT,
        ResponseCorrect
    )

exp2.clean %<>% filter(attractor_num != "filler")
exp2.clean %<>% drop.levels()
exp2.clean$grammatical %<>%
    dplyr::recode(gram = "grammatical", ungram = "ungrammatical")
exp2.clean$attractor_num %<>% dplyr::recode(pl = "plural", sg = "singular")
exp2.clean$att_type %<>% dplyr::recode(gen = "gen", rc = "rc")
exp2.clean$item %<>% as.factor()
exp2.clean$subject %<>% as.character()


# 2. PLOT DESCRIPTIVE
# Calculate averages
exp2.avgs <- exp2.clean %>%
    dplyr::filter(match != "filler") %>%
    group_by(grammatical, attractor_num, att_type) %>%
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
exp2.att_type.label <- c(
    "rc" = "Verbal",
    "gen" = "Nominal"
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
    # p_hat must be a value that forces the y-axis to extend to the desired max/min
    p_hat = c(0.8, 1.0, 0.0, 0.2)
)

# 2. Plotting code with geom_blank
p_desc <- exp2.avgs %>%
    ggplot(aes(
        x = att_type,
        y = p_hat,
        linetype = attractor_num,
        group = attractor_num
    )) +
    geom_point(position = position_dodge(0.3)) +
    geom_errorbar(
        aes(ymin = lwr, ymax = upr),
        width = 0,
        position = position_dodge(0.3)
    ) +
    # Add the geom_blank layer. It only needs the facet variable ('grammatical')
    # and the y-axis variable ('p_hat') to influence the scale limits.
    geom_blank(data = facet_limits, aes(y = p_hat), inherit.aes = FALSE) +

    facet_wrap(
        ~grammatical,
        labeller = as_labeller(exp2.gram.label),
        scale = "free_y"
    ) +
    scale_x_discrete(labels = exp2.att_type.label, drop = FALSE) +
    xlab("Attractor Type") +
    ylab("Proportion 'Yes' Responses") +
    scale_y_continuous(labels = scales::percent) +
    scale_linetype_discrete(
        name = "Attractor Number",
        labels = c("Singular", "Plural")
    ) +
    theme(
        legend.position = "bottom",
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 10),
    )

# p_desc now contains the plot with the correct y-axis ranges for each facet.
ggsave("hsp_desc_plot.png", p_desc, width = 6, height = 3)


# 3. FIT BRMS MODEL
exp2.dfModel <- exp2.clean %>% subset(match != "filler") %>% droplevels()

# Set simple sum contrasts (centered)
contrasts(exp2.dfModel$grammatical) <- contr.sum(2)
contrasts(exp2.dfModel$attractor_num) <- contr.sum(2)
contrasts(exp2.dfModel$att_type) <- contr.sum(2)

# Fit model
# Increased iterations
m.exp2 <- brm(
    bf(
        response_yes ~ grammatical *
            attractor_num *
            att_type +
            (1 + grammatical * attractor_num * att_type | subject) +
            (1 + grammatical * attractor_num * att_type | item),
        decomp = "QR"
    ),
    data = exp2.dfModel,
    family = bernoulli(link = "logit"),
    threads = threading(4),
    chains = 4,
    iter = 4000,
    warmup = 2000,
    seed = 1,
    file = "m.hsp.simple"
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
# Effect > 0 involves the level coded as +1.
# grammatical1 (Gram vs Ungram) -> Gram (1) > Ungram (-1).
coeffs <- bind_rows(
    summarize_draws(draws$b_grammatical1, "Grammaticality\n(Gram vs Ungram)"),
    summarize_draws(draws$b_attractor_num1, "Attractor Number\n(Pl vs Sg)"),
    summarize_draws(draws$b_att_type1, "Attractor Type\n(Gen vs RC)"),
    summarize_draws(draws$`b_grammatical1:attractor_num1`, "Gram x AttrNum"),
    summarize_draws(draws$`b_grammatical1:att_type1`, "Gram x Type"),
    summarize_draws(draws$`b_attractor_num1:att_type1`, "AttrNum x Type"),
    summarize_draws(
        draws$`b_grammatical1:attractor_num1:att_type1`,
        "Gram x AttrNum x Type"
    )
)

# Format label with P(>0)
coeffs %<>%
    mutate(
        label = glue("{term}\nP(>0)={round(p_gt0, 2)}")
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
    theme_minimal(base_family = "Times") +
    theme(
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 10)
    )

ggsave("hsp_model_plot.png", p_model, width = 6, height = 4)

print("Analysis Complete. Plots saved.")

exp2.clean.nom <- exp2.clean %>% dplyr::filter(att_type == "gen")

m_nom <- brm(
    bf(
        response_yes ~ 1 +
            (grammatical * attractor_num) +
            (1 + grammatical * attractor_num | subject) +
            (1 + grammatical * attractor_num | item),
        decomp = "QR"
    ),
    data = exp2.clean.nom,
    family = bernoulli(link = "logit"),
    threads = threading(4),
    chains = 4,
    iter = 4000,
    warmup = 2000,
    seed = 1,
    file = "m.nom"
)


exp2.clean.verb <- exp2.clean %>% dplyr::filter(att_type == "rc")

m_verb <- brm(
    bf(
        response_yes ~ 1 +
            (grammatical * attractor_num) +
            (1 + grammatical * attractor_num | subject) +
            (1 + grammatical * attractor_num | item),
        decomp = "QR"
    ),
    data = exp2.clean.verb,
    family = bernoulli(link = "logit"),
    threads = threading(4),
    chains = 4,
    iter = 4000,
    warmup = 2000,
    seed = 1,
    file = "m.verb"
)


# 4. PLOT MODEL COEFFICIENTS
nom_draws <- as_draws_df(m_nom)
verb_draws <- as_draws_df(m_verb)

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
# Effect > 0 involves the level coded as +1.
# grammatical1 (Gram vs Ungram) -> Gram (1) > Ungram (-1).
nom_coeffs <- bind_rows(
    summarize_draws(nom_draws$b_grammaticalungrammatical, "Grammaticality"),
    summarize_draws(nom_draws$b_attractor_numsingular, "Attractor Number"),
    summarize_draws(
        nom_draws$`b_grammaticalungrammatical:attractor_numsingular`,
        "Interaction"
    )
) %>%
    mutate(att_type = "Nominal")

verb_coeffs <- bind_rows(
    summarize_draws(verb_draws$b_grammaticalungrammatical, "Grammaticality"),
    summarize_draws(verb_draws$b_attractor_numsingular, "Attractor Number"),
    summarize_draws(
        verb_draws$`b_grammaticalungrammatical:attractor_numsingular`,
        "Interaction"
    )
) %>%
    mutate(att_type = "Verbal")
# Format label with P(>0)

makeppp <- function(p) {
    temp = round(p, 2)
    label = ifelse(temp == 0, "<0.001", paste0("=", temp))
    label
}

coefs <- bind_rows(nom_coeffs, verb_coeffs) %>%
    mutate(
        label = glue("{att_type}: {term}\nP(\u03B2>0){makeppp(p_gt0)}")
    )

p_model <- coefs %>%
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
    theme_minimal(base_family = "Times") +
    theme(
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()
    )
p_model
ggsave(
    "fig-exp2-nested.pdf",
    p_model,
    width = 6,
    height = 2.5,
    device = cairo_pdf
)

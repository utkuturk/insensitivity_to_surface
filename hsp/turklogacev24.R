tl24 <- read_experimental_data(
    "../data/results_tl.txt",
    subj_offset = 1000,
    item_offset = 1000
)

tl24 %<>%
    mutate(
        exp_condition = case_when(
            exp_condition == "filler" & item_num <= 120 ~ "filler_g",
            exp_condition == "filler" & item_num >= 121 ~ "filler_ung",
            exp_condition == "practice" ~ "practice",
            exp_condition == "condition_b" ~ "condition_b",
            exp_condition == "condition_a" ~ "condition_a",
            exp_condition == "condition_c" ~ "condition_c",
            exp_condition == "condition_d" ~ "condition_d"
        )
    )


tl24.conditions <- data.frame(
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
    condition = c("practice", "a", "b", "c", "d", "filler_ung", "filler_g"),
    grammatical = c(
        "practice",
        "ungram",
        "gram",
        "ungram",
        "gram",
        "ungram",
        "gram"
    ),
    verb_num = c("practice", "pl", "sg", "pl", "sg", "sg", "pl"),
    attractor_num = c("practice", "pl", "pl", "sg", "sg", "filler", "filler"),
    match = c(
        "practice",
        "mismatch",
        "mismatch",
        "match",
        "match",
        "filler",
        "filler"
    ),
    stringsAsFactors = T
)


tl24 %<>% left_join(tl24.conditions, by = "exp_condition")

tl24 <- tl24 %>%
    subset(exp_condition != "practice")

tl24.clean <- exclude_bad_subjects(
    tl24,
    accuracy_threshold = 0.25,
    rt_below = 200,
    rt_upper = 4999
)

tl24.clean %<>% no_null_no_practice(.)

stopifnot(tl24.clean %>% subset(is.na(response_yes)) %>% nrow() == 0)


tl24.clean$isGram <- ifelse(tl24.clean$grammatical == "ungram", F, T)
tl24.clean$p_acc <- with(tl24.clean, response_yes & isGram)
tl24.clean %<>%
    mutate(ResponseCorrect = (response_yes == (grammatical == "gram")))

tl24.clean$age %<>% gsub("on sekiz", "18", .) %>% as.integer()

# merge both datasets
tl24.clean %<>%
    ungroup() %>%
    dplyr::select(
        source = experiment,
        grammatical,
        attractor_num,
        match,
        # condition,
        subject,
        trial_no,
        item,
        response_yes,
        RT,
        ResponseCorrect
    )
tl24.clean$experiment <- "Turk & Logacev 2024"
tl24.clean$grammatical %<>%
    dplyr::recode(gram = "grammatical", ungram = "ungrammatical")
tl24.clean$attractor_num %<>% dplyr::recode(pl = "plural", sg = "singular")
tl24.clean$att_type <- "gen"
tl24.clean$item %<>% as.factor()
tl24.clean$subject %<>% as.character()


tl24.avgs <- tl24.clean %>%
    filter(match != "filler") %>%
    group_by(grammatical, attractor_num, att_type) %>%
    summarise(
        successes = sum(response_yes, na.rm = TRUE),
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
    ) %>%
    select(grammatical, attractor_num, att_type, successes, N, p_hat, lwr, upr)

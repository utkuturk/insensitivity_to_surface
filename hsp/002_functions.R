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


#!/usr/bin/env Rscript

init_user_lib <- function() {
    lib_user <- Sys.getenv("R_LIBS_USER", unset = "")
    if (!nzchar(lib_user)) {
        minor_major <- strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1]
        lib_user <- file.path(path.expand("~"), "R", paste0(R.version$major, ".", minor_major), "library")
    } else {
        lib_user <- strsplit(lib_user, .Platform$path.sep, fixed = TRUE)[[1]][1]
    }
    dir.create(lib_user, recursive = TRUE, showWarnings = FALSE)
    .libPaths(c(path.expand(lib_user), .libPaths()))
    invisible(lib_user)
}

ensure_packages <- function(pkgs) {
    auto_install <- tolower(Sys.getenv("AUTO_INSTALL_R_PKGS", unset = "true")) %in%
        c("1", "true", "t", "yes", "y")
    repos <- Sys.getenv("R_CRAN_MIRROR", unset = "https://cloud.r-project.org")

    missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing) == 0) {
        return(invisible(TRUE))
    }
    if (!auto_install) {
        stop(
            "Missing packages: ",
            paste(missing, collapse = ", "),
            ". Set AUTO_INSTALL_R_PKGS=true to auto-install."
        )
    }

    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = repos, dependencies = TRUE)

    still_missing <- missing[!vapply(missing, requireNamespace, logical(1), quietly = TRUE)]
    if (length(still_missing) > 0) {
        stop("Failed to install required packages: ", paste(still_missing, collapse = ", "))
    }
    invisible(TRUE)
}

required_pkgs <- c("tidyverse", "brms", "data.table", "gdata", "magrittr", "DescTools")
init_user_lib()
ensure_packages(required_pkgs)

suppressPackageStartupMessages({
    for (pkg in required_pkgs) {
        library(pkg, character.only = TRUE)
    }
})

parse_args <- function(x) {
    out <- list()
    i <- 1L
    while (i <= length(x)) {
        tok <- x[[i]]
        if (startsWith(tok, "--")) {
            key <- sub("^--", "", tok)
            val <- TRUE
            if (i < length(x) && !startsWith(x[[i + 1L]], "--")) {
                val <- x[[i + 1L]]
                i <- i + 1L
            }
            out[[key]] <- val
        }
        i <- i + 1L
    }
    out
}

as_int <- function(x, default = NA_integer_) {
    if (is.null(x)) {
        return(default)
    }
    as.integer(x)
}

as_num <- function(x, default = NA_real_) {
    if (is.null(x)) {
        return(default)
    }
    as.numeric(x)
}

as_bool <- function(x, default = FALSE) {
    if (is.null(x)) {
        return(default)
    }
    if (is.logical(x)) {
        return(x)
    }
    tolower(as.character(x)) %in% c("1", "true", "t", "yes", "y")
}

cmd <- parse_args(commandArgs(trailingOnly = TRUE))

model_id <- as_int(cmd[["model-id"]], NA_integer_)
if (is.na(model_id)) {
    stop("Missing required argument: --model-id (1..9)")
}

threads <- as_int(cmd[["threads"]], 7L)
chains <- as_int(cmd[["chains"]], 4L)
cores <- as_int(cmd[["cores"]], 4L)
iter <- as_int(cmd[["iter"]], 12000L)
warmup <- as_int(cmd[["warmup"]], 2000L)
seed <- as_int(cmd[["seed"]], 25022026L)
force_refit <- as_bool(cmd[["force"]], FALSE)
adapt_delta <- as_num(cmd[["adapt-delta"]], NA_real_)
max_treedepth <- as_int(cmd[["max-treedepth"]], NA_integer_)

backend <- Sys.getenv("BRMS_BACKEND", unset = "")

project_dir <- "/home/uturk_umass_edu/insensitivity_to_surface"
if (!dir.exists(project_dir)) {
    stop("Project directory does not exist: ", project_dir)
}
setwd(project_dir)

message("Project dir: ", project_dir)
message("Requested model id: ", model_id)
message(
    "Sampler settings: chains=",
    chains,
    ", cores=",
    cores,
    ", threads=",
    threads,
    ", iter=",
    iter,
    ", warmup=",
    warmup,
    ", adapt_delta=",
    ifelse(is.na(adapt_delta), "auto", format(adapt_delta, digits = 3)),
    ", max_treedepth=",
    ifelse(is.na(max_treedepth), "auto", as.character(max_treedepth)),
    ", seed=",
    seed
)

source(file.path(project_dir, "utility", "functions.R"))

exp1 <- read_experimental_data(
    file.path(project_dir, "utility", "data", "results_8cond.txt"),
    subj_offset = 2500,
    item_offset = 2500
)

exp1 %<>%
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

exp1.conditions <- data.frame(
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
    stringsAsFactors = TRUE
)

exp1 %<>% left_join(exp1.conditions, by = "exp_condition")

exp1.clean <- exclude_bad_subjects_8(
    exp1,
    accuracy_threshold = 0.25,
    rt_below = 200,
    rt_upper = 4999
)

exp1.clean %<>% no_null_no_practice(.)
stopifnot(exp1.clean %>% filter(is.na(response_yes)) %>% nrow() == 0)

exp1.clean %<>%
    ungroup() %>%
    dplyr::select(
        source = experiment,
        grammatical,
        attractor_num,
        att_type,
        match,
        age,
        subject,
        trial_no,
        item,
        response_yes,
        RT
    )

exp1.clean$experiment <- "Experiment 1"
exp1.clean$grammatical %<>%
    dplyr::recode(gram = "grammatical", ungram = "ungrammatical")
exp1.clean$attractor_num %<>% dplyr::recode(pl = "plural", sg = "singular")
exp1.clean$att_type %<>% dplyr::recode(gen = "gen", rc = "rc")
exp1.clean$item %<>% as.factor()
exp1.clean$subject %<>% as.character()

source(file.path(project_dir, "utility", "hsp", "turklogacev24.R"), chdir = TRUE)

exp1.dfModel <- exp1.clean %>% subset(match != "filler")
exp1.dfModel %<>% mutate(exp = "current") %>% droplevels()

tl24.gen <- tl24.clean %>%
    subset(match != "filler") %>%
    mutate(att_type = "gen-tl") %>%
    droplevels()

exp1.all <- bind_rows(exp1.dfModel, tl24.gen)

exp1.all %<>%
    mutate(
        grammatical = factor(
            grammatical,
            levels = c("grammatical", "ungrammatical"),
            labels = c("Grammatical", "Ungrammatical")
        ),
        attractor_num = factor(
            attractor_num,
            levels = c("singular", "plural"),
            labels = c("Singular", "Plural")
        ),
        att_type = factor(
            att_type,
            levels = c("gen", "gen-tl", "rc"),
            labels = c("Gen-Current", "Gen-TL24", "RC")
        )
    )

C2 <- contr.sum(2) / 2
Cg <- C2
colnames(Cg) <- "Gram_minus_Ungram"
Ca <- C2
colnames(Ca) <- "Plural_minus_Singular"

C3 <- matrix(
    c(
        -1,
        -1,
        2,
        1,
        -1,
        0
    ),
    ncol = 2
)
C3 <- apply(C3, 2, function(x) x / sum(abs(x)) * 2 / 3)
colnames(C3) <- c("RC_vs_Gens", "GenCurrent_vs_GenTL24")
rownames(C3) <- c("Gen-Current", "Gen-TL24", "RC")

contrasts(exp1.all$att_type) <- C3
contrasts(exp1.all$grammatical) <- Cg
contrasts(exp1.all$attractor_num) <- -Ca

make_priors_generic <- function(
    f_mean = 0,
    f_sd = 1,
    intercept_mean = 0.85,
    intercept_sd = 0.70,
    exp_rate = 1,
    lkj_eta = 2
) {
    c(
        set_prior(
            sprintf("normal(%g, %g)", intercept_mean, intercept_sd),
            class = "Intercept"
        ),
        set_prior(sprintf("normal(%g, %g)", f_mean, f_sd), class = "b"),
        set_prior(sprintf("exponential(%g)", exp_rate), class = "sd"),
        set_prior(sprintf("lkj(%g)", lkj_eta), class = "cor")
    )
}

fit_formula_full <- brms::bf(
    response_yes ~ grammatical *
        attractor_num *
        att_type +
        (1 + grammatical * attractor_num * att_type | subject) +
        (1 + grammatical * attractor_num * att_type | item),
    decomp = "QR"
)

fit_formula_no3way <- brms::bf(
    response_yes ~ grammatical *
        attractor_num +
        grammatical * att_type +
        attractor_num * att_type +
        (1 + grammatical * attractor_num * att_type | subject) +
        (1 + grammatical * attractor_num * att_type | item),
    decomp = "QR"
)

fit_formula_noGA <- brms::bf(
    response_yes ~ grammatical +
        attractor_num +
        att_type +
        grammatical:att_type +
        attractor_num:att_type +
        (1 + grammatical * attractor_num * att_type | subject) +
        (1 + grammatical * attractor_num * att_type | item),
    decomp = "QR"
)

model_specs <- tibble::tribble(
    ~model_id, ~model_label, ~formula_name, ~prior_sd, ~file_stub,
    1L, "full_n05", "full", 0.5, "m.exp1.full.prior_n05",
    2L, "no3way_n05", "no3way", 0.5, "m.exp1.no3way.prior_n05",
    3L, "noGA_n05", "noGA", 0.5, "m.exp1.noGA.prior_n05",
    4L, "full_n10", "full", 1.0, "m.exp1.full.prior_n10",
    5L, "no3way_n10", "no3way", 1.0, "m.exp1.no3way.prior_n10",
    6L, "noGA_n10", "noGA", 1.0, "m.exp1.noGA.prior_n10",
    7L, "full_n20", "full", 2.0, "m.exp1.full.prior_n20",
    8L, "no3way_n20", "no3way", 2.0, "m.exp1.no3way.prior_n20",
    9L, "noGA_n20", "noGA", 2.0, "m.exp1.noGA.prior_n20"
)

spec <- model_specs %>% filter(model_id == !!model_id)
if (nrow(spec) != 1L) {
    stop("model-id must be one of 1..9")
}

formula_i <- switch(
    spec$formula_name[[1]],
    full = fit_formula_full,
    no3way = fit_formula_no3way,
    noGA = fit_formula_noGA,
    stop("Unknown formula_name in model_specs.")
)

prior_i <- make_priors_generic(
    f_mean = 0,
    f_sd = spec$prior_sd[[1]],
    exp_rate = 1,
    lkj_eta = 2
)

if (is.na(adapt_delta)) {
    adapt_delta <- if (spec$prior_sd[[1]] >= 2.0) 0.995 else 0.99
}
if (is.na(max_treedepth)) {
    max_treedepth <- if (spec$prior_sd[[1]] >= 2.0) 15L else 12L
}

file_i <- file.path(project_dir, "utility", "models", spec$file_stub[[1]])
file_rds_i <- paste0(file_i, ".rds")

if (file.exists(file_rds_i) && !force_refit) {
    message("Model already exists; skipping fit: ", file_rds_i)
    quit(save = "no", status = 0)
}

dir.create(dirname(file_i), recursive = TRUE, showWarnings = FALSE)

message("Fitting ", spec$model_label[[1]], " -> ", file_i)

fit_args <- list(
    formula = formula_i,
    data = exp1.all,
    family = bernoulli(link = "logit"),
    prior = prior_i,
    sample_prior = "no",
    save_pars = save_pars(all = TRUE),
    chains = chains,
    cores = cores,
    iter = iter,
    warmup = warmup,
    init = 0,
    seed = seed,
    file = file_i,
    control = list(
        adapt_delta = adapt_delta,
        max_treedepth = max_treedepth
    )
)

if (threads > 1L) {
    fit_args$threads <- threading(threads = threads)
}
if (nzchar(backend)) {
    fit_args$backend <- backend
}

do.call(brm, fit_args)

message("Done: ", file_i)

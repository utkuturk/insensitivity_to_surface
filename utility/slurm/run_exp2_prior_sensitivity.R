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

backend <- Sys.getenv("BRMS_BACKEND", unset = "")

project_dir <- "/home/uturk_umass_edu/insensitivity_to_surface"
if (!dir.exists(project_dir)) {
    stop("Project directory does not exist: ", project_dir)
}
setwd(project_dir)

message("Project dir: ", project_dir)
message("Requested model id: ", model_id)
message(
    "Sampler settings: chains=", chains,
    ", cores=", cores,
    ", threads=", threads,
    ", iter=", iter,
    ", warmup=", warmup,
    ", seed=", seed
)

source(file.path(project_dir, "utility", "functions.R"))

exp2 <- read_experimental_data(
    file.path(project_dir, "utility", "data", "results.txt"),
    subj_offset = 2000,
    item_offset = 2000
)

exp2 %<>% mutate(exp_condition = case_when(
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
    experiment = c("practice", "AgrAttr", "AgrAttr", "AgrAttr", "AgrAttr", "filler", "filler"),
    condition = c("practice", "a", "b", "c", "d", "filler_ung", "filler_g"),
    grammatical = c("practice", "ungram", "gram", "ungram", "gram", "ungram", "gram"),
    verb_num = c("practice", "pl", "sg", "pl", "sg", "sg", "pl"),
    attractor_num = c("practice", "pl", "pl", "sg", "sg", "filler", "filler"),
    match = c("practice", "mismatch", "mismatch", "match", "match", "filler", "filler"),
    stringsAsFactors = TRUE
)

exp2 %<>% left_join(exp2.conditions, by = "exp_condition")

exp2.clean <- exclude_bad_subjects(
    exp2,
    accuracy_threshold = 0.25,
    rt_below = 200,
    rt_upper = 4999
)

exp2.clean %<>% no_null_no_practice(.)
stopifnot(exp2.clean %>% filter(is.na(response_yes)) %>% nrow() == 0)

exp2.clean %<>% ungroup() %>%
    dplyr::select(
        source = experiment,
        grammatical,
        attractor_num,
        match,
        age,
        subject,
        trial_no,
        item,
        response_yes,
        RT
    )

exp2.clean$experiment <- "Experiment 2"
exp2.clean$grammatical %<>% dplyr::recode(gram = "grammatical", ungram = "ungrammatical")
exp2.clean$attractor_num %<>% dplyr::recode(pl = "plural", sg = "singular")
exp2.clean$item %<>% as.factor()
exp2.clean$subject %<>% as.character()

exp2.dfModel <- exp2.clean %>% subset(match != "filler")

exp2.dfModel %<>%
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
        )
    )

C2 <- contr.sum(2) / 2
Cg <- C2
colnames(Cg) <- "Gram_minus_Ungram"
Ca <- C2
colnames(Ca) <- "Plural_minus_Singular"
contrasts(exp2.dfModel$grammatical) <- Cg
contrasts(exp2.dfModel$attractor_num) <- -Ca

make_priors_exp2 <- function(
    f_mean = 0, f_sd = 1,
    intercept_mean = 0, intercept_sd = 1,
    exp_rate = 1, lkj_eta = 2) {
    c(
        set_prior(sprintf("normal(%g, %g)", intercept_mean, intercept_sd), class = "Intercept"),
        set_prior(sprintf("normal(%g, %g)", f_mean, f_sd), class = "b"),
        set_prior(sprintf("exponential(%g)", exp_rate), class = "sd"),
        set_prior(sprintf("lkj(%g)", lkj_eta), class = "cor")
    )
}

fit_formula_full <- brms::bf(
    response_yes ~ grammatical * attractor_num +
        (1 + grammatical * attractor_num | subject) +
        (1 + grammatical * attractor_num | item),
    decomp = "QR"
)

fit_formula_noInt <- brms::bf(
    response_yes ~ grammatical + attractor_num +
        (1 + grammatical * attractor_num | subject) +
        (1 + grammatical * attractor_num | item),
    decomp = "QR"
)

fit_formula_noA <- brms::bf(
    response_yes ~ grammatical +
        (1 + grammatical * attractor_num | subject) +
        (1 + grammatical * attractor_num | item),
    decomp = "QR"
)

model_specs <- tibble::tribble(
    ~model_id, ~model_label, ~formula_name, ~prior_sd, ~file_stub,
    1L, "full_n05", "full", 0.5, "m.exp2.full.prior_n05",
    2L, "noInt_n05", "noInt", 0.5, "m.exp2.noInt.prior_n05",
    3L, "noA_n05", "noA", 0.5, "m.exp2.noA.prior_n05",
    4L, "full_n10", "full", 1.0, "m.exp2.full.prior_n10",
    5L, "noInt_n10", "noInt", 1.0, "m.exp2.noInt.prior_n10",
    6L, "noA_n10", "noA", 1.0, "m.exp2.noA.prior_n10",
    7L, "full_n20", "full", 2.0, "m.exp2.full.prior_n20",
    8L, "noInt_n20", "noInt", 2.0, "m.exp2.noInt.prior_n20",
    9L, "noA_n20", "noA", 2.0, "m.exp2.noA.prior_n20"
)

spec <- model_specs %>% filter(model_id == !!model_id)
if (nrow(spec) != 1L) {
    stop("model-id must be one of 1..9")
}

formula_i <- switch(
    spec$formula_name[[1]],
    full = fit_formula_full,
    noInt = fit_formula_noInt,
    noA = fit_formula_noA,
    stop("Unknown formula_name in model_specs.")
)

prior_i <- make_priors_exp2(
    f_mean = 0,
    f_sd = spec$prior_sd[[1]],
    exp_rate = 1,
    lkj_eta = 2
)

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
    data = exp2.dfModel,
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
    file = file_i
)

if (threads > 1L) {
    fit_args$threads <- threading(threads = threads)
}
if (nzchar(backend)) {
    fit_args$backend <- backend
}

do.call(brm, fit_args)

message("Done: ", file_i)

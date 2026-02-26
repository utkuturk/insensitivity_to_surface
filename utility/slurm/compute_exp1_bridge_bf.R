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

required_pkgs <- c("tidyverse", "bridgesampling")
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

as_bool <- function(x, default = FALSE) {
    if (is.null(x)) {
        return(default)
    }
    if (is.logical(x)) {
        return(x)
    }
    tolower(as.character(x)) %in% c("1", "true", "t", "yes", "y")
}

as_int <- function(x, default = NA_integer_) {
    if (is.null(x)) {
        return(default)
    }
    as.integer(x)
}

cmd <- parse_args(commandArgs(trailingOnly = TRUE))
recompile <- as_bool(cmd[["recompile"]], FALSE)
force_bridge <- as_bool(cmd[["force-bridge"]], FALSE)
bridge_method <- if (is.null(cmd[["bridge-method"]])) "normal" else as.character(cmd[["bridge-method"]])
bridge_repetitions <- as_int(cmd[["bridge-repetitions"]], 1L)
bridge_maxiter <- as_int(cmd[["bridge-maxiter"]], 2000L)
bridge_use_neff <- as_bool(cmd[["bridge-use-neff"]], TRUE)

project_dir <- "/home/uturk_umass_edu/insensitivity_to_surface"
if (!dir.exists(project_dir)) {
    stop("Project directory does not exist: ", project_dir)
}
setwd(project_dir)

models_dir <- file.path(project_dir, "utility", "models")

safe_read_fit <- function(stub) {
    p <- file.path(models_dir, paste0(stub, ".rds"))
    if (!file.exists(p)) {
        stop("Missing model fit: ", p)
    }
    readRDS(p)
}

safe_bridge <- function(
    fit,
    bridge_path,
    recompile = FALSE,
    force = FALSE,
    method = "normal",
    repetitions = 1L,
    maxiter = 2000L,
    use_neff = TRUE
) {
    if (file.exists(bridge_path) && !force) {
        return(readRDS(bridge_path))
    }

    methods_to_try <- unique(c(method, if (method == "normal") "warp3"))
    out <- NULL
    for (m in methods_to_try) {
        out_try <- tryCatch(
            bridgesampling::bridge_sampler(
                fit,
                method = m,
                repetitions = repetitions,
                maxiter = maxiter,
                use_neff = use_neff,
                silent = TRUE,
                recompile = recompile
            ),
            error = function(e) NULL
        )
        if (!is.null(out_try) && is.finite(out_try$logml)) {
            out <- out_try
            break
        }
        if (!is.null(out_try) && is.null(out)) {
            out <- out_try
        }
    }
    if (is.null(out)) {
        stop("bridge_sampler failed for all attempted methods.")
    }

    saveRDS(out, bridge_path)
    out
}

compute_bf01 <- function(bridge_alt, bridge_null) {
    bf10 <- as.numeric(bridgesampling::bf(bridge_alt, bridge_null)$bf)
    1 / bf10
}

interpret_bf <- function(bf) {
    case_when(
        is.na(bf) ~ "not computed",
        bf > 100 ~ "extreme evidence for null",
        bf > 30 ~ "very strong evidence for null",
        bf > 10 ~ "strong evidence for null",
        bf > 3 ~ "moderate evidence for null",
        bf > 1 ~ "anecdotal evidence for null",
        bf > 1 / 3 ~ "anecdotal evidence against null",
        bf > 1 / 10 ~ "moderate evidence against null",
        TRUE ~ "strong evidence against null"
    )
}

message(
    "Computing Exp1 bridge BFs (recompile=", recompile,
    ", force_bridge=", force_bridge,
    ", method=", bridge_method,
    ", reps=", bridge_repetitions,
    ", maxiter=", bridge_maxiter,
    ", use_neff=", bridge_use_neff,
    ")"
)

# Main BF outputs (n10 prior set used in text)
fit_full_n10 <- safe_read_fit("m.exp1.full.prior_n10")
fit_no3way_n10 <- safe_read_fit("m.exp1.no3way.prior_n10")
fit_noGA_n10 <- safe_read_fit("m.exp1.noGA.prior_n10")

bridge_full_n10 <- safe_bridge(
    fit_full_n10,
    file.path(models_dir, "m.exp1.full.prior_n10.bridge.rds"),
    recompile = recompile,
    force = force_bridge,
    method = bridge_method,
    repetitions = bridge_repetitions,
    maxiter = bridge_maxiter,
    use_neff = bridge_use_neff
)
bridge_no3way_n10 <- safe_bridge(
    fit_no3way_n10,
    file.path(models_dir, "m.exp1.no3way.prior_n10.bridge.rds"),
    recompile = recompile,
    force = force_bridge,
    method = bridge_method,
    repetitions = bridge_repetitions,
    maxiter = bridge_maxiter,
    use_neff = bridge_use_neff
)
bridge_noGA_n10 <- safe_bridge(
    fit_noGA_n10,
    file.path(models_dir, "m.exp1.noGA.prior_n10.bridge.rds"),
    recompile = recompile,
    force = force_bridge,
    method = bridge_method,
    repetitions = bridge_repetitions,
    maxiter = bridge_maxiter,
    use_neff = bridge_use_neff
)

bf_exp1 <- list(
    bf01_3way = as.numeric(compute_bf01(bridge_full_n10, bridge_no3way_n10)),
    bf01_2way = as.numeric(compute_bf01(bridge_no3way_n10, bridge_noGA_n10)),
    comparison_3way = "Full vs No-3way",
    comparison_2way = "No-3way vs No-(GxA)"
)
saveRDS(bf_exp1, file.path(models_dir, "bf_exp1_bridge.rds"))

message("Saved: utility/models/bf_exp1_bridge.rds")

# Prior-sensitivity BF outputs
prior_grid <- tibble::tribble(
    ~Prior_Set, ~fixed_sd, ~full_stub, ~no3way_stub, ~noGA_stub,
    "Narrow", 0.5, "m.exp1.full.prior_n05", "m.exp1.no3way.prior_n05", "m.exp1.noGA.prior_n05",
    "Main", 1.0, "m.exp1.full.prior_n10", "m.exp1.no3way.prior_n10", "m.exp1.noGA.prior_n10",
    "Wide", 2.0, "m.exp1.full.prior_n20", "m.exp1.no3way.prior_n20", "m.exp1.noGA.prior_n20"
)

rope_halfwidth <- 0.20

prior_sens <- purrr::pmap_dfr(
    prior_grid,
    function(Prior_Set, fixed_sd, full_stub, no3way_stub, noGA_stub) {
        fit_full <- safe_read_fit(full_stub)
        fit_no3way <- safe_read_fit(no3way_stub)
        fit_noGA <- safe_read_fit(noGA_stub)

        bridge_full <- safe_bridge(
            fit_full,
            file.path(models_dir, paste0(full_stub, ".bridge.rds")),
            recompile = recompile,
            force = force_bridge,
            method = bridge_method,
            repetitions = bridge_repetitions,
            maxiter = bridge_maxiter,
            use_neff = bridge_use_neff
        )
        bridge_no3way <- safe_bridge(
            fit_no3way,
            file.path(models_dir, paste0(no3way_stub, ".bridge.rds")),
            recompile = recompile,
            force = force_bridge,
            method = bridge_method,
            repetitions = bridge_repetitions,
            maxiter = bridge_maxiter,
            use_neff = bridge_use_neff
        )
        bridge_noGA <- safe_bridge(
            fit_noGA,
            file.path(models_dir, paste0(noGA_stub, ".bridge.rds")),
            recompile = recompile,
            force = force_bridge,
            method = bridge_method,
            repetitions = bridge_repetitions,
            maxiter = bridge_maxiter,
            use_neff = bridge_use_neff
        )

        bf01_3way <- as.numeric(compute_bf01(bridge_full, bridge_no3way))
        bf01_2way <- as.numeric(compute_bf01(bridge_no3way, bridge_noGA))

        tibble(
            Prior_Set = Prior_Set,
            Fixed_Effect_Prior = sprintf("Normal(0, %.1f)", fixed_sd),
            fixed_sd = fixed_sd,
            Prior_Density_at_0 = dnorm(0, mean = 0, sd = fixed_sd),
            Prior_P_abs_beta_lt_0_20 = pnorm(rope_halfwidth, 0, fixed_sd) - pnorm(-rope_halfwidth, 0, fixed_sd),
            BF01_GxAxT1 = bf01_3way,
            BF01_GxA = bf01_2way,
            BF01_GxAxT1_Interpretation = interpret_bf(bf01_3way),
            Fit = "all fits loaded"
        )
    }
)

saveRDS(
    prior_sens,
    file.path(models_dir, "bf_exp1_prior_sensitivity_bridge.rds")
)

message("Saved: utility/models/bf_exp1_prior_sensitivity_bridge.rds")
message("Done.")

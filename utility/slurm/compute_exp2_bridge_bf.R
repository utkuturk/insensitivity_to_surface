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

cmd <- parse_args(commandArgs(trailingOnly = TRUE))
recompile <- as_bool(cmd[["recompile"]], FALSE)

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

safe_bridge <- function(fit, bridge_path, recompile = FALSE) {
    if (file.exists(bridge_path)) {
        return(readRDS(bridge_path))
    }
    out <- bridgesampling::bridge_sampler(fit, silent = TRUE, recompile = recompile)
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

message("Computing Exp2 bridge BFs (recompile=", recompile, ")")

# Main BF outputs (n10 prior set used as primary)
fit_full_n10 <- safe_read_fit("m.exp2.full.prior_n10")
fit_noInt_n10 <- safe_read_fit("m.exp2.noInt.prior_n10")
fit_noA_n10 <- safe_read_fit("m.exp2.noA.prior_n10")

bridge_full_n10 <- safe_bridge(
    fit_full_n10,
    file.path(models_dir, "m.exp2.full.prior_n10.bridge.rds"),
    recompile = recompile
)
bridge_noInt_n10 <- safe_bridge(
    fit_noInt_n10,
    file.path(models_dir, "m.exp2.noInt.prior_n10.bridge.rds"),
    recompile = recompile
)
bridge_noA_n10 <- safe_bridge(
    fit_noA_n10,
    file.path(models_dir, "m.exp2.noA.prior_n10.bridge.rds"),
    recompile = recompile
)

bf_exp2 <- list(
    bf01_int = as.numeric(compute_bf01(bridge_full_n10, bridge_noInt_n10)),
    bf01_attr = as.numeric(compute_bf01(bridge_noInt_n10, bridge_noA_n10)),
    comparison_int = "Full vs No-interaction",
    comparison_attr = "No-interaction vs No-attractor"
)
saveRDS(bf_exp2, file.path(models_dir, "bf_exp2_bridge.rds"))

message("Saved: utility/models/bf_exp2_bridge.rds")

# Prior-sensitivity BF outputs
prior_grid <- tibble::tribble(
    ~Prior_Set, ~fixed_sd, ~full_stub, ~noInt_stub, ~noA_stub,
    "Narrow", 0.5, "m.exp2.full.prior_n05", "m.exp2.noInt.prior_n05", "m.exp2.noA.prior_n05",
    "Main", 1.0, "m.exp2.full.prior_n10", "m.exp2.noInt.prior_n10", "m.exp2.noA.prior_n10",
    "Wide", 2.0, "m.exp2.full.prior_n20", "m.exp2.noInt.prior_n20", "m.exp2.noA.prior_n20"
)

rope_halfwidth <- 0.20

prior_sens <- purrr::pmap_dfr(
    prior_grid,
    function(Prior_Set, fixed_sd, full_stub, noInt_stub, noA_stub) {
        fit_full <- safe_read_fit(full_stub)
        fit_noInt <- safe_read_fit(noInt_stub)
        fit_noA <- safe_read_fit(noA_stub)

        bridge_full <- safe_bridge(
            fit_full,
            file.path(models_dir, paste0(full_stub, ".bridge.rds")),
            recompile = recompile
        )
        bridge_noInt <- safe_bridge(
            fit_noInt,
            file.path(models_dir, paste0(noInt_stub, ".bridge.rds")),
            recompile = recompile
        )
        bridge_noA <- safe_bridge(
            fit_noA,
            file.path(models_dir, paste0(noA_stub, ".bridge.rds")),
            recompile = recompile
        )

        bf01_int <- as.numeric(compute_bf01(bridge_full, bridge_noInt))
        bf01_attr <- as.numeric(compute_bf01(bridge_noInt, bridge_noA))

        tibble(
            Prior_Set = Prior_Set,
            Fixed_Effect_Prior = sprintf("Normal(0, %.1f)", fixed_sd),
            fixed_sd = fixed_sd,
            Prior_Density_at_0 = dnorm(0, mean = 0, sd = fixed_sd),
            Prior_P_abs_beta_lt_0_20 = pnorm(rope_halfwidth, 0, fixed_sd) - pnorm(-rope_halfwidth, 0, fixed_sd),
            BF01_Interaction = bf01_int,
            BF01_Attractor = bf01_attr,
            BF01_Interaction_Interpretation = interpret_bf(bf01_int),
            Fit = "all fits loaded"
        )
    }
)

saveRDS(
    prior_sens,
    file.path(models_dir, "bf_exp2_prior_sensitivity_bridge.rds")
)

message("Saved: utility/models/bf_exp2_prior_sensitivity_bridge.rds")
message("Done.")

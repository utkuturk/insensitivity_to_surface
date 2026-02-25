rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(truncnorm)

###############################################################
## Generate interACT predictions for Slioussar (2018)
## "Forms and features: The role of syncretism in number
##  agreement attraction" (JML, 101, 51-63)
##
## Following Lacina et al. (2025) approach:
##   - All ACT-R parameters FIXED to Engelmann et al. (2019) defaults
##   - Only lf (latency factor) sampled from a truncated normal prior
##   - Structural assumptions varied: phon_activation = 1.0 vs 0.5
##
## We focus on ERRORS only (miss rate = distractor retrieved).
## The model's "miss" rate = probability of retrieving the
## distractor instead of the target = agreement attraction error.
##
## Target data: Experiment 1 (Production) and Experiment 2
## (Comprehension), singular head conditions only.
##
## Key conditions (singular head, attractor varies):
##   1. Acc.Sg — not syncretic, same number as head -> baseline
##   2. Acc.Pl — syncretic (= Nom.Pl), plural number -> max attraction
##   3. Gen.Sg — syncretic (= Nom.Pl), singular number -> phon similarity
##   4. Gen.Pl — not syncretic, plural number -> number-only attraction
##
## The puzzle: Gen.Sg (singular!) triggers MORE errors than Gen.Pl (plural!),
## because Gen.Sg is morphologically identical to Nom.Pl.
## This cannot be explained by number mismatch alone.
###############################################################

## ----------------------------------------------------------
## Observed error rates (from Tables 3-4 & Tables 5-6)
## ----------------------------------------------------------

## Experiment 1: Production (agreement errors only, singular heads)
obs_exp1 <- data.frame(
    Condition = c("Acc.Sg", "Acc.Pl", "Gen.Sg", "Gen.Pl"),
    Syncretic = c(FALSE, TRUE, TRUE, FALSE),
    Attractor_Number = c("Sg", "Pl", "Sg", "Pl"),
    Error_Rate = c(0.00, 0.153, 0.041, 0.006)
)

## Experiment 2: Comprehension (incorrect responses, ungrammatical, singular heads)
obs_exp2 <- data.frame(
    Condition = c("Acc.Sg", "Acc.Pl", "Gen.Sg", "Gen.Pl"),
    Syncretic = c(FALSE, TRUE, TRUE, FALSE),
    Attractor_Number = c("Sg", "Pl", "Sg", "Pl"),
    Error_Rate = c(0.048, 0.234, 0.145, 0.095)
)

cat("=== Observed Error Rates ===\n")
cat("\nExperiment 1 (Production):\n")
print(obs_exp1)
cat("\nExperiment 2 (Comprehension, ungrammatical):\n")
print(obs_exp2)


## ----------------------------------------------------------
## Load the phonological model engine
## (2 features: structural + number, with phon_activation)
## ----------------------------------------------------------
source("interACT_2features_phon.R")


## ----------------------------------------------------------
## Adapt the model for Slioussar's design
## ----------------------------------------------------------
##
## Slioussar's design for singular heads:
##   Subject = Singular (target for agreement)
##   Verb cue = [subject, singular]
##
## INTERPRETATION (following Slioussar's analysis):
##   The critical syncretic effect is on the CASE/STRUCTURAL cue, not number.
##   When the attractor's form = Nom.Pl, it partially matches the "nominative"
##   (structural) cue, even though it's actually in a different case.
##
##   So the match vectors should be:
##   Feature 1: structural/case (nominative match)
##   Feature 2: number
##
##   Acc.Sg: structural = 0 (not nom), number = 1 (sg matches sg) -> (0,1)
##   Acc.Pl (=Nom.Pl): structural = phon_act (form looks nominative), number = 0 (pl != sg) -> (phon_act, 0)
##   Gen.Sg (=Nom.Pl): structural = phon_act (form looks nominative), number = 1 (sg = sg) -> (phon_act, 1)
##   Gen.Pl: structural = 0 (not nom), number = 0 (pl != sg) -> (0,0)

distortion_slioussar <- function(phon_act = phon_activation) {
    # 4 conditions for singular head, verb cues [nominative, singular]
    # Target always: structural = 1, number = 1
    # Distractor varies by condition

    model_4cond <- list(
        target_match = list(
            c(1, 1), # Acc.Sg: target matches both cues
            c(1, 1), # Acc.Pl: target matches both cues
            c(1, 1), # Gen.Sg: target matches both cues
            c(1, 1) # Gen.Pl: target matches both cues
        ),
        distractor_match = list(
            c(0, 1), # Acc.Sg: not nom (0), sg matches sg cue (1)
            c(phon_act, 0), # Acc.Pl (=Nom.Pl): looks nom (phon_act), pl mismatches sg cue (0)
            c(phon_act, 1), # Gen.Sg (=Nom.Pl): looks nom (phon_act), sg matches sg cue (1) !!! DOUBLE MATCH
            c(0, 0) # Gen.Pl: not nom (0), pl mismatches sg cue (0)
        ),
        Target = c("Sg head", "Sg head", "Sg head", "Sg head"),
        Distractor = c(
            "Acc.Sg (not syncretic, sg)",
            "Acc.Pl (syncretic=Nom.Pl, pl)",
            "Gen.Sg (syncretic=Nom.Pl, sg)",
            "Gen.Pl (not syncretic, pl)"
        ),
        weights = list(c(strWeight(), numWeight()))
    )
    model_4cond
}


## ----------------------------------------------------------
## Create parameter matrix for 4 conditions
## ----------------------------------------------------------
create_param_matrix_4cond <- function(
    phon_act,
    model,
    iterations = 1000,
    verbose = FALSE
) {
    if (verbose) {
        print("creating parameter matrix...")
    }
    cl <<- (cuesim + 1) * 100
    set_prameters()
    n_params <- length(parameters)
    n_sets <- prod(unlist(lapply(parameters, length)))
    n_cond <- length(model$target_match)
    total <- iterations * n_sets * n_cond
    if (verbose) {
        print(paste("Conditions:", n_cond))
        print(paste("Combinations:", n_sets))
        print(paste("Total runs:", total))
    }
    param_combs <- matrix(nrow = n_sets, ncol = n_params)
    cumulative_num_combs <- 1
    for (p in 1:n_params) {
        param_combs[, p] <- rep(
            parameters[p][[1]],
            each = cumulative_num_combs,
            length.out = n_sets
        )
        cumulative_num_combs <- cumulative_num_combs *
            length(parameters[p][[1]])
    }
    param_matrix <- matrix(
        data = t(param_combs),
        nrow = total,
        ncol = n_params,
        byrow = TRUE
    )
    condnames <- 1:n_cond
    header <- c(idnames, paramnames, actrnames)
    id_matrix <- matrix(nrow = total, ncol = length(idnames))
    actr_matrix <- matrix(nrow = total, ncol = length(actrnames))
    d <- data.frame(cbind(id_matrix, param_matrix, actr_matrix))
    colnames(d) <- header
    d$Set <- 1:n_sets
    d$Condition <- rep(condnames, each = n_sets)
    d$Target <- rep(model$Target, each = n_sets)
    d$Distractor <- rep(model$Distractor, each = n_sets)
    tmatch <- c()
    dmatch <- c()
    for (i in 1:iterations) {
        modeli <- distortion_slioussar(phon_act)
        tmatch <- c(tmatch, modeli$target_match)
        dmatch <- c(dmatch, modeli$distractor_match)
    }
    d$match1 <- tmatch
    d$match2 <- dmatch
    d$Iteration <- rep(1:iterations, each = n_sets * n_cond)
    for (i in 1:nrow(d)) {
        d$weights1[i] <- strWeight(d$cueweighting[i])
        d$weights2[i] <- numWeight(d$cueweighting[i])
    }
    return(d)
}


## ----------------------------------------------------------
## Compute condition means including error rates
## ----------------------------------------------------------
compute_int_means_errors <- function(d) {
    means <- group_by(
        d,
        Set,
        Condition,
        lf,
        ans,
        mas,
        mp,
        rth,
        bll,
        psc,
        pic,
        qcf,
        qco,
        cuesim,
        tprom,
        dprom,
        lp,
        ldp,
        blc,
        dbl,
        ndistr,
        cueweighting
    ) %>%
        summarise(
            Effect = mean(latency),
            SE = sd(latency) / sqrt(length(latency)),
            Acc = mean(acc),
            Miss = mean(miss),
            Fail = mean(fail),
            Sji_neg = sum(Sji_neg),
            .groups = "drop"
        ) %>%
        mutate(lower = Effect - SE, upper = Effect + SE)
}


## ----------------------------------------------------------
## Fixed ACT-R parameters (Engelmann et al. 2019 defaults)
## Same as Lacina et al. (2025) / Yadav et al. (2023)
## ----------------------------------------------------------
reset_params()
psc <<- 0
qcf <<- 0
cuesim <<- -1
bll <<- 0.5
mp <<- 0.15 # mismatch penalty (Engelmann et al 2019)
mas <<- 1.5 # max associative strength (Engelmann et al 2019)
ans <<- 0.2 # activation noise (Engelmann et al 2019)
rth <<- -1.5 # retrieval threshold
dbl <<- 0
cueweighting <<- 1

cat("\n=== Fixed ACT-R parameters (Engelmann et al. 2019) ===\n")
cat(sprintf(
    "  mp  = %.2f\n  mas = %.1f\n  ans = %.2f\n  bll = %.1f\n  rth = %.1f\n\n",
    mp,
    mas,
    ans,
    bll,
    rth
))


## ----------------------------------------------------------
## Sampler: draw lf from prior, run model, record errors
## Following Lacina et al. approach: only lf is free
## ----------------------------------------------------------
iterate_lf_errors <- function(values, phon_act_val, iterations = 1000) {
    maxset <- 0
    means <- NULL
    for (v in 1:nrow(values)) {
        lf <<- values[v]$latency_f
        phon_activation <<- phon_act_val
        model <- distortion_slioussar(phon_act_val)
        pmatr <- create_param_matrix_4cond(phon_act_val, model, iterations)
        results <- run(pmatr)
        means2 <- compute_int_means_errors(results)
        means2$Set <- means2$Set + maxset
        means <- bind_rows(means, means2)
    }
    means
}


sampler <- function(phon_act_val, npart = 2000) {
    cat(sprintf(
        "  Sampling %d lf values (phon_activation = %.2f)...\n",
        npart,
        phon_act_val
    ))

    df.pool <- data.frame(matrix(ncol = 12, nrow = 0))
    colnames(df.pool) <- c(
        "sample_id",
        "latency_f",
        "phon_act",
        "xsim_condA",
        "xsim_condB",
        "xsim_condC",
        "xsim_condD",
        "miss_condA",
        "miss_condB",
        "miss_condC",
        "miss_condD",
        "fail_condA"
    )

    for (i in 1:npart) {
        if (i %% 200 == 0) {
            cat(sprintf("    Progress: %d/%d\n", i, npart))
        }

        latency_f <- rtruncnorm(1, a = 0.05, b = 1, mean = 0.15, sd = 0.05)
        proposal <- data.frame(latency_f)

        generated <- iterate_lf_errors(proposal, phon_act_val)

        df.pool[nrow(df.pool) + 1, ] <- c(
            i,
            latency_f,
            phon_act_val,
            generated$Effect[1],
            generated$Effect[2],
            generated$Effect[3],
            generated$Effect[4],
            generated$Miss[1],
            generated$Miss[2],
            generated$Miss[3],
            generated$Miss[4],
            generated$Fail[1]
        )
    }
    df.pool
}


## ----------------------------------------------------------
## Run under two structural assumptions
## ----------------------------------------------------------

## Assumption 1: Full syncretism (phon_activation = 1)
cat("\n=== Running: Full syncretism (phon_activation = 1.0) ===\n")
old <- Sys.time()
results_full <- sampler(phon_act_val = 1.0, npart = 2000)
cat(sprintf("  Time: %s\n", format(Sys.time() - old)))
save(results_full, file = "forms_features/slioussar_predictions_full_sync.Rda")

## Assumption 2: Half syncretism (phon_activation = 0.5)
cat("\n=== Running: Half syncretism (phon_activation = 0.5) ===\n")
old <- Sys.time()
results_half <- sampler(phon_act_val = 0.5, npart = 2000)
cat(sprintf("  Time: %s\n", format(Sys.time() - old)))
save(results_half, file = "forms_features/slioussar_predictions_half_sync.Rda")


## ----------------------------------------------------------
## Summarize predicted error rates
## ----------------------------------------------------------
## RMSD function
rmsd <- function(obs, pred) {
    sqrt(mean((obs - pred)^2))
}

## Target error rates (Experiment 2, singular heads, ungrammatical)
target_errors <- c(
    AccSg = 0.048, # Acc.Sg: baseline
    AccPl = 0.234, # Acc.Pl (syncretic): highest
    GenSg = 0.145, # Gen.Sg (syncretic): second highest
    GenPl = 0.095 # Gen.Pl: number-only
)

## Also store Experiment 1 (production) for reference
target_errors_prod <- c(
    AccSg = 0.000,
    AccPl = 0.153,
    GenSg = 0.041,
    GenPl = 0.006
)

## Process results for both assumptions
summarize_predictions <- function(results, assumption_label) {
    summary <- results %>%
        summarise(
            Assumption = assumption_label,
            AccSg_Miss = mean(miss_condA),
            AccPl_Miss = mean(miss_condB),
            GenSg_Miss = mean(miss_condC),
            GenPl_Miss = mean(miss_condD),
            AccSg_Miss_SD = sd(miss_condA),
            AccPl_Miss_SD = sd(miss_condB),
            GenSg_Miss_SD = sd(miss_condC),
            GenPl_Miss_SD = sd(miss_condD),
            AccSg_RT = mean(xsim_condA),
            AccPl_RT = mean(xsim_condB),
            GenSg_RT = mean(xsim_condC),
            GenPl_RT = mean(xsim_condD)
        )

    pred <- c(
        summary$AccSg_Miss,
        summary$AccPl_Miss,
        summary$GenSg_Miss,
        summary$GenPl_Miss
    )
    summary$RMSD_Exp2 <- rmsd(target_errors, pred)
    summary$RMSD_Exp1 <- rmsd(target_errors_prod, pred)
    summary
}

summary_full <- summarize_predictions(results_full, "Full syncretism (1.0)")
summary_half <- summarize_predictions(results_half, "Half syncretism (0.5)")

cat("\n\n========================================\n")
cat("=== RESULTS: Predicted Error Rates ===\n")
cat("========================================\n\n")

cat("Fixed parameters: mp=0.15, mas=1.5, ans=0.2, bll=0.5, rth=-1.5\n")
cat("lf sampled from TruncNorm(mean=0.15, sd=0.05, [0.05, 1.0])\n\n")

cat("--- Observed (Slioussar 2018, Exp. 2) ---\n")
cat(sprintf(
    "  Acc.Sg: %.1f%%   Acc.Pl: %.1f%%   Gen.Sg: %.1f%%   Gen.Pl: %.1f%%\n\n",
    target_errors[1] * 100,
    target_errors[2] * 100,
    target_errors[3] * 100,
    target_errors[4] * 100
))

cat("--- Full syncretism (phon_activation = 1.0) ---\n")
cat(sprintf(
    "  Acc.Sg: %.1f%% (SD=%.1f)   Acc.Pl: %.1f%% (SD=%.1f)   Gen.Sg: %.1f%% (SD=%.1f)   Gen.Pl: %.1f%% (SD=%.1f)\n",
    summary_full$AccSg_Miss * 100,
    summary_full$AccSg_Miss_SD * 100,
    summary_full$AccPl_Miss * 100,
    summary_full$AccPl_Miss_SD * 100,
    summary_full$GenSg_Miss * 100,
    summary_full$GenSg_Miss_SD * 100,
    summary_full$GenPl_Miss * 100,
    summary_full$GenPl_Miss_SD * 100
))
cat(sprintf(
    "  RMSD vs. Exp. 2: %.4f   RMSD vs. Exp. 1: %.4f\n\n",
    summary_full$RMSD_Exp2,
    summary_full$RMSD_Exp1
))

cat("--- Half syncretism (phon_activation = 0.5) ---\n")
cat(sprintf(
    "  Acc.Sg: %.1f%% (SD=%.1f)   Acc.Pl: %.1f%% (SD=%.1f)   Gen.Sg: %.1f%% (SD=%.1f)   Gen.Pl: %.1f%% (SD=%.1f)\n",
    summary_half$AccSg_Miss * 100,
    summary_half$AccSg_Miss_SD * 100,
    summary_half$AccPl_Miss * 100,
    summary_half$AccPl_Miss_SD * 100,
    summary_half$GenSg_Miss * 100,
    summary_half$GenSg_Miss_SD * 100,
    summary_half$GenPl_Miss * 100,
    summary_half$GenPl_Miss_SD * 100
))
cat(sprintf(
    "  RMSD vs. Exp. 2: %.4f   RMSD vs. Exp. 1: %.4f\n\n",
    summary_half$RMSD_Exp2,
    summary_half$RMSD_Exp1
))


## ----------------------------------------------------------
## Plot: Observed vs. Predicted error rates (both assumptions)
## ----------------------------------------------------------
cond_labels <- c(
    "Acc.Sg\n(no sync)",
    "Acc.Pl\n(sync=Nom.Pl)",
    "Gen.Sg\n(sync=Nom.Pl)",
    "Gen.Pl\n(no sync)"
)

plot_data <- bind_rows(
    # Observed
    data.frame(
        Condition = factor(cond_labels, levels = cond_labels),
        Source = "Observed (Exp. 2)",
        Error_Rate = target_errors,
        Lower = NA,
        Upper = NA
    ),
    # Full syncretism
    data.frame(
        Condition = factor(cond_labels, levels = cond_labels),
        Source = "Predicted: full sync (1.0)",
        Error_Rate = c(
            summary_full$AccSg_Miss,
            summary_full$AccPl_Miss,
            summary_full$GenSg_Miss,
            summary_full$GenPl_Miss
        ),
        Lower = c(
            summary_full$AccSg_Miss - summary_full$AccSg_Miss_SD,
            summary_full$AccPl_Miss - summary_full$AccPl_Miss_SD,
            summary_full$GenSg_Miss - summary_full$GenSg_Miss_SD,
            summary_full$GenPl_Miss - summary_full$GenPl_Miss_SD
        ),
        Upper = c(
            summary_full$AccSg_Miss + summary_full$AccSg_Miss_SD,
            summary_full$AccPl_Miss + summary_full$AccPl_Miss_SD,
            summary_full$GenSg_Miss + summary_full$GenSg_Miss_SD,
            summary_full$GenPl_Miss + summary_full$GenPl_Miss_SD
        )
    ),
    # Half syncretism
    data.frame(
        Condition = factor(cond_labels, levels = cond_labels),
        Source = "Predicted: half sync (0.5)",
        Error_Rate = c(
            summary_half$AccSg_Miss,
            summary_half$AccPl_Miss,
            summary_half$GenSg_Miss,
            summary_half$GenPl_Miss
        ),
        Lower = c(
            summary_half$AccSg_Miss - summary_half$AccSg_Miss_SD,
            summary_half$AccPl_Miss - summary_half$AccPl_Miss_SD,
            summary_half$GenSg_Miss - summary_half$GenSg_Miss_SD,
            summary_half$GenPl_Miss - summary_half$GenPl_Miss_SD
        ),
        Upper = c(
            summary_half$AccSg_Miss + summary_half$AccSg_Miss_SD,
            summary_half$AccPl_Miss + summary_half$AccPl_Miss_SD,
            summary_half$GenSg_Miss + summary_half$GenSg_Miss_SD,
            summary_half$GenPl_Miss + summary_half$GenPl_Miss_SD
        )
    )
)

pos <- position_dodge(width = 0.5)

p <- ggplot(
    plot_data,
    aes(x = Condition, y = Error_Rate, color = Source, group = Source)
) +
    geom_point(size = 3, position = pos) +
    geom_errorbar(
        aes(ymin = Lower, ymax = Upper),
        width = 0.2,
        position = pos,
        na.rm = TRUE
    ) +
    scale_color_manual(
        values = c(
            "Observed (Exp. 2)" = "firebrick",
            "Predicted: full sync (1.0)" = "steelblue",
            "Predicted: half sync (0.5)" = "grey50"
        )
    ) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 0.40)) +
    labs(
        title = "Slioussar (2018) Exp. 2: Observed vs. Predicted Error Rates",
        subtitle = "Fixed params: mp=0.15, mas=1.5, ans=0.2 (Engelmann et al. 2019); lf sampled from prior",
        y = "Agreement Error Rate (Miss %)",
        x = "Attractor Condition",
        color = ""
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "top", plot.title = element_text(face = "bold"))

ggsave("forms_features/fit_slioussar_errors.pdf", p, width = 10, height = 7)
cat("Plot saved to forms_features/fit_slioussar_errors.pdf\n")


## ----------------------------------------------------------
## Distribution plot: error rates across lf samples
## ----------------------------------------------------------
dist_data <- bind_rows(
    results_full %>%
        select(sample_id, latency_f, miss_condA:miss_condD) %>%
        pivot_longer(
            miss_condA:miss_condD,
            names_to = "Condition",
            values_to = "Miss"
        ) %>%
        mutate(
            Assumption = "Full syncretism (1.0)",
            Condition = recode(
                Condition,
                "miss_condA" = "Acc.Sg (no sync)",
                "miss_condB" = "Acc.Pl (sync)",
                "miss_condC" = "Gen.Sg (sync)",
                "miss_condD" = "Gen.Pl (no sync)"
            )
        ),
    results_half %>%
        select(sample_id, latency_f, miss_condA:miss_condD) %>%
        pivot_longer(
            miss_condA:miss_condD,
            names_to = "Condition",
            values_to = "Miss"
        ) %>%
        mutate(
            Assumption = "Half syncretism (0.5)",
            Condition = recode(
                Condition,
                "miss_condA" = "Acc.Sg (no sync)",
                "miss_condB" = "Acc.Pl (sync)",
                "miss_condC" = "Gen.Sg (sync)",
                "miss_condD" = "Gen.Pl (no sync)"
            )
        )
)

p2 <- ggplot(dist_data, aes(x = Miss, fill = Condition)) +
    geom_density(alpha = 0.5) +
    facet_wrap(~Assumption, ncol = 1) +
    scale_x_continuous(labels = scales::percent) +
    labs(
        title = "Distribution of Predicted Error Rates Across lf Samples",
        subtitle = "Fixed params: mp=0.15, mas=1.5, ans=0.2; lf ~ TruncNorm(0.15, 0.05)",
        x = "Error Rate (Miss %)",
        y = "Density",
        fill = "Condition"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top", plot.title = element_text(face = "bold"))

ggsave(
    "forms_features/fit_slioussar_distributions.pdf",
    p2,
    width = 10,
    height = 8
)
cat(
    "Distribution plot saved to forms_features/fit_slioussar_distributions.pdf\n"
)


## ----------------------------------------------------------
## Save all results
## ----------------------------------------------------------
save(
    results_full,
    results_half,
    summary_full,
    summary_half,
    target_errors,
    target_errors_prod,
    file = "forms_features/fit_results.Rda"
)
cat("Results saved to forms_features/fit_results.Rda\n")

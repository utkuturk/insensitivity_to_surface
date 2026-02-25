rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(parallel)
library(LaplacesDemon)
library(truncnorm)

## some helper functions:
rmsd <- function(obs, pred) {
    sqrt(mean((obs - pred)^2, na.rm = TRUE))
}

# compute condition means:
compute_int_means <- function(d) {
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
            Sji_neg = sum(Sji_neg)
        ) %>%
        ungroup() %>%
        mutate(lower = Effect - SE, upper = Effect + SE)
}


## Load the phonological model engine
source("interACT_2features_phon.R")


## Basic engine for generating predictions
printcounts <- FALSE

iterate_lf <- function(values, iterations = 1000) {
    ## values is a dataframe containing latency factor values
    ## iterations is the number of iterations per value.
    maxset <- 0
    means <- NULL
    for (v in 1:nrow(values)) {
        lf <<- values[v]$latency_f
        phon_act <<- phon_activation
        pmatr <- create_param_matrix(phon_act, distortion(phon_act), iterations)
        results <- run(pmatr)
        means2 <- compute_int_means(results)
        means2$Set <- means2$Set + maxset
        means <- bind_rows(means, means2)
    }
    means
}


## Set parameters (following Engelmann et al. 2019 defaults)
reset_params()
psc <<- 0
qcf <<- 0
cuesim <<- -1
bll <<- 0.5
mp <<- 0.15 # mismatch penalty (0.15 = Engelmann et al 2019 default)
mas <<- 1.5 # max associative strength
ans <<- 0.2 # activation noise
rth <<- -1.5 # retrieval threshold
dbl <<- 0
cueweighting <<- 1


############################################################
## Run: Full phonological activation (phon_activation = 1)
############################################################
phon_activation <<- 1

psdx <- 0.25
sampler <- function(prior_id) {
    npart <- 2000
    df.pool <- data.frame(matrix(ncol = 16, nrow = 0))
    colnames(df.pool) <- c(
        "pool_id",
        "sample_id",
        "latency_f",
        "prior.prob",
        "weight",
        "likelihood",
        "imp_density",
        "xsim_condA",
        "xsim_condB",
        "xsim_condC",
        "xsim_condD",
        "xsim_condE",
        "xsim_condF",
        "xsim_condG",
        "xsim_condH",
        "ESS"
    )
    for (i in 1:npart) {
        latency_f <<- rtruncnorm(1, a = 0.05, b = 1, mean = 0.15, sd = 0.05)
        proposal <- data.frame(latency_f)
        ## get generated effect for all 8 conditions:
        generated_effect <- iterate_lf(proposal)$Effect
        xsim_condA <- generated_effect[1]
        xsim_condB <- generated_effect[2]
        xsim_condC <- generated_effect[3]
        xsim_condD <- generated_effect[4]
        xsim_condE <- generated_effect[5]
        xsim_condF <- generated_effect[6]
        xsim_condG <- generated_effect[7]
        xsim_condH <- generated_effect[8]
        df.pool[length(df.pool$pool_id) + 1, ] <- c(
            1,
            i,
            latency_f,
            NA,
            NA,
            NA,
            NA,
            xsim_condA,
            xsim_condB,
            xsim_condC,
            xsim_condD,
            xsim_condE,
            xsim_condF,
            xsim_condG,
            xsim_condH,
            NA
        )
    }
    df.pool
}

old <- Sys.time()
all_chains_par <- sampler(1)
head(all_chains_par)
new <- Sys.time() - old
print(new)
save(all_chains_par, file = "ACTR_predictions_phon_full_activation_mp0.15.Rda")


############################################################
## Run: Half phonological activation (phon_activation = 0.5)
############################################################
phon_activation <<- 0.5

old <- Sys.time()
all_chains_par <- sampler(1)
head(all_chains_par)
new <- Sys.time() - old
print(new)
save(all_chains_par, file = "ACTR_predictions_phon_half_activation_mp0.15.Rda")

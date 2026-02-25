library(tidyr)
library(dplyr)

###############################################################
## interACT model: 2-feature phonological similarity version
##
## Adapted from interACT_2features.R (Lacina et al. case syncretism model)
## for number agreement with phonological similarity manipulation.
##
## 2 retrieval cues/features:
##   Feature 1: Structural role (subject vs. non-subject)
##   Feature 2: Number (matching vs. mismatching verb's number cue)
##
## 8 conditions (2 x 2 x 2):
##   Grammaticality (grammatical vs. ungrammatical)
##   x Attractor number (match vs. mismatch with subject)
##   x Phonological similarity (similar vs. dissimilar)
##
## Phonological similarity logic:
##   A distractor that is phonologically similar to a word with
##   the OPPOSITE number (e.g., "cruise" sg ~ "crews" pl) partially
##   activates that number feature. This increases the distractor's
##   match to a number cue it would otherwise mismatch.
##   Analogous to case syncretism in the original model.
##
##   phon_activation parameter controls the degree:
##     1   = full activation (mismatch becomes full match)
##     0.5 = half activation (mismatch becomes partial match)
##     0   = no activation (equivalent to no phon similarity)
###############################################################

###############################################################
## Global parameters
###############################################################
reset_params <- function() {
    lf <<- 0.15 # latency factor
    le <<- 1 # latency exponent
    rth <<- -1.5 # retrieval threshold
    bll <<- 0.5 # decay parameter
    ans <<- 0.2 # activation noise
    mas <<- 1 # maximum associative strength
    mp <<- 1 # mismatch penalty
    ga <<- 1 # goal source activation
    rand_time <<- 3 # latency variability
    blc <<- 0 # base-level activation
    ##
    lp <<- 1 # default time since last presentation (msec)
    ldp <<- 1 # last distractor presentation (msec)
    ## Distractor control:
    ndistr <<- 1 # number of distractors
    dbl <<- 0 # distractor base-level
    ##
    ## Cue weighting, cue confusion and prominence:
    cueweighting <<- 1 # Strength of structural cue as ratio structural/number
    normalizeWeights <<- TRUE
    qcf <<- 1 # match quality correction factor
    qco <<- -2 * rth # match quality correction offset
    psc <<- 1 # prominence scaling constant C1
    pic <<- 0 # prominence scaling constant C2
    tprom <<- 0 # target prominence
    dprom <<- 0 # distractor prominence
    cuesim <<- -1 # cue-feature similarity [-1..0]
    #
    # Phonological activation parameter:
    phon_activation <<- 1 # degree of number activation via phonological similarity
    # 1 = full activation, 0.5 = half, 0 = none
    #
    # Fitted meta-parameters
    meta_recent <<- 0.7
    meta_distant <<- 1.3
    meta_lowprom <<- -0.5
    meta_medprom <<- 0
    meta_highprom <<- 2.5
    meta_conflevel <<- 0
    meta_memory <<- 1
    meta_deptype <<- 0
    meta_method <<- 0
    #
    VERBOSE <<- FALSE
}
reset_params()

cuesim2cl <- function(x = cuesim) {
    cl <<- (x + 1) * 100
    cl
}

cl2cuesim <- function(x = cl) {
    cuesim <<- x / 100 - 1
    cuesim
}


strWeight <- function(ratio = cueweighting, normalize = normalizeWeights) {
    ifelse(normalize, ratio / (ratio + 1) * 2, ratio)
}

numWeight <- function(ratio = cueweighting, normalize = normalizeWeights) {
    ifelse(normalize, 1 / (ratio + 1) * 2, 1)
}


idnames <- c("Set", "Iteration", "Condition", "Target", "Distractor")
actrnames <- c(
    "weights",
    "bl1",
    "bl2",
    "times1",
    "times2",
    "noise1",
    "noise2",
    "blact1",
    "blact2",
    "act1",
    "act2",
    "activation",
    "latency",
    "retrieved",
    "acc",
    "miss",
    "fail"
)
paramnames <- c(
    "lf",
    "le",
    "rth",
    "bll",
    "ans",
    "mas",
    "mp",
    "ga",
    "rand_time",
    "lp",
    "blc",
    "ldp",
    "dbl",
    "ndistr",
    "cueweighting",
    "psc",
    "pic",
    "qcf",
    "qco",
    "cuesim",
    "tprom",
    "dprom"
)

set_prameters <- function() {
    parameters <<- list(
        lf,
        le,
        rth,
        bll,
        ans,
        mas,
        mp,
        ga,
        rand_time,
        lp,
        blc,
        ldp,
        dbl,
        ndistr,
        cueweighting,
        psc,
        pic,
        qcf,
        qco,
        cuesim,
        tprom,
        dprom
    )
    names(parameters) <<- paramnames
}
set_prameters()


compute_cond_means <- function(results) {
    params <- results %>%
        select(Set:weights, -Iteration, -latency) %>%
        distinct()
    condMeans <- results %>%
        group_by(Set, Condition, Target, Distractor) %>%
        summarise(
            Latency = mean(latency),
            SE = sd(latency, na.rm = TRUE) / sqrt(n()),
            Acc = mean(acc),
            Miss = mean(miss),
            Fail = mean(fail)
        ) %>%
        ungroup()
    suppressMessages(left_join(condMeans, params))
}


###############################################################
## RUN MODEL
###############################################################

###############################################################
## 8-condition design: Phonological similarity in number agreement
###############################################################
#
# Design: 2 (Grammaticality) x 2 (Attractor number) x 2 (Phon similarity)
#
# Subject is singular. Verb cues for [subject, number].
#   Grammatical:   verb = singular -> cues [subject, singular]
#   Ungrammatical: verb = plural   -> cues [subject, plural]
#
# Match vectors: c(structural, number)
#   Target  (subject): structural = 1 (always subject)
#   Distractor (attr): structural = 0 (never subject)
#
# Phonological similarity effect (asymmetric, like syncretism):
#   Only affects the distractor when its actual number MISMATCHES the cue.
#   Phon sim activates the homophone's (opposite) number, creating a
#   partial/full match where there would otherwise be a mismatch (0).
#   When the distractor already matches the cue (1), phon sim has no
#   additional effect (it can't "double match").
#
# Full activation (phon_activation = 1):
#   target_match     = list(c(1,1), c(1,1), c(1,1), c(1,1),
#                           c(1,0), c(1,0), c(1,0), c(1,0))
#   distractor_match = list(c(0,1), c(0,1), c(0,0), c(0,1),
#                           c(0,0), c(0,1), c(0,1), c(0,1))
#
# Half activation (phon_activation = 0.5):
#   target_match     = list(c(1,1), c(1,1), c(1,1), c(1,1),
#                           c(1,0), c(1,0), c(1,0), c(1,0))
#   distractor_match = list(c(0,1), c(0,1), c(0,0), c(0,1/2),
#                           c(0,0), c(0,1/2), c(0,1), c(0,1))
#
# Alternative: symmetric ambiguity model (phon sim always -> 0.5):
#   distractor_match = list(c(0,1), c(0,0.5), c(0,0), c(0,0.5),
#                           c(0,0), c(0,0.5), c(0,1), c(0,0.5))

distortion <- function(phon_act = phon_activation) {
    # phon_act: degree of phonological activation of opposite number
    #   1   = full activation (mismatch -> 1)
    #   0.5 = half activation (mismatch -> 0.5)
    #   0   = no activation (no phon effect)

    model_8cond <- list(
        # Feature 1: structural role match (1 = subject, 0 = non-subject)
        # Feature 2: number match against verb's number cue
        target_match = list(
            c(1, 1), # a: Grammatical  — target matches both cues
            c(1, 1), # b: Grammatical
            c(1, 1), # c: Grammatical
            c(1, 1), # d: Grammatical
            c(1, 0), # e: Ungrammatical — target matches structural but not number
            c(1, 0), # f: Ungrammatical
            c(1, 0), # g: Ungrammatical
            c(1, 0) # h: Ungrammatical
        ),
        distractor_match = list(
            c(0, 1), # a: Gram, #Match, No phon — distractor matches number cue
            c(0, 1), # b: Gram, #Match, Phon sim — already matches, no change
            c(0, 0), # c: Gram, #Mismatch, No phon — distractor mismatches number cue
            c(0, phon_act), # d: Gram, #Mismatch, Phon sim — mismatch activated by phon
            c(0, 0), # e: Ungram, #Match, No phon — distractor mismatches verb cue
            c(0, phon_act), # f: Ungram, #Match, Phon sim — mismatch activated by phon
            c(0, 1), # g: Ungram, #Mismatch, No phon — distractor matches verb cue (ATTRACTION)
            c(0, 1) # h: Ungram, #Mismatch, Phon sim — already matches, no change
        ),
        Target = c(
            "Grammatical", # a
            "Grammatical", # b
            "Grammatical", # c
            "Grammatical", # d
            "Ungrammatical", # e
            "Ungrammatical", # f
            "Ungrammatical", # g
            "Ungrammatical" # h
        ),
        Distractor = c(
            "Number match, No phon sim", # a: e.g., "The key to the table was..."
            "Number match, Phon sim", # b: e.g., "The key to the cruise was..." (cruise~crews)
            "Number mismatch, No phon sim", # c: e.g., "The key to the tables was..."
            "Number mismatch, Phon sim", # d: e.g., "The key to the crews was..." (crews~cruise)
            "Number match, No phon sim", # e: e.g., "The key to the table were..."
            "Number match, Phon sim", # f: e.g., "The key to the cruise were..." (cruise~crews)
            "Number mismatch, No phon sim", # g: e.g., "The key to the tables were..."
            "Number mismatch, Phon sim" # h: e.g., "The key to the crews were..." (crews~cruise)
        ),
        weights = list(c(strWeight(), numWeight()))
    )
    model_8cond
}


##
## PARAMETER MATRIX
##
create_param_matrix <- function(
    phon_act,
    model,
    iterations = 1000,
    verbose = FALSE
) {
    if (verbose) {
        print("creating parameter matrix...")
    }
    #
    cl <<- (cuesim + 1) * 100
    set_prameters()
    #
    n_params <- length(parameters)
    #
    ## The total number of combinations is the product of the number of values
    ## for each parameter
    n_sets <- prod(unlist(lapply(parameters, length)))
    n_cond <- length(model$target_match)
    total <- iterations * n_sets * n_cond
    #
    if (verbose) {
        print(paste("Conditions: ", n_cond))
        print(paste("Combinations: ", n_sets))
        print(paste("Total runs: ", total))
    }
    #
    ## Set up matrix of parameter combinations.  Rows are model experiments,
    ## columns are parameters.
    param_combs <- matrix(nrow = n_sets, ncol = n_params)
    #
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
    #
    param_matrix <- matrix(
        data = t(param_combs),
        nrow = total,
        ncol = n_params,
        byrow = TRUE
    )
    #
    condnames <- 1:length(model$target_match)
    header <- c(idnames, paramnames, actrnames)
    #
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
        modeli <- distortion(phon_act)
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


run <- function(d) {
    if (VERBOSE) {
        print("computing base-levels...")
    }
    #
    total <- nrow(d)
    d$times1 <- d$lp
    d$times2 <- d$ldp
    d$bl1 <- d$blc
    d$bl2 <- d$dbl
    d$cl <- (cuesim + 1) * 100
    #
    d$noise1 <- act_r_noise_n(total, d$ans)
    d$noise2 <- act_r_noise_n(total, d$ans)
    d$blact1 <- base_act(
        prom = d$tprom,
        times = d$times1,
        C1 = d$pic,
        C2 = d$psc,
        b = d$bl1,
        noise = d$noise1,
        dec = d$bll
    )
    d$blact2 <- base_act(
        prom = d$dprom,
        times = d$times2,
        C1 = d$pic,
        C2 = d$psc,
        b = d$bl2,
        noise = d$noise2,
        dec = d$bll
    )
    #
    ## Cue match
    match1 <- matrix(
        unlist(d$match1),
        nrow = total,
        ncol = length(d$match1[[1]]),
        byrow = TRUE
    )
    match2 <- matrix(
        unlist(d$match2),
        nrow = total,
        ncol = length(d$match2[[1]]),
        byrow = TRUE
    )
    weights <- select(d, weights1, weights2) %>% as.matrix()
    #
    ## Match quality (including cue confusion and scaled by base-level activation)
    cueconf1 <- match1 - ((match1 - 1) * (1 + d$cuesim))
    cueconf1[cueconf1 == 0] <- 1
    cueconf2 <- match2 - ((match2 - 1) * (1 + d$cuesim))
    cueconf2[cueconf2 == 0] <- 1
    if (VERBOSE) {
        print("computing match quality...")
    }
    Qj1 <- match_quality(
        match = match1,
        sim = d$cuesim,
        blact = d$blact1,
        tau = d$rth,
        s = d$ans,
        q = d$qcf,
        q2 = d$qco
    )
    Qj2 <- match_quality(
        match = match2,
        sim = d$cuesim,
        blact = d$blact2,
        tau = d$rth,
        s = d$ans,
        q = d$qcf,
        q2 = d$qco
    )
    #
    ## Probability of item given cue
    if (VERBOSE) {
        print("computing probability P(i|j)...")
    }
    P1j <- Qj1 / (Qj1 + (Qj2 * d$ndistr) + 0.0001)
    P2j <- Qj2 / (Qj1 + (Qj2 * d$ndistr) + 0.0001)
    #
    ## Spreading activation
    if (VERBOSE) {
        print("computing spreading activation...")
    }
    Sj1 <- spreading_act(Pij = P1j, S = d$mas)
    Sj2 <- spreading_act(Pij = P2j, S = d$mas)
    d$Sji_neg <- rowMeans(ifelse(Sj1 < 0 | Sj2 < 0, T, F))
    #
    ## Activations
    if (VERBOSE) {
        print("computing activations...")
    }
    d$act1 <- d$blact1 +
        weight_spreading_act(
            Sji = Sj1,
            weights = weights * cueconf1,
            W = d$ga
        ) +
        mismatch_penalty(cuematch = match1, P = d$mp) +
        d$noise1
    d$act2 <- d$blact2 +
        weight_spreading_act(
            Sji = Sj2,
            weights = weights * cueconf2,
            W = d$ga
        ) +
        mismatch_penalty(cuematch = match2, P = d$mp) +
        d$noise2

    ##
    ## FINAL VALUES
    ##
    if (VERBOSE) {
        print("computing latencies...")
    }
    d$activation <- ifelse(d$act1 > d$act2, d$act1, d$act2)
    retrieved <- ifelse(d$act1 > d$act2, 1, 2)
    d$retrieved <- ifelse(d$activation > d$rth, retrieved, 0)
    d$latency <- latency(d$activation, F = d$lf, f = d$le, tau = d$rth)
    d$acc <- ifelse(d$retrieved == 1, 1, 0)
    d$miss <- ifelse(d$retrieved == 2, 1, 0)
    d$fail <- ifelse(d$retrieved == 0, 1, 0)
    #
    if (VERBOSE) {
        print("FINISHED")
    }
    #
    return(tibble::as_tibble(d))
}


###############################################################
## ACT-R functions
###############################################################
activation <- function(
    Pij = matrix(c(1, 1), 1, 2, T),
    prom = 0,
    weights = matrix(c(1, 1), 1, 2, T),
    times = lp,
    b = blc,
    noise = act_r_noise(ans),
    W = ga,
    dec = bll,
    S = mas,
    P = mp,
    C1 = pic,
    C2 = psc
) {
    baseact <- base_act(
        prom = prom,
        times = times,
        C1 = C1,
        C2 = C2,
        b = b,
        noise = noise,
        dec = dec
    )
    Sji <- S + log(Pij)
    Sji <- ifelse(Sji == -Inf | is.na(Sji), 0, Sji)
    ifelse(Sji < 0, print("!!! WARNING: Sji < 0 !!!"), T)
    Sji <- ifelse(Sji < 0, 0, Sji)
    Wkj <- W * weights / rowSums(weights)
    penalty <- Pij - 1
    Pi <- rowSums(P * penalty)
    act <- baseact + rowSums(Wkj * Sji) + Pi + noise
    return(act)
}


spreading_act <- function(Pij = matrix(c(1, 1), 1, 2, T), S = mas) {
    Sji <- S + log(Pij)
    Sji <- ifelse(Sji == -Inf | is.na(Sji), 0, Sji)
    ifelse(Sji < 0, print("!!! WARNING: Sji < 0 !!!"), T)
    Sji <- ifelse(Sji < 0, 0, Sji)
    Sji
}

weight_spreading_act <- function(
    Sji = matrix(c(0, 0), 1, 2, T),
    weights = matrix(c(1, 1), 1, 2, T),
    W = ga
) {
    Wkj <- W * weights / rowSums(weights)
    rowSums(Wkj * Sji)
}

mismatch_penalty <- function(cuematch = matrix(c(1, 1), 1, 2, T), P = mp) {
    penalty <- cuematch - 1
    Pi <- rowSums(P * penalty)
    Pi
}


base_act <- function(
    prom = 0,
    times = lp,
    C1 = pic,
    C2 = psc,
    b = blc,
    noise = act_r_noise(ans),
    dec = bll
) {
    log(times^(-dec)) + b + (C2 * (prom + C1))
}


match_quality <- function(
    match = matrix(c(1, 1), 1, 2, T),
    sim = cuesim,
    blact = 0,
    tau = rth,
    s = ans,
    q = qcf,
    q2 = qco
) {
    qcorr <- 1 / (1 + q * exp(-(blact - tau - q2)))
    conf <- match[, c(2, 1)] * (1 + sim)
    (match + conf) * qcorr
}


latency <- function(A, F = lf, f = le, tau = rth) {
    t <- ifelse(A >= tau, F * exp(-f * A) * 1000, F * exp(-f * tau) * 1000)
    round(randomize_time(t))
}


act_r_noise_n <- function(n, s = ans) {
    var <- pi^2 / 3 * s^2
    rnorm(n, 0, sqrt(var))
}

act_r_noise <- function(s = ans) {
    var <- pi^2 / 3 * s^2
    rnorm(1, 0, sqrt(var))
}


## Random component ##
randomize_time <- function(time, n = rand_time) {
    if (n > 0) {
        runif(length(time), time * (n - 1) / n, time * (n + 1) / n)
    } else {
        time
    }
}


noise_off <- function() {
    ans <<- 0
    rand_time <<- 0
}

noise_on <- function() {
    ans <<- 0.15
    rand_time <<- 3
}

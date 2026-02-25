rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(parallel)
library(LaplacesDemon)
library(truncnorm)

## some helper functions:
rmsd <- function (obs, pred) {
  sqrt(mean((obs - pred)^2, na.rm = TRUE))
}

# This computes differences between conditions
# compute_int_means <- function(d){
#   # in semantic match
#   sem_match <- select(filter(d, Distractor=="Number mismatch, Semantic match"), -Condition)
#   sem_match$int <- filter(d, Distractor=="Number mismatch, Semantic match")$latency - filter(d, Distractor=="Number match, Semantic match")$latency
#   means_sem_match <- group_by(sem_match, Set, Target, Distractor, lf, ans, mas, mp, rth, bll, psc, pic, qcf, qco, cuesim, tprom, dprom, lp, ldp, blc, dbl, ndistr, cueweighting) %>% summarise(Effect=mean(int), SE=sd(int)/sqrt(length(int)), Sji_neg=sum(Sji_neg)) %>% ungroup() %>% mutate(lower=Effect-SE, upper=Effect+SE)
#   
#   # in semantic mismatch
#   sem_mismatch <- select(filter(d, Distractor=="Number mismatch, Semantic mismatch"), -Condition)
#   sem_mismatch$int <- filter(d, Distractor=="Number mismatch, Semantic mismatch")$latency - filter(d, Distractor=="Number match, Semantic mismatch")$latency
#   means_sem_mismatch <- group_by(sem_mismatch, Set, Target, Distractor, lf, ans, mas, mp, rth, bll, psc, pic, qcf, qco, cuesim, tprom, dprom, lp, ldp, blc, dbl, ndistr, cueweighting) %>% summarise(Effect=mean(int), SE=sd(int)/sqrt(length(int)), Sji_neg=sum(Sji_neg)) %>% ungroup() %>% mutate(lower=Effect-SE, upper=Effect+SE)
#   means = bind_rows(means_sem_match, means_sem_mismatch)
#   means
# }

# this computes condition means:
compute_int_means <- function(d){
  means <- group_by(d, Set,
                    Condition,
                    lf, ans, mas, mp, rth, bll, psc, pic, 
                    qcf, qco, cuesim, tprom, dprom, lp, 
                    ldp, blc, dbl, ndistr, cueweighting) %>% 
    summarise(Effect=mean(latency), 
              SE=sd(latency)/sqrt(length(latency)), 
              Sji_neg=sum(Sji_neg)) %>% 
    ungroup() %>% 
    mutate(lower=Effect-SE, upper=Effect+SE)
}

convert2log <- function(x){
  ifelse(x>=1, log(x), ifelse(x<=-1, -log(abs(x)), 0))
}

convert2log10 <- function(x){
  x <- ifelse(x>-1 & x<1, 0, x)
  x <- ifelse(x<=-1, -log10(abs(x)), x) 
  x <- ifelse(x>=1, log10(abs(x)), x)
}


# Introduction 
## Load interact

source("../Models/interACT_2features.R")

# or, if you want to run the 3-feature model,
# source("../Models/interACT_3features.R")

## Basic engine for generating predictions
printcounts<-FALSE

iterate_lf <- function(values,iterations=1000){
  ## values is a dataframe contaning either 1 row of parameters (a combination of latency   factor and cue-weighting parameter, or multiple rows i.e., multiple combinations of two parameters)
  ## iterations is the number of iterations for that given value.
  ## We need multiple iterations as noise is non-zero and there will be some 
  ## variability due to noise. 
  maxset <- 0
  means <- NULL
  for(v in 1:nrow(values)){
    lf <<- values[v]$latency_f  #values[v,]$latency_f
    perc <<- 0
    pmatr <- create_param_matrix(perc,distortion(perc),iterations) 
    results <- run(pmatr)
    means2 <- compute_int_means(results)
    means2$Set <- means2$Set+maxset
    means <- bind_rows(means, means2)
  }
  means
}


reset_params()
psc <<- 0
qcf <<- 0
cuesim <<- -1
bll <<- 0.5
mp <<- 0.15 #  1 is default in ACT-R; 0.15 is default in Engelmann et al 2019 Cog Sci paper
mas <<- 1.5 ## could change this to a random starting value: 
## mas <- runif(1,min=1,max=2)
## mas<<-sort(rnorm(50,mean=1.5,sd=0.25))
# default in Engelmann et al 2019 Cog Sci paper
ans <<- 0.2
# default in Engelmann et al 2019 Cog Sci paper
rth <<-  -1.5
dbl <<- 0
cueweighting <<- 1 
#perc <<- 0


############

psdx <- 0.25
sampler <- function(prior_id){
  npart <- 2000
  reject <- 0
  df.pool <- data.frame(matrix(ncol = 14, nrow = 0))          
  colnames(df.pool) <- c("pool_id","sample_id","latency_f","prior.prob",
                         "weight","likelihood","imp_density",
                         "xsim_condA", "xsim_condB", "xsim_condC",
                         "xsim_condD", "xsim_condE","xsim_condF", 
                         "ESS")
  for(i in 1:npart){
    latency_f <<- rtruncnorm(1,a=0.05,b=1,mean=0.15,sd=0.05)
    proposal <- data.frame(latency_f)
    ## get generated effect for ungrammatical conditions:   
    generated_effect <- iterate_lf(proposal)$Effect
    xsim_condA <- generated_effect[1]
    xsim_condB <- generated_effect[2]
    xsim_condC <- generated_effect[3] 
    xsim_condD <- generated_effect[4] 
    xsim_condE <- generated_effect[5] 
    xsim_condF <- generated_effect[6] 
    df.pool[length(df.pool$pool_id)+1,]<-c(1,i,latency_f,NA,NA,NA,NA,
                                           xsim_condA, xsim_condB, 
                                           xsim_condC, 
                                           xsim_condD, xsim_condE, xsim_condF,
                                           NA)
  }
  df.pool
}

old <- Sys.time()
all_chains_par <- sampler(1)
head(all_chains_par)
new <- Sys.time() - old
print(new)
save(all_chains_par,file="../ACTR_predictions_2features_full_match_mp0.15.Rda")

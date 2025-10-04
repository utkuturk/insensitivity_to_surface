set.seed(01110011)
packages <- c("here", "yaml", "ggplot2", "dplyr", "magrittr", "brms", "rstan")
lapply(packages, require, character.only = TRUE)

## Save compiled models:
rstan_options(auto_write = TRUE)
rstan_options(silent = TRUE, open_progress=FALSE,show_messages=FALSE)
rstan_options(javascript = FALSE)
## Use all the cores:
options(mc.cores = parallel::detectCores())

theme_set(theme_bw(base_family = "Times"))

# config <- yaml::read_yaml("scripts/config.yml")
# load(config$exp2$out$dataPrep)
# config <- yaml::read_yaml("scripts/config.yml")
options(brms.file_refit = config$refit)
## MODELS -----

exp2.dfModel <- exp2.merged %>% subset(experiment != "Lago et al. (2019)")
exp2.dfModel %<>% subset(match != "filler")

exp2.dfModel %<>% within(., {
    c_form <- ifelse(experiment != "Experiment 2", .5, -.5)
    c_ung <- ifelse(grammatical == "ungrammatical", .5, -.5)
    c_att <- ifelse(attractor_num == "plural", .5, -.5)
    c_mis <- ifelse(attractor_num == "plural", .5, -.5)
    l_trial <- log(trial_no)
})

## old

exp2and1.formula <- response_yes ~ c_form * c_ung * c_mis + l_trial +
                            (c_ung * c_mis + 1 | subject) +
                            (c_ung * c_mis + l_trial + 1 | item)

exp2.priors <- c(
    set_prior("student_t(3,0,2.5)", class = "Intercept"),
    set_prior("normal(0,1)", class = "b"),
    set_prior("cauchy(0,1)", class = "sd"),
    set_prior("lkj(2)", class = "cor")
)

exp2and1.mresp <- brm(
    formula = exp2and1.formula,
    data = exp2.dfModel,
    family = bernoulli("probit"),
    prior = exp2.priors,
    chains = config$exp2$model$chains,
    cores = config$exp2$model$cores,
    iter = config$exp2$model$iter,
    warmup = config$exp2$model$warmup,
    init_r = .1,
    file = paste0(config$exp2$model$path, "exp2and1.mresp")
)


exp2and1.coefs <- c(
    "c_ung" = "Ungrammaticality",
    "c_mis" = "Plural Attactor",
    "c_ung:c_mis" = "Ungrammaticality * Plural Attractor",
    "c_form" = "Genitive Modifier",
    "l_trial" = "Trial No (log)",
    "c_form:c_ung" = "Genitive Modifier * Ungrammaticality",
    "c_form:c_mis" = "Genitive Modifier * Plural Attractor",
    "c_form:c_ung:c_mis" = "Genitive Modifier * Attraction\n(Genitive Modifier * Ungram. * Pl. Attractor)"
)


exp2and1.pmresp <- create_model_coefs_plot(
    exp2and1.mresp,
    plot_stats = T, map_names = exp2and1.coefs,
    expand_right = 1, expand_top = 1.5,
    x_stat_adjust = 0.8, x_breaks = -4:2
) + annotate(
    x = -4, xend = 2, y = 0, yend = 0, 
    lwd = 0.25, geom = "segment"
) + xlab("Estimate (probit)")


exp2.formula <- response_yes ~ c_ung * c_mis + l_trial +
                            (c_ung * c_mis + 1 | subject) +
                            (c_ung * c_mis + l_trial + 1 | item)


exp2.mresp <- brm(
    formula = exp2.formula,
    data = subset(exp2.dfModel, experiment == "Experiment 2"),
    family = bernoulli("probit"),
    prior = exp2.priors,
    chains = config$exp2$model$chains,
    cores = config$exp2$model$cores,
    iter = config$exp2$model$iter,
    warmup = config$exp2$model$warmup,
    init_r = .1,
    file = paste0(config$exp2$model$path, "exp2.mresp")
)

exp2.coefs <- c(
    "c_ung" = "Ungrammaticality",
    "c_mis" = "Plural Attactor",
    "l_trial" = "Trial No (log)",
    "c_ung:c_mis" = "Ungrammaticality * Plural Attractor"
)


exp2.pmresp <- create_model_coefs_plot(
    exp2.mresp,
    plot_stats = T, map_names = exp2.coefs,
    expand_right = 1, expand_top = 2.6,
    x_stat_adjust = 0.9, x_breaks = -4:1
) + annotate(
    x = -4, xend = 1, y = 0, yend = 0, 
    lwd = 0.25, geom = "segment"
) + xlab("Estimate (probit)")


exp2.mdf <- fixef(exp2.mresp) %>% as.data.frame()
count = 1
for (i in row.names(exp2.mdf)) {
    temp <- print_estimate_with_ci(exp2.mresp, i)
    exp2.mdf$text[count] = temp
    count = count + 1
}

exp2and1.mdf <- fixef(exp2and1.mresp) %>% as.data.frame()
count = 1
for (i in row.names(exp2and1.mdf)) {
    temp <- print_estimate_with_ci(exp2and1.mresp, i)
    exp2and1.mdf$text[count] = temp
    count = count + 1
}


exp2.models <- list(
    Model = exp2.mresp,
    dfModel = exp2.mdf,
    Plot = exp2.pmresp,
    and1Model = exp2and1.mresp,
    dfand1Model = exp2and1.mdf,
    and1Plot = exp2and1.pmresp
)

# saveRDS(exp2.models, file = config$exp2$out$model)

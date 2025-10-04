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
# load(config$exp2b$out$dataPrep)
# config <- yaml::read_yaml("scripts/config.yml")
options(brms.file_refit = config$refit)
## MODELS -----

exp2b.dfModel <- exp2b.clean
exp2b.dfModel %<>% subset(match != "filler")

exp2b.dfModel %<>% within(., {
    c_form <- ifelse(att_type != "rc", .5, -.5)
    c_ung <- ifelse(grammatical == "ungram", .5, -.5)
    c_att <- ifelse(attractor_num == "pl", .5, -.5)
    c_mis <- ifelse(attractor_num == "pl", .5, -.5)
    l_trial <- log(trial_no)
})

## old

exp2b.formula <- response_yes ~ c_form * c_ung * c_mis + l_trial +
                            (c_ung * c_mis + 1 | subject) +
                            (c_ung * c_mis + l_trial + 1 | item)

exp2b.priors <- c(
    set_prior("student_t(3,0,2.5)", class = "Intercept"),
    set_prior("normal(0,1)", class = "b"),
    set_prior("cauchy(0,1)", class = "sd"),
    set_prior("lkj(2)", class = "cor")
)

exp2b.mresp <- brm(
    formula = exp2b.formula,
    data = exp2b.dfModel,
    family = bernoulli("probit"),
    prior = exp2b.priors,
    chains = config$exp2b$model$chains,
    cores = config$exp2b$model$cores,
    iter = config$exp2b$model$iter,
    warmup = config$exp2b$model$warmup,
    init_r = .1,
    file = paste0(config$exp2b$model$path, "exp2b.mresp")
)


exp2b.coefs <- c(
    "c_ung" = "Ungrammaticality",
    "c_mis" = "Plural Attactor",
    "c_ung:c_mis" = "Ungrammaticality * Plural Attractor",
    "c_form" = "Genitive Attractor",
    "l_trial" = "Trial No (log)",
    "c_form:c_ung" = "Gen. Attractor * Ungrammaticality",
    "c_form:c_mis" = "Gen. Attractor * Plural Attractor",
    "c_form:c_ung:c_mis" = "Gen. Attractor * Attraction\n            Ungrammaticality * Plural Attractor"
)

exp2bRC.formula <- response_yes ~ c_ung * c_mis + l_trial +
                            (c_ung * c_mis + 1 | subject) +
                            (c_ung * c_mis + l_trial + 1 | item)

exp2bRC.dfModel <- exp2b.dfModel %>% subset(att_type == "rc")

exp2bRC.mresp <- brm(
    formula = exp2bRC.formula,
    data = exp2bRC.dfModel,
    family = bernoulli("probit"),
    prior = exp2b.priors,
    chains = config$exp2b$model$chains,
    cores = config$exp2b$model$cores,
    iter = config$exp2b$model$iter,
    warmup = config$exp2b$model$warmup,
    init_r = .1,
    file = paste0(config$exp2b$model$path, "exp2bRC.mresp")
)

exp2bRC.coefs <- c(
    "c_ung" = "Ungrammaticality",
    "c_mis" = "Plural Attactor",
    "c_ung:c_mis" = "Ungrammaticality * Plural Attractor",
    "l_trial" = "Trial No (log)"
)


exp2b.pmresp <- create_model_coefs_plot(
    exp2b.mresp,
    plot_stats = T, map_names = exp2b.coefs,
    expand_right = 1.5, expand_top = 1.5,
    x_stat_adjust = 0.9, x_breaks = -4:2
) + annotate(
    x = -4, xend = 2, y = 0, yend = 0, 
    lwd = 0.25, geom = "segment"
) + xlab("Estimate (probit)")


exp2bRC.pmresp <- create_model_coefs_plot(
    exp2bRC.mresp,
    plot_stats = T, map_names = exp2bRC.coefs,
    expand_right = 1.5, expand_top = 2.6,
    x_stat_adjust = 0.9, x_breaks = -5:1
) + annotate(
    x = -5, xend = 1, y = 0, yend = 0, 
    lwd = 0.25, geom = "segment"
) + xlab("Estimate (probit)")


exp2b.mdf <- fixef(exp2b.mresp) %>% as.data.frame()
count = 1
for (i in row.names(exp2b.mdf)) {
    temp <- print_estimate_with_ci(exp2b.mresp, i)
    exp2b.mdf$text[count] = temp
    count = count + 1
}

exp2bRC.mdf <- fixef(exp2bRC.mresp) %>% as.data.frame()
count = 1
for (i in row.names(exp2bRC.mdf)) {
    temp <- print_estimate_with_ci(exp2bRC.mresp, i)
    exp2bRC.mdf$text[count] = temp
    count = count + 1
}

exp2b.models <- list(
    Model = exp2b.mresp,
    dfModel = exp2b.mdf,
    Plot = exp2b.pmresp,
    RCModel = exp2bRC.mresp,
    RCdfModel = exp2bRC.mdf,
    RCPlot = exp2bRC.pmresp
)

# saveRDS(exp2b.models, file = config$exp2b$out$model)

set.seed(01110011)
packages <- c("here", "yaml", "ggplot2", "dplyr", "magrittr", "patchwork", "ggstatsplot")
lapply(packages, require, character.only = TRUE)

theme_set(theme_bw(base_family = "Times"))

# config <- yaml::read_yaml("scripts/config.yml")
# load(config$exp2$out$dataPrep)
# config <- yaml::read_yaml("scripts/config.yml")

## PLOTS -------
exp2.plots <- list()


exp2.gram.label <- c(
    grammatical = "Grammatical\n(Singular Verb)",
    ungrammatical = "Ungrammatical\n(Plural Verb)"
)

exp2.x.order <- c(
    "Experiment 2", "Experiment 1", "Lago et al. (2019)"
)

exp2.x.label <- c(
    "Exp. 2", "Exp. 1", "Lago et al. (2019)"
)
# responses 

exp2.resp.avg.plot <- exp2.avgs %>%
    ggplot(aes(resp.experiment, resp.M, 
            linetype = resp.attractor_num, 
            group = resp.attractor_num)) + 
    geom_point() + geom_line() + 
    facet_wrap(~resp.grammatical, 
                labeller = labeller(resp.grammatical = exp2.gram.label),
                scales = "free_y") + 
    geom_errorbar(aes(ymin = resp.M - 1.96*resp.SE, 
                    ymax = resp.M + 1.96*resp.SE), 
                width = 0.1) + 
    theme(strip.background = element_rect(fill="white")) +
    xlab("Experiment") + 
    ylab("Percentage 'acceptable'") + 
    scale_y_continuous(labels=scales::percent) + 
    scale_linetype_discrete(name = "Attractor Number", 
                            labels = c("Plural", "Singular")) + 
    scale_x_discrete(limits = exp2.x.order, labels = exp2.x.label) +
    theme_minimal(base_family = "Times") + 
    theme(legend.position = 'bottom') 

exp2.plots$avgResp = exp2.resp.avg.plot



# rt_correct
# 
exp2.rt.avg.plot <- exp2.avgs %>%
    ggplot(aes(resp.experiment, rt_correct.M,
        linetype = rt_correct.attractor_num,
        group = rt_correct.attractor_num
    )) +
    geom_point() +
    geom_line() +
    facet_wrap(~rt_correct.grammatical,
        labeller = labeller(rt_correct.grammatical = exp2.gram.label)
    ) +
    geom_errorbar(aes(
        ymin = rt_correct.M - 1.96 * rt_correct.SE,
        ymax = rt_correct.M + 1.96 * rt_correct.SE
    ),
    width = 0.1
    ) +
    theme(strip.background = element_rect(fill="white")) +
    xlab("Response Bias") + 
    ylab("Response Times")  + 
    scale_linetype_discrete(name = "Attractor Number", 
                            labels = c("Plural", "Singular")) + 
    scale_x_discrete(labels = c("Exp. 1", "Exp. 2", "Lago et al. (2019)")) + 
    theme_minimal(base_family = "Times") + 
    theme(legend.position = 'bottom') 

exp2.plots$avgRT = exp2.rt.avg.plot



####!!!! FILLER GRAPH




# saveRDS(exp2.plots, file = config$exp2$out$plot)

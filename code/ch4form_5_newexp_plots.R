set.seed(01110011)
packages <- c("here", "yaml", "ggplot2", "dplyr", "magrittr", "patchwork", "ggstatsplot")
lapply(packages, require, character.only = TRUE)

theme_set(theme_bw(base_family = "Times"))

# config <- yaml::read_yaml("scripts/config.yml")
# load(config$exp2b$out$dataPrep)
# config <- yaml::read_yaml("scripts/config.yml")

## PLOTS -------
exp2b.plots <- list()

exp2.avgs$resp.att_type = ifelse(exp2.avgs$resp.experiment != "Experiment 2", "gen", "rc")
exp2.avgs$rt.att_type = ifelse(exp2.avgs$resp.experiment != "Experiment 2", "gen", "rc")
exp2.avgs$rt_correct.att_type = ifelse(exp2.avgs$resp.experiment != "Experiment 2", "gen", "rc") 

exp2ab.avgs = bind_rows(exp2b.avgs, exp2.avgs)

exp2ab.avgs %<>% mutate(
    resp.grammatical = case_when(
        resp.grammatical == "gram" ~ "grammatical",
        resp.grammatical == "ungram" ~ "ungrammatical",
        resp.grammatical == "ungrammatical" ~ "ungrammatical",
        resp.grammatical == "grammatical" ~ "grammatical"
    ), 
    resp.attractor_num = case_when(
        resp.attractor_num == "pl" ~ "plural",
        resp.attractor_num == "sg" ~ "singular",
        resp.attractor_num == "plural" ~ "plural",
        resp.attractor_num == "singular" ~ "singular"
    ),
    resp.experiment = case_when(
        resp.experiment == "AgrAttr" ~ "Experiment 2B",
        resp.experiment == "Experiment 2" ~ "Experiment 2A",
        resp.experiment == "Experiment 1" ~ "Experiment 1",
        resp.experiment == "Lago et al. (2019)" ~ "Lago et al. (2019)"
    ),
    resp.att_type = case_when(
        resp.experiment == "Experiment 2A" ~ "exp2a-rc",
        resp.experiment == "Experiment 1" ~ "exp1-gen",
        resp.experiment == "Lago et al. (2019)" ~ "lago-gen",
        resp.experiment == "Experiment 2B" & resp.att_type == "gen" ~ "gen",
        resp.experiment == "Experiment 2B" & resp.att_type == "rc" ~ "rc"
    )
)

exp2b.gram.label <- c(
    gram = "Grammatical\n(Singular Verb)",
    ungram = "Ungrammatical\n(Plural Verb)"
)

# responses 

exp2b.resp.avg.plot <- exp2b.avgs %>%
    ggplot(aes(resp.att_type, resp.M, 
            linetype = resp.attractor_num, 
            group = resp.attractor_num)) + 
    geom_point() + geom_line() + 
    facet_wrap(~resp.grammatical, 
                labeller = labeller(resp.grammatical = exp2b.gram.label),
                scales = "free_y") + 
    geom_errorbar(aes(ymin = resp.M - 1.96*resp.SE, 
                    ymax = resp.M + 1.96*resp.SE), 
                width = 0.1) + 
    theme(strip.background = element_rect(fill="white")) +
    xlab("Attractor Type") + 
    ylab("Percentage 'acceptable'") + 
    scale_y_continuous(labels=scales::percent) + 
    scale_x_discrete(labels = c("Genitive Modifier", "RC Modifier")) +
    scale_linetype_discrete(name = "Attractor Number", 
                            labels = c("Plural", "Singular")) + 
    theme_minimal(base_family = "Times") + 
    theme(legend.position = 'bottom') 

exp2b.plots$avgResp = exp2b.resp.avg.plot



exp2ab.gram.label <- c(
    grammatical = "Grammatical\n(Singular Verb)",
    ungrammatical = "Ungrammatical\n(Plural Verb)"
)
exp2ab.x.order <- c(
    "gen", "rc", "exp2a-rc"
)
exp2ab.resp.avg.plot <- subset(exp2ab.avgs, grepl("Experiment 2", resp.experiment)) %>%
    ggplot(aes(resp.att_type, resp.M, 
            linetype = resp.attractor_num, 
            group = resp.attractor_num)) + 
    geom_point() + geom_line() + 
    facet_wrap(~resp.grammatical, 
                labeller = labeller(resp.grammatical = exp2ab.gram.label),
                scales = "free_y") + 
    geom_errorbar(aes(ymin = resp.M - 1.96*resp.SE, 
                    ymax = resp.M + 1.96*resp.SE), 
                width = 0.1) + 
    theme(strip.background = element_rect(fill="white")) +
    xlab("Attractor Type") + 
    ylab("Percentage 'acceptable'") + 
    scale_y_continuous(labels=scales::percent) + 
    scale_x_discrete(limits = exp2ab.x.order, labels = c("Exp2B\nGen.", "Exp2B\nRC", "Exp2A\n(RC)")) +
    scale_linetype_discrete(name = "Attractor Number", 
                            labels = c("Plural", "Singular")) + 
    theme_minimal(base_family = "Times") + 
    theme(legend.position = 'bottom') 

exp2b.plots$avgRespAB = exp2ab.resp.avg.plot

exp2b1.x.order <- c("exp1-gen", "gen", "rc")

exp2b1.resp.avg.plot <- subset(exp2ab.avgs, resp.experiment == "Experiment 1" | resp.experiment == "Experiment 2B" ) %>%
    ggplot(aes(resp.att_type, resp.M, 
            linetype = resp.attractor_num, 
            group = resp.attractor_num)) + 
    geom_point() + geom_line() + 
    facet_wrap(~resp.grammatical, 
                labeller = labeller(resp.grammatical = exp2ab.gram.label),
                scales = "free_y") + 
    geom_errorbar(aes(ymin = resp.M - 1.96*resp.SE, 
                    ymax = resp.M + 1.96*resp.SE), 
                width = 0.1) + 
    theme(strip.background = element_rect(fill="white")) +
    xlab("Attractor Type") + 
    ylab("Percentage 'acceptable'") + 
    scale_y_continuous(labels=scales::percent) + 
    scale_x_discrete(limits = exp2b1.x.order, labels = c("Exp1\n(Gen.)", "Exp2B\nGen.", "Exp2B\nRC")) +
    scale_linetype_discrete(name = "Attractor Number", 
                            labels = c("Plural", "Singular")) + 
    theme_minimal(base_family = "Times") + 
    theme(legend.position = 'bottom') 

exp2b.plots$avgRespB1 = exp2b1.resp.avg.plot

# rt_correct
# 
exp2b.rt.avg.plot <- exp2b.avgs %>%
    ggplot(aes(resp.att_type, rt_correct.M,
        linetype = rt_correct.attractor_num,
        group = rt_correct.attractor_num
    )) +
    geom_point() +
    geom_line() +
    facet_wrap(~rt_correct.grammatical,
        labeller = labeller(rt_correct.grammatical = exp2b.gram.label)
    ) +
    geom_errorbar(aes(
        ymin = rt_correct.M - 1.96 * rt_correct.SE,
        ymax = rt_correct.M + 1.96 * rt_correct.SE
    ),
    width = 0.1
    ) +
    theme(strip.background = element_rect(fill="white")) +
    xlab("Attractor Type") + 
    ylab("Response Times")  + 
    scale_linetype_discrete(name = "Attractor Number", 
                            labels = c("Plural", "Singular")) + 
    scale_x_discrete(labels = c("Genitive Modifier", "RC Modifier")) +
    theme_minimal(base_family = "Times") + 
    theme(legend.position = 'bottom') 

exp2b.plots$avgRT = exp2b.rt.avg.plot


exp2ab.rt.avg.plot <- subset(exp2ab.avgs, grepl("Experiment 2", resp.experiment)) %>%
    ggplot(aes(resp.att_type, rt_correct.M, 
            linetype = resp.attractor_num, 
            group = resp.attractor_num)) + 
    geom_point() + geom_line() + 
    facet_wrap(~resp.grammatical, 
                labeller = labeller(resp.grammatical = exp2ab.gram.label),
                scales = "free_y") + 
    geom_errorbar(aes(ymin = rt_correct.M - 1.96*rt_correct.SE, 
                    ymax = rt_correct.M + 1.96*rt_correct.SE), 
                width = 0.1) + 
    theme(strip.background = element_rect(fill="white")) +
    xlab("Attractor Type") + 
    ylab("Response Times") + 
    scale_x_discrete(limits = exp2ab.x.order, labels = c("Exp2B\nGen.", "Exp2B\nRC", "Exp2A\n(RC)")) +
    scale_linetype_discrete(name = "Attractor Number", 
                            labels = c("Plural", "Singular")) + 
    theme_minimal(base_family = "Times") + 
    theme(legend.position = 'bottom') 

exp2b.plots$avgRTAB = exp2ab.rt.avg.plot

exp2b1.rt.avg.plot <- subset(exp2ab.avgs, resp.experiment == "Experiment 1" | resp.experiment == "Experiment 2B" ) %>%
    ggplot(aes(resp.att_type, rt_correct.M, 
            linetype = resp.attractor_num, 
            group = resp.attractor_num)) + 
    geom_point() + geom_line() + 
    facet_wrap(~resp.grammatical, 
                labeller = labeller(resp.grammatical = exp2ab.gram.label),
                scales = "free_y") + 
    geom_errorbar(aes(ymin = rt_correct.M - 1.96*rt_correct.SE, 
                    ymax = rt_correct.M + 1.96*rt_correct.SE), 
                width = 0.1) + 
    theme(strip.background = element_rect(fill="white")) +
    xlab("Attractor Type") + 
    ylab("Response Times") + 
    scale_x_discrete(limits = exp2b1.x.order, labels = c("Exp1\n(Gen.)", "Exp2B\nGen.", "Exp2B\nRC")) +
    scale_linetype_discrete(name = "Attractor Number", 
                            labels = c("Plural", "Singular")) + 
    theme_minimal(base_family = "Times") + 
    theme(legend.position = 'bottom') 

exp2b.plots$avgRTB1 = exp2b1.rt.avg.plot

####!!!! FILLER GRAPH




# saveRDS(exp2b.plots, file = config$exp2$out$plot)

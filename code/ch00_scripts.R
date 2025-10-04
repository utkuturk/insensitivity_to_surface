
# TODO: Figure out how to treat missing data in this function
# TODO: Make sure this is strictly a within-participants design. Strange things happen to the means if one tries to process two experiments at once, even if the experiment is used as a grouping factor in the group argument
se_cousineau <- function(df, n_conditions, subject, DV, group, is_proportion = NULL)
{
  stopifnot(!"avgDV" %in% colnames(df))
  subject_var <- substitute(subject) %>% deparse()
  DV <- substitute(DV) %>% deparse()
  
  subj_means <- df %>% group_by(.dots = subject_var) %>% dplyr::summarize(avgDV := mean(!!as.name(DV), na.rm = T))
  GM <- mean(subj_means$avgDV)
  df %<>% group_by(.dots = subject_var) %>% dplyr::mutate(nDV = !!as.name(DV) - mean(!!as.name(DV), na.rm = T) + GM )
  
  if (is.null(is_proportion)) {
    dv <- df[[DV]]
    dv_unique <- unique(dv)
    if ( is.logical(dv) || (length(dv_unique) == 2 && all(dv_unique %in% c(0,1))) ) {
      is_proportion <- TRUE
    } else {
      is_proportion <- FALSE
    }
  }
  
  var_correction_factor <- n_conditions/(n_conditions-1)
  df %>% group_by(.dots = group) %>% 
    dplyr::summarize(M = mean(nDV, na.rm = T),
                     Var = ifelse(is_proportion, M*(1-M), var(nDV, na.rm = T)) * var_correction_factor,
                     #Var = var(nDV, na.rm = T) * var_correction_factor,
                     N = sum(!is.na(nDV)),
                     SE = sqrt(Var/N) )
}


# 
# se_cousineau <- function(df, n_conditions, subject, DV, group, is_proportion = NULL)
# {
#   stopifnot(!"avgDV" %in% colnames(df))
#   subject_var <- substitute(subject) %>% deparse()
#   DV <- substitute(DV) %>% deparse()
#   
#   subj_means <- df %>% group_by(.dots = subject_var) %>% dplyr::summarize(avgDV := mean(!!as.name(DV), na.rm = T))
#   #subj_means$avgDV <- mean(subj_means$avgDV)
#   GM <- mean(subj_means$avgDV)
#   df %<>% left_join( subj_means, by = subject_var )
#   df %<>% group_by(.dots = subject_var) %>% dplyr::mutate(nDV := !!as.name(DV) - avgDV)
#   
#   #df
#   df %>% group_by(.dots = group) %>% dplyr::summarize( M = mean(nDV, na.rm = T),# + GM,
#                                                        N_yes = sum(ResponseYes == T, na.rm = T),
#                                                        N = sum(!is.na(nDV))
#                                                        )
# }
# 
# 


nunique <- function(x) length(unique(x))


read_file <- function(fname) { readChar(fname, file.info(fname)$size) }




lognormalParamMeanSigma <- custom_family(
  "lognormalParamMeanSigma", dpars = c("mu", "sigma"),
  links = c("identity", "log"), lb = c(0, 0),
  type = "real"
)

stan_funs_lognormalParamMeanSigma <- "
real lognormalmean2mu(real mean, real sigma) {
  real mu;
  if (mean < 25) {
    mu = log( mean + (exp(mean)-mean)/(exp(2*mean) + 1) ) - sigma^2/2;
  } else {
    mu = log( mean ) - sigma^2/2;
  }
  return mu;
}
real lognormalParamMeanSigma_lpdf(real y, real mean, real sigma) {
  return lognormal_lpdf(y | lognormalmean2mu(mean, sigma), sigma);
}
real lognormalParamMeanSigma_rng(real mean, real sigma) {
  return lognormal_rng(lognormalmean2mu(mean, sigma), sigma);
}
"
stanvars_lognormalParamMeanSigma <- stanvar(scode = stan_funs_lognormalParamMeanSigma, 
                                            block = "functions")




prob2odds_str <- function(p, round_from = 5) {
  odds <- p/(1-p)
  odds_inv <- odds <= 1
  odds_round <- (odds >= round_from) | (odds <= 1/round_from)
  odds <- ifelse(odds_inv, 1/odds, odds)
  odds <- ifelse(odds_round, round(odds), odds)
  template <- ifelse(odds_inv, 
                     ifelse(odds_round, "1:%0.0f", "1:%0.1f"), 
                     ifelse(odds_round, "%0.0f:1", "%0.1f:1"))
  sapply(seq_along(template), function(i) { sprintf(template[i], odds[i]) })
}


prob_str <- function(p, gtst = 0.001) {
  if (p < .001) {
    str <- "< .001"
  } else if (p > .999) {
    str <- "> .999"
  } else if (p > .99 | p < .01 ) {
    str <- sprintf("  %.3f", p) %>% gsub("0\\.", ".", .)
  } else {
    str <- sprintf("   %.2f", p) %>% gsub("0\\.", ".", .)
  }
  str
}


model_summary <- function(m, include_pp_below_zero = T)
{
  tbl <- fixef(m)[-1,-2] %>% as.data.frame()
  tbl$coef <- rownames(tbl)
  
  if (include_pp_below_zero) {
    cnames <- paste("b", tbl$coef, sep = "_")
    samples <- brms::posterior_samples(m, pars = cnames)
    stopifnot(ncol(samples) == length(cnames))
    
    pref_coef_stats_df <- function(df, name) {
      df %>% as.data.frame(colnames = "x") %T>% 
        { colnames(.) <- name } %T>%
        { .$coef <- rownames(.) %>% gsub("^b_", "", .) }
    }
    
    p_below_zero <- samples %>% sapply(function(x) mean(x < 0)) %>% 
      pref_coef_stats_df("PBelowZero")
    tbl %<>% left_join(p_below_zero, by = "coef")
    
    p_below_zero_str <- samples %>% sapply(function(x) mean(x < 0) %>% prob_str()) %>% 
      pref_coef_stats_df("PBelowZeroStr")
    tbl %<>% left_join(p_below_zero_str, by = "coef")
    
    p_above_zero <- samples %>% sapply(function(x) mean(x > 0)) %>% 
      pref_coef_stats_df("PAboveZero")
    tbl %<>% left_join(p_above_zero, by = "coef")
    
    p_above_zero_str <- samples %>% sapply(function(x) mean(x > 0) %>% prob_str()) %>% 
      pref_coef_stats_df("PAboveZeroStr")
    tbl %<>% left_join(p_above_zero_str, by = "coef")
    
  }
  
  rownames(tbl) <- tbl$coef
  tbl
}


# TODO: In addition to label_max_width, add another argument, strip_label_max_terms,
#       which inserts a line break on a by-term basis
#       Alternatively, write a labeller, which finds the closest interaction symbol next to 
#       the character maximum, and breaks there.
create_model_coefs_plot <- function(m, 
                                    interaction_panels = c(), 
                                    strip_label_max_characters = NULL, 
                                    map_names = NULL,
                                    exclude_names = NULL,
                                    plot_stats = FALSE, 
                                    expand_right = 1, 
                                    expand_top = 1,
                                    x_stat_adjust = 0,
                                    x_breaks = ggplot2::waiver(),
                                    x_minor_breaks = ggplot2::waiver())
{
  interaction_symbol <- " * "
  use_interaction_panels <- length(interaction_panels) > 0
  
  if ( "brmsfit" %in% class(m) ) {
    tbl <- model_summary( m #, include_pp_below_zero = plot_stats 
    )
    
  } else if (is.list(m)) {
    stopifnot( length(names(m)) == length(unique(names(m))) )
    
    tbl <- plyr::ldply(seq_along(m), function(i) { 
      tbl <- model_summary( m[[i]] #, include_pp_below_zero = plot_stats 
      )
      tbl$model <- names(m)[i]
      tbl
    })
    tbl$model %<>% factor( levels = names(m) )
    tbl
    
  } else {
    stop("Unknown model format.")
  }
  tbl %<>% subset(!coef %in% exclude_names)
  
  # rename some rows 
  if (length(map_names) > 0) {
    for (i in seq_along(map_names)) {
      idx <- which(tbl$coef == names(map_names)[i])
      if (length(idx) > 0) {
        if (map_names[i] == "") {
          tbl <- tbl[-idx,]
        } else {
          tbl$coef[idx] <- map_names[i]
        }
      }
    }
  }
  
  if (use_interaction_panels) {
    tbl$interaction <- ""
  }
  for (cur_interaction in interaction_panels) {
    cur_interaction_term1 <- paste0(cur_interaction,":")
    cur_interaction_term2 <- paste0(":",cur_interaction)
    
    is_target_interaction <- grepl(cur_interaction_term1, tbl$coef) | grepl(cur_interaction_term2, tbl$coef)
    
    tbl$coef[is_target_interaction] %<>% gsub(cur_interaction_term1, "", .) %>% 
      gsub(cur_interaction_term2, "", .)
    
    tbl$interaction[is_target_interaction] <- paste0(cur_interaction, interaction_symbol, "...")
  }
  
  # replace interaction symbol if necessary
  if (interaction_symbol != ":") {
    tbl$coef %<>% gsub("([^ ]):([^ ])", paste0("\\1", interaction_symbol, "\\2"), .)
    
    if (use_interaction_panels)
      tbl$interaction %<>% gsub("([^ ]):([^ ])", paste0("\\1", interaction_symbol, "\\2"), .)
  }
  coefs_order <- c(rev(map_names), rev(tbl$coef)) %>% unique() # %>% rev()
  tbl$coef %<>% factor(levels = coefs_order)
  #tbl$coef %<>% factor(levels = tbl$coef %>% unique %>% rev())
  
  # plot
  p <- ggplot(tbl, aes(Estimate, coef)) + geom_point() + 
    geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5), height=0) + 
    geom_vline(xintercept = 0, color = "grey")
  
  if (plot_stats)
  {
    tbl$xmax <- with(tbl, max(c(Estimate, Q2.5, Q97.5))) + x_stat_adjust
    
    
    p <- p + scale_y_discrete(expand = expand_scale(mult = c(.05, .15*expand_top), 
                                                    add = c(0, 0))
    )
    p <- p + scale_x_continuous(expand = expand_scale(mult = c(.05, .15*expand_right), 
                                                      add = c(0, 0)),
                                breaks = x_breaks, 
                                minor_breaks = x_minor_breaks)
    
    p <- p + geom_text(aes(x = tbl$xmax, y = tbl$coef,#_idx, 
                           label = sprintf("[%s]", tbl$PBelowZeroStr)), 
                       family = "mono", hjust = "left")
    suppressWarnings({
      label <- parse(text = "underline(paste('P(', beta, ' < 0)'))")
      p <-  p + geom_text(x = tbl$xmax[1], y = length(unique(tbl$coef))+1, 
                          label = label,
                          family = "mono", hjust = "left")#, fontface = "underlined")
    })
  }
  
  if (use_interaction_panels) {
    p <- p + facet_wrap(~ interaction, strip.position = "left", ncol = 1, scales = "free_y")
    if (!is.null(strip_label_max_characters))
      p <- p + label_wrap_gen(width = strip_label_max_characters)
  }
  
  if ( !is.null(tbl$model) ) {
    p <- p + facet_wrap(~model)
  }
  
  p <- p + theme_bw(base_family = "Times")  + 
    theme(panel.border = element_blank(), 
          axis.ticks.y = element_blank(),
          #strip.text.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.placement = "outside") +
    ylab("")
  
  return (p)
}


print_estimate_with_ci <- function(model, contr_name, fmt = "%0.2f") {
  full_fmt <- sprintf("$\\hat{\\beta}=%s;$ $CI=[%s; %s];$ $P(\\beta<0)%s$", fmt, fmt, fmt, "%s")
  est <- fixef(model, summary = T, robust = F) %>% .[contr_name,]
  post_prob <- model_summary(model) %>% .[contr_name,]
  post_prob %<>% mutate( is_extreme = post_prob[['PBelowZero']] < .001 | post_prob[['PBelowZero']] > .999 )
  post_prob %<>% mutate( PBelowZeroStr = ifelse(is_extreme, as.character(PBelowZeroStr), paste("=", PBelowZeroStr) ) )
  sprintf(full_fmt, est[['Estimate']], est[['Q2.5']], est[['Q97.5']], post_prob[['PBelowZeroStr']])
}



read_experimental_data <- function(fname, subj_offset = 0, item_offset = 0)
{
  data <- read.csv(fname, 
                   header = F, 
                   comment.char = "#", 
                   encoding = "UTF-8" , 
                   col.names = paste0("V",seq_len(11)), 
                   fill = TRUE, 
                   stringsAsFactors = FALSE)
  colnames(data) = c("Time", "MD5", "ControllerType", "SentenceNoInStimFile", "Element", "exp_condition", "item", "Sentence", "Question","Answer", "RT")
  
  subject_id <- with(data, { as.integer(as.factor(paste(Time, MD5))) })
  data$item[data$exp_condition == "intro" | data$exp_condition == "practice"] <- 0
  data$item_num <- as.integer(data$item)
  data$subject <- sprintf("S[%d]", subject_id + subj_offset)
  data$item <- sprintf("I[%d]", data$item_num + item_offset)
  
  df_forms <- data %>% subset(ControllerType != "DashedAcceptabilityJudgment" ) %>% gdata::drop.levels()
  data %<>% subset(ControllerType == "DashedAcceptabilityJudgment")
  
  age <- df_forms %>% dplyr::filter(Sentence == "age") %>% 
    dplyr::select(subject, age = Question)
  natturk <- df_forms %>% dplyr::filter(Sentence == "natturk") %>% 
    dplyr::select(subject, natturk = Question) %T>% 
    { .$natturk %<>% recode(male ="nat_turk", female = "nat_non_turk") } 
  forms <- dplyr::left_join(age, natturk, by = "subject")
  
  stopifnot( nrow(data) %% 2 == 0 )
  rows_stim <- data[c(T,F),]
  rows_resp <- data[c(F,T),]
  stopifnot( all(is.na( rows_stim$RT )) )
  
  data <- rows_resp %>% left_join(forms) %>% 
    dplyr::select(-MD5, -Time, -ControllerType, -Sentence, -Element) %>%
    dplyr::rename(ResponseCorrect=Answer, Response=Question) %>%
    dplyr::select(-ResponseCorrect)
  data %<>% group_by(subject) %>% mutate(trial_no = seq(subject))
  data %<>% mutate( late_response = (Response == "NULL"), Response = ifelse(late_response, NA, as.character(Response)) )
  
  responses <- c(yes="İYİ (P'ye basınız)", no="KÖTÜ (Q'ya basınız)")
  data$Response %<>% as.character() %>% enc2native()
  stopifnot( all(data$Response %in% responses | is.na(data$Response) ) )
  
  data$response_yes <- ifelse(grepl("P'ye",data$Response) , T, 
                              ifelse(grepl("Q'ya",data$Response) , F, NA))
  print( with(data, table(Response, response_yes)) )
  data %<>% dplyr::select(-Response)
  data
}

quantify_subjects <- function(data_to_count) {
  n_subject <- data_to_count$subject %>% unique() %>% length() 
  print(sprintf("Number of subject: %i", n_subject))
  n_subject
}


exclude_bad_subjects <- function(data_to_clean, accuracy_threshold = 0.25, rt_below = 200, rt_upper = 4999) {
  avg_by_subj <- data_to_clean %>%
    group_by(subject, experiment, condition, 
             grammatical, verb_num, attractor_num) %>%
    summarize(avRT = mean(RT), 
              p_yes = mean(response_yes, na.rm = T), 
              N = sum(!is.na(response_yes))  )
  
  avg_by_subj_wide <- avg_by_subj %>% 
    mutate(expcond = paste(experiment, condition, sep="_")) %>% 
    ungroup() %>%
    dplyr::select(-experiment, -condition, -avRT, -N,
                  -grammatical, -verb_num, -attractor_num) %>%
    tidyr::spread(expcond, p_yes) %>% 
    mutate(delta_dc = AgrAttr_d - AgrAttr_c)
  
  bad_subjects <- subset(avg_by_subj_wide, delta_dc <= accuracy_threshold ) %>% .$subject
  data_clean <- data_to_clean %>% subset(!subject %in% bad_subjects)
  
  data_clean %<>% filter(RT < rt_upper & rt_below< RT)
  if("natturk" %in% colnames(data_clean)){
    data_clean %<>% subset(natturk == "nat_turk")
  }
  
  print( with(data_clean, table(exp_condition, response_yes)) )
  print( sprintf("number of bad subjects: %f", length(bad_subjects)))
  data_clean
  
}

exclude_bad_subjects_8 <- function(data_to_clean, accuracy_threshold = 0.25, rt_below = 200, rt_upper = 4999) {
  avg_by_subj <- data_to_clean %>%
    group_by(subject, experiment, condition, 
                grammatical, verb_num, attractor_num, att_type) %>%
    summarize(avRT = mean(RT), 
                p_yes = mean(response_yes, na.rm = T), 
                N = sum(!is.na(response_yes))  )

    avg_by_subj_wide <- avg_by_subj %>% 
    mutate(expcond = paste(experiment, condition, sep="_")) %>% 
    ungroup() %>%
    dplyr::select(-experiment, -condition, -avRT, -N,
                    -grammatical, -verb_num, -attractor_num, -att_type) %>%
    tidyr::spread(expcond, p_yes) %>% 
    mutate(delta_gen_dc = AgrAttr_gen_d - AgrAttr_gen_c, delta_rc_dc = AgrAttr_rc_d - AgrAttr_rc_c)

    bad_subjects_gen <- subset(avg_by_subj_wide, delta_gen_dc <= 0.25 ) %>% .$subject
    bad_subjects_rc <- subset(avg_by_subj_wide, delta_rc_dc <= 0.25 ) %>% .$subject
    data_clean <- data_to_clean %>% subset(!subject %in% bad_subjects_gen | !subject %in% bad_subjects_rc)

    data_clean %<>% filter(RT < rt_upper & rt_below< RT)
    if("natturk" %in% colnames(data_clean)){
    data_clean %<>% subset(natturk == "nat_turk")
    }

    print( with(data_clean, table(exp_condition, response_yes)) )
    print( sprintf("number of bad subjects: %f", length(bad_subjects_gen) + length(bad_subjects_rc)))
    data_clean

  
}

exclude_bad_subjects_8_formal <- function(data_to_clean, accuracy_threshold = 0.25, rt_below = 200, rt_upper = 4999) {
  avg_by_subj <- data_to_clean %>%
    group_by(subject, experiment, condition, 
                grammatical, verb_num, attractor_num, register) %>%
    summarize(avRT = mean(RT), 
                p_yes = mean(response_yes, na.rm = T), 
                N = sum(!is.na(response_yes))  )

    avg_by_subj_wide <- avg_by_subj %>% 
    mutate(expcond = paste(experiment, condition, sep="_")) %>% 
    ungroup() %>%
    dplyr::select(-experiment, -condition, -avRT, -N,
                    -grammatical, -verb_num, -attractor_num, -register) %>%
    tidyr::spread(expcond, p_yes) %>% 
    mutate(delta_formal_dc = AgrAttr_formal_d - AgrAttr_formal_c, delta_informal_dc = AgrAttr_informal_d - AgrAttr_informal_c)

    bad_subjects_formal <- subset(avg_by_subj_wide, delta_formal_dc <= 0.25 ) %>% .$subject
    bad_subjects_informal <- subset(avg_by_subj_wide, delta_informal_dc <= 0.25 ) %>% .$subject
    data_clean <- data_to_clean %>% subset(!subject %in% bad_subjects_formal | !subject %in% bad_subjects_informal)

    data_clean %<>% filter(RT < rt_upper & rt_below< RT)
    if("natturk" %in% colnames(data_clean)){
    data_clean %<>% subset(natturk == "nat_turk")
    }

    print( with(data_clean, table(condition, response_yes)) )
    print( sprintf("number of bad subjects: %f", length(bad_subjects_formal) + length(bad_subjects_informal)))
    data_clean

  
}

quantify_preprocessing <- function(old_data, clean_data, k_round=2) {
  perc <- round(100*((nrow(old_data)-nrow(clean_data))  / nrow(old_data)),k_round)
  print(sprintf("%g%% of the experimental data is deleted", perc))
  perc
}

no_null_no_practice <- function(data_to_clean) {
  data_to_clean %<>% subset(exp_condition != "practice") %>% subset(!is.na(response_yes))
}

asi <- function(x) {as.integer(x)}
asf <- function(x) {as.factor(x)}
asc <- function(x) {as.character(x)}

get_averages <- function(data_to_get_avg, grouping= c("experiment", "grammatical", "attractor_num")) {
  avg_clean <- list()
  avg_clean$resp <- data_to_get_avg %>% 
    plyr::ddply(c("experiment"), function(df) {
      df %>% se_cousineau(n_conditions = 4, subject, DV = response_yes, 
                          group = grouping, 
                          is_proportion = TRUE)
    })
  
  avg_clean$rt <- data_to_get_avg %>%
    plyr::ddply(c("experiment"), function(df) {
      df %>% se_cousineau(n_conditions = 4, subject, DV = RT, 
                          group = grouping, 
                          is_proportion = FALSE)
    })
  
  avg_clean$rt_correct <- data_to_get_avg %>% subset(ResponseCorrect) %>%
    plyr::ddply(c("experiment"), function(df) {
      df %>% se_cousineau(n_conditions = 4, subject, DV = RT, 
                          group = grouping, 
                          is_proportion = FALSE)
    })
  
  avg_clean
}

check_model_coefs <- function(modelname) {
  mcmc_areas(modelname, 
             pars=vars(starts_with("b_")), 
             prob_outer = 0.95,
             prob = 0.8, 
             point_est = "median")
}

hammerly_bias_calc <- function(data) {
  data %>%
    group_by(subj, Grammaticality, exp) %>%
    mutate(correct = sum(Accuracy) +.5 ) %>% 
    mutate(numItem = n_distinct(item)+1) %>%  
    mutate(mean = correct/numItem) %>% 
    group_by(subj, Grammaticality, exp) %>%
    summarise(average = mean(mean)) %>%
    spread(Grammaticality, average) %>%
    mutate(FA = 1 - Ungrammatical, hit = Grammatical) %>%  
    group_by(subj, exp) %>%
    summarise(bias = -.5*(qnorm(hit)+qnorm(FA)))
}


extract_subtitle <- function(biasavgs) {
  ggstatsplot::ggbetweenstats(
    data = biasavgs,
    x = exp,
    y = bias,
    type = "bayes",
    plot.type = "box",
    bf.prior =  "ultrawide",
  ) %>% ggstatsplot::extract_stats() %>% .$subtitle_data %>% .$expression %>% .[[1]]
}



hammerly_bias_plot <- function(biasavgs) {
  ggstatsplot::ggbetweenstats(
    data = biasavgs,
    x = exp,
    y = bias,
    xlab = "",
    ylab = "Estimated Bias",
    type = "bayes",
    plot.type = "box",
    bf.prior =  "ultrawide",
    results.subtitle = F,
    centrality.plotting = FALSE,
    package = "wesanderson",
    palette = "GrandBudapest1"
  ) +
    scale_x_discrete(
      labels = c("1" = "Experiment 1", "3" = "Experiment 3")
    ) +
    scale_y_continuous(breaks=seq(-1,1,.5), limits = c(-1,1)) + 
    theme(legend.position = 'none') 
}


load_config <- function() {yaml::read_yaml("config.yml")}

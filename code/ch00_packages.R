# SETTINGS ====
setwd(here::here())

# settings
set.seed(01110011)

packages <- c(
    "papaja", "knitcitations", "dplyr", "magrittr", 
    "ggplot2", "brms", "rstan", "patchwork", "bayesplot",
    "tidyr", "purrr", "ggdist", "data.table", "tidybayes",
    "gdata", "cowplot", "gganimate", "ggstatsplot"
)

lapply(packages, require, character.only = TRUE)

theme_set(theme_tidybayes() + panel_border())
theme_set(theme_bw(base_family = "Times"))
# To solve some conflicts between  packages
select <- dplyr::select
extract <- rstan::extract
theme_set(theme_bw())
color_scheme_set("red")
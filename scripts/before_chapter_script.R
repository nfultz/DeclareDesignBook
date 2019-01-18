
source("scripts/package_list.R")

lapply(bookwide_packages, function(x)
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  })

set.seed(42)
# rerun_templates <- FALSE
# do_diagnosis <- FALSE
sims <- 100
b_sims <- 20

pro_con_colors <- c("#C67800", "#205C8A")

source("scripts/ggplot_dd_theme.R")

theme_set(dd_theme())

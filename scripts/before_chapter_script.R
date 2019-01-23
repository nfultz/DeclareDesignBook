
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

load_book_file <- function(obj_name){
  obj_name <- paste0(obj_name, ".RDS")
  path <- file.path("rfiles", gsub(".Rmd", "", knitr::current_input()))
  obj_file <- read_rds(paste(path, obj_name, sep = "/"))
  assign(x = obj_name, value = obj_file)
}

save_book_file <- function(obj){
  obj_name <- paste0(substitute(obj), ".RDS")
  path <- file.path("rfiles", gsub(".Rmd", "", knitr::current_input()))
  dir.create(path, showWarnings = FALSE)
  write_rds(obj, path = paste(path, obj_name, sep = "/"))
}
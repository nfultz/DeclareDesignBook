source("scripts/package_list.R")

# detach packages

packages <- sapply(sessionInfo()$otherPkgs, function(x) x$Package)
packages <- packages[!packages %in% bookwide_packages]
packages <- sapply(packages, function(p) paste0("package:", p))
lapply(packages, detach, character.only = TRUE, unload = TRUE)
# clear environment
rm(list = ls())
gc()
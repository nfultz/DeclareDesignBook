library(checkpoint)

found_packages <- scanForPackages(".", use.knitr = TRUE)$pkgs
if (length(found_packages[!found_packages %in% installed.packages()]) > 0) {
  install.packages(found_packages[!found_packages %in% installed.packages()])
}


# devtools::install_github("ebenmichael/augsynth")

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

# Model -------------------------------------------------------------------
population <- declare_population(
  N = 500,
  type = sample(c("A", "B", "C", "D"), size = N, 
                replace = TRUE, prob = c(.40, .05, .10, .45)))

potential_outcomes <- declare_potential_outcomes(
  R_Z_0 = type %in% c("A", "C"),
  R_Z_1 = type %in% c("A", "B"),
  Y_Z_0 = ifelse(R_Z_0, rnorm(n = sum(R_Z_0), mean = .1*(type == "A") - 2*(type == "C")), NA),
  Y_Z_1 = ifelse(R_Z_1, rnorm(n = sum(R_Z_1), mean = .2*(type == "A") + 2*(type == "B")), NA)
)

# Inquiry -----------------------------------------------------------------
estimand_1 <- declare_estimand(ATE_R = mean(R_Z_1 - R_Z_0))
estimand_2 <- declare_estimand(ATE_Y = mean(Y_Z_1 - Y_Z_0))
estimand_3 <- declare_estimand(
  ATE_Y_for_As = mean(Y_Z_1[type == "A"] - Y_Z_0[type == "A"]))

# Data Strategy -----------------------------------------------------------
assignment <- declare_assignment(m = 250)

# Answer Strategy ---------------------------------------------------------
estimator_1 <- declare_estimator(R ~ Z, estimand = estimand_1, label = "ATE_R")
estimator_2 <- declare_estimator(Y ~ Z, estimand = estimand_2, label = "ATE_Y")
estimator_3 <- declare_estimator(Y ~ Z, estimand = estimand_3, label = "ATE_YA")

# Design ------------------------------------------------------------------
design <- 
  population + 
  potential_outcomes + 
  assignment + 
  estimand_1 + estimand_2 + estimand_3 + 
  declare_reveal(outcome_variables = c("R", "Y")) + 
  estimator_1 + estimator_2 + estimator_3





knitr::kable(reshape_diagnosis(diagnosis))

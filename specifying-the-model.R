# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

population <- declare_population(N = 8, e = runif(N), X = rnorm(N, e, 1)) 

population()

potential_outcomes <- declare_potential_outcomes(Y_Z_0 = .5 < e,
                                                 Y_Z_1 = .5 < e + .05)

potential_outcomes <- declare_potential_outcomes(Y ~ .5 < e + .05 * Z)

potential_outcomes <- declare_potential_outcomes(
  income ~ employed + education + u, 
  assignment_variables = list(employed = c("No","Yes"), education = c(10,12)))

potential_outcomes <- declare_potential_outcomes(
  Y_X_0_Z_0 = .5 < e,
  Y_X_0_Z_1 = .5 < e + .05,
  Y_X_1_Z_0 = .5 < e,
  Y_X_1_Z_1 = .5 < e + .05 + .05)

## declare_potential_outcomes(Y ~ .5 < e + Z * .05 + Z * X * .05,
##   assignment_variables = list(Z = 0:1, X = 0:1))

potential_outcomes <- declare_potential_outcomes(
  X_Z_0 = .5 < e * 0.75,
  X_Z_1 = .5 < e * 1.25,
  Y_X_0 = .5 < e,
  Y_X_1 = .5 < e + .05)

## hawthorne_POs <- declare_potential_outcomes(
##   Y_Z_1_M_0 = .5 < e,
##   Y_Z_2_M_0 = .5 < e + .05,
##   Y_Z_3_M_0 = .5 < e + .05,
##   Y_Z_1_M_1 = .5 < e + .05,
##   Y_Z_2_M_1 = .5 < e + .05 + .05,
##   Y_Z_3_M_1 = .5 < e + .05 + .05)

## assignment <- declare_assignment(conditions = c(1,2,3))
## estimand_no_m <- declare_estimand(ate_2_no_m = mean(Y_Z_2_M_0 - Y_Z_1_M_0))
## measurement <- declare_step(M = 1, handler = fabricate)
## reveal_outcomes_measurement <- declare_reveal(Y, c(Z, M))
## hawthorne_design <- population + hawthorne_POs + estimand_no_m +
##   sampling + assignment + measurement + reveal_outcomes_measurement + estimator

## experimenter_demand_POs <- declare_potential_outcomes(
##   Y_Z_1_M_0 = .5 < e,
##   Y_Z_2_M_0 = .5 < e + .05,
##   Y_Z_3_M_0 = .5 < e + .05,
##   Y_Z_1_M_1 = .5 < e,
##   Y_Z_2_M_1 = .5 < e + .05 + .05,
##   Y_Z_3_M_1 = .5 < e + .05 + .10)
## demand_design <- replace_step(design = hawthorne_design,
##                               step = "hawthorne_POs",
##                               new_step = experimenter_demand_POs)

interference_design  <- 
  declare_population(N = 3, e = runif(N)) +
  declare_potential_outcomes(
    Y_J_1 = c(.5 < e[1] + 1, .5 < e[2] - 1, .5 < e[3]),
    Y_J_2 = c(.5 < e[1] - 1, .5 < e[2] + 1, .5 < e[3]),
    Y_J_3 = c(.5 < e[1], .5 < e[2], .5 < e[3] + 1)) +
  declare_assignment(conditions = c("1","2","3"), assignment_variable = "J") +
  declare_step(Z = as.numeric(ID == J), handler = fabricate) +
  declare_reveal(Y, J) +
  declare_estimator(Y ~ Z, model = lm_robust)

## spillover_POs <- declare_potential_outcomes(
##   Y_Z_0_S_0 = .5 < e,
##   Y_Z_1_S_0 = .5 < e + .05,
##   Y_Z_0_S_1 = .5 < e - .05 / 2,
##   Y_Z_1_S_1 = .5 < e + .05 / 2)
## neighbors <- declare_step(next_neighbor = c(N,(1:(N-1))),
##                           S = Z[next_neighbor],
##                           handler = fabricate)
## reveal_spillovers <- declare_reveal(Y, c(Z, S))
## spillover_design <- population + spillover_POs +
##   sampling + assignment + neighbors + reveal_spillovers + estimator

## compliance_POs <- declare_potential_outcomes(
##   D_Z_0 = 0,
##   D_Z_1 = ifelse(order(e) > 4, 1, 0),
##   Y_D_0 = .5 < e,
##   Y_D_1 = .5 < e + .05)
## ate_estimand <- declare_estimand(ate = mean(Y_D_1 - Y_D_0))
## cace_estimand <- declare_estimand(cace = mean(Y_D_1 - Y_D_0),
##                                   subset = D_Z_0 == 0 & D_Z_1 == 1)

## attrition_POs <- declare_potential_outcomes(
##   R_Z_0 = ifelse(order(e) <= 4, 1, 0),
##   R_Z_1 = 1,
##   Y_R_0_Z_0 = NA,
##   Y_R_0_Z_1 = NA,
##   Y_R_1_Z_0 = .5 < e,
##   Y_R_1_Z_1 = .5 < e + .05)

population <- declare_population(N = 8, e = rnorm(N, 0, 1))
model_1 <- population + declare_potential_outcomes(Y ~ e)
model_2 <- population + declare_potential_outcomes(Y ~ e + e * 2 * Z)
model_3 <- population + declare_potential_outcomes(Y ~ ifelse(e > .5, Z * .2, -Z * .2))

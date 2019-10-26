# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

tau <- .10
N <- 8
N_sampled <- 4
population <- declare_population(N = N, e = runif(N)) 
potential_outcomes <- declare_potential_outcomes(
  Y_Z_0 = .5 < e, Y_Z_1 = .5 < e + tau)
estimand <- declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0))
sampling <- declare_sampling(n = N_sampled)
assignment <- declare_assignment(prob = .5)
reveal_outcomes <- declare_reveal(Y, Z)
estimator <- declare_estimator(Y ~ Z, label = "DiM", estimand = "PATE")
simple_design <- population + potential_outcomes + estimand + 
  sampling + assignment + reveal_outcomes + estimator
simple_design_data <- draw_data(simple_design)

estimates_df <- difference_in_means(Y ~ Z, data = simple_design_data)
kable(tidy(estimates_df))

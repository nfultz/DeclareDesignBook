# ---
# Difference-in-differences
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = T)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

N_units <- 2
N_time_periods <- 2

two_period_design <- 
  
  declare_population(
    units = add_level(N = N_units, unit_shock = rnorm(N)),
    periods = add_level(N = N_time_periods, nest = FALSE,
                        time = (1:N_time_periods) - N_time_periods + 1),
    unit_period = cross_levels(by = join(units, periods), unit_time_shock = rnorm(N))
  ) + 
  
  # internal note: the unbiasedness obtains whether or not there is a unit-time shock
  declare_potential_outcomes(
    Y_Z_0 = unit_shock + 0.5 * time + unit_time_shock, # common pretreatment trend
    Y_Z_1 = Y_Z_0 + 0.2) +
  
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0), subset = time == 1) + 
  
  declare_assignment(Z = unit_shock == max(unit_shock), handler = mutate) + 
  
  declare_reveal(
    Y = case_when(Z == 0 | time < 1 ~ Y_Z_0, TRUE ~ Y_Z_1), handler = mutate) +
  
  declare_estimator(estimate = (mean(Y[Z == 1 & time == 1]) - mean(Y[Z == 0 & time == 1])) - 
                      (mean(Y[Z == 1 & time == 0]) - mean(Y[Z == 0 & time == 0])), 
                    estimator_label = "DiD", handler = summarize, label = "DiD") + 
  
  declare_estimator(estimate = mean(Y[Z == 1 & time == 1]) - mean(Y[Z == 1 & time == 0]), 
                    estimator_label = "Diff", handler = summarize, label = "Diff") + 
  
  declare_estimator(estimate = mean(Y[Z == 1 & time == 1]) - mean(Y[Z == 0 & time == 1]), 
                    estimator_label = "DiM", handler = summarize, label = "DiM")





kable(get_diagnosands(diagnosis_two_period))

set.seed(2)
draw_data(two_period_design) %>% 
  ggplot(aes(time, Y, color = as.factor(Z))) + 
  geom_line()

N_units <- 20
N_time_periods <- 20

multi_period_design <- 
  
  declare_population(
    units = add_level(N = N_units, 
                      unit_shock = rnorm(N), 
                      unit_treated = 1*(unit_shock > median(unit_shock)), 
                      unit_treatment_start = 
                        sample(2:(N_time_periods - 1) - N_time_periods + 1, N, replace = TRUE)),
    periods = add_level(N = N_time_periods, nest = FALSE, 
                        time = (1:N_time_periods) - N_time_periods + 1),
    unit_period = cross_levels(by = join(units, periods),
                               noise = rnorm(N), 
                               pretreatment = 1*(time < unit_treatment_start))
  ) + 
  
  declare_potential_outcomes(
    Y_Z_0 = unit_shock + 0.5 * time + noise, # common pretreatment trend
    Y_Z_1 = Y_Z_0 + 0.2) +
  
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0), subset = time == 1) + 
  
  declare_assignment(Z = 1*(unit_treated & pretreatment == FALSE), handler = fabricate) + 
  declare_reveal(Y, Z) + 
  
  declare_estimator(Y ~ Z + time, fixed_effects = ~ units + periods, 
                    model = lm_robust, label = "twoway-fe", estimand = "ATE") 
  





kable(get_diagnosands(diagnosis_multi_period))

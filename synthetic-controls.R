# ---
# Synthetic controls
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = T)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R
library(Synth)
library(augsynth) # remotes::install_github("ebenmichael/augsynth")
library(glmnet)
library(sf)

# tidy function that takes data and just adds the synthetic control weights to it
synth_weights_tidy <- function(data) {
  dataprep.out <- dataprep(
    foo = data,
    predictors = "prop_non_hispanic_below_hs",
    predictors.op = "mean",
    time.predictors.prior = 1998:2006,
    dependent = "prop_non_hispanic_below_hs",
    unit.variable = "state_number",
    time.variable = "year",
    treatment.identifier = 4,
    controls.identifier = c(1:3, 5:50), # states without Arizona
    time.optimize.ssr = 1998:2006,
    time.plot = 1998:2009)
  capture.output(fit <- synth(data.prep.obj = dataprep.out))
  tab <- synth.tab(dataprep.res = dataprep.out, synth.res = fit) 
  
  data %>% 
    left_join(tab$tab.w %>% mutate(synth_weights = w.weights) %>% dplyr::select(synth_weights, unit.numbers), by = c("state_number" = "unit.numbers")) %>% 
    mutate(synth_weights = replace(synth_weights, state_number == 4, 1))
}

augsynth_tidy <- function(data) {
  fit <- augsynth(prop_non_hispanic_below_hs ~ legal_worker_act, state, year, t_int = 2007, data = data)
  res <- summary(fit)$att %>% filter(Time == 2007) %>% select(Estimate, Std.Error)
  names(res) <- c("estimate", "std.error")
  res$p.value <- 2 * pt(-abs(res$estimate/res$std.error), df = nrow(data) - 15)
  res$conf.low <- res$estimate - 1.96 * res$std.error
  res$conf.high <- res$estimate + 1.96 * res$std.error
  res
}

# note need to clean up the range of the data, currently over 1
design <-
  declare_population(
    states = add_level(
      N = 50, 
      state = state.abb,
      state_number = as.numeric(as.factor(state)),
      state_shock = runif(N, -.15, .15),
      border_state = state %in% c("AZ", "CA", "NM", "TX"),
      state_shock = ifelse(border_state, .2, state_shock)
    ),
    years = add_level(
      N = 12, nest = FALSE,
      year = 1998:2009,
      post_treatment_period = year >= 2007,
      year_shock = runif(N, -.025, .025), 
      year_trend = year - 1998
    ),
    obs = cross_levels(
      by = join(states, years),
      # treatment indicator:
      legal_worker_act = if_else(post_treatment_period == TRUE & state == "AZ", 1, 0),
      state_year_shock = runif(N, -.025, .025),
      prop_non_hispanic_below_hs_baseline = 
        0.4 + state_shock + year_shock + (.01 + .05 * border_state) * year_trend + state_year_shock
    )
  ) +
  declare_potential_outcomes(
    prop_non_hispanic_below_hs ~ prop_non_hispanic_below_hs_baseline + 0.25 * legal_worker_act, 
    assignment_variable = legal_worker_act) +
  declare_estimand(
    ATE_AZ = mean(prop_non_hispanic_below_hs_legal_worker_act_1 - 
                    prop_non_hispanic_below_hs_legal_worker_act_0), 
    subset = legal_worker_act == TRUE) +
  declare_reveal(prop_non_hispanic_below_hs, legal_worker_act) +
  declare_step(handler = synth_weights_tidy) +
  declare_estimator(
    prop_non_hispanic_below_hs ~ legal_worker_act, 
    subset = year >= 2007, weights = synth_weights, model = lm_robust, label = "synth") +
  declare_estimator(
    prop_non_hispanic_below_hs ~ legal_worker_act, subset = year >= 2007, 
    model = lm_robust, label = "unweighted") +
  declare_estimator(
    prop_non_hispanic_below_hs ~ I(state == "AZ") + post_treatment_period + legal_worker_act, term = "legal_worker_act", 
    model = lm_robust, label = "unweighted_did") +
  declare_estimator(handler = tidy_estimator(augsynth_tidy), label = "augsynth")

state_data <- draw_data(design)

state_data %>% dplyr::select(state, synth_weights) %>% distinct %>% arrange(-synth_weights) %>% head

state_data %>% 
  ggplot() +
  geom_line(aes(year, prop_non_hispanic_below_hs)) +
  facet_wrap(~ state)

state_data %>% 
  mutate(treatment_state = factor(state == "AZ", levels = c(FALSE, TRUE), labels = c("Synthethic Control", "Arizona"))) %>% 
  group_by(treatment_state, year) %>% 
  summarize(prop_non_hispanic_below_hs = weighted.mean(prop_non_hispanic_below_hs, w = synth_weights)) %>% 
  ggplot(aes(x = year, y = prop_non_hispanic_below_hs, color = treatment_state)) +
  geom_line() + 
  geom_vline(xintercept = 2007) + 
  scale_x_continuous(breaks = scales::pretty_breaks()) + 
  annotate("text", x = 2006.7, y = 1.7, label = "Law Introduced in 2007", hjust = "right", family = "Palatino") +
  labs(color = "") +
  xlab("") + ylab("Proportion Non-Hispanic Below H.S. Education") +
  dd_theme()





synth_diagnosands <- declare_diagnosands(select = c("bias", "rmse", "coverage"))

diagnosis <- diagnose_design(simulations, diagnosands = synth_diagnosands, bootstrap_sims = b_sims)

kable(reshape_diagnosis(diagnosis))

# declaration outside the convex hull
design_outside_hull <- replace_step(
  design, 
  step = 2, 
  new_step = declare_potential_outcomes(
    prop_non_hispanic_below_hs ~ prop_non_hispanic_below_hs_baseline + 0.25 * legal_worker_act + 0.2 * (state == "AZ"), 
    assignment_variable = legal_worker_act))

state_data_outside_hull <- draw_data(design_outside_hull)





diagnosis_outside_hull <- diagnose_design(simulations_outside_hull, diagnosands = synth_diagnosands, bootstrap_sims = b_sims)

kable(reshape_diagnosis(diagnosis_outside_hull))

# plot the synthetic control constructed in this way (it usually picks just texas and is highly biased)
state_data_outside_hull %>% 
  mutate(treatment_state = factor(state == "AZ", levels = c(FALSE, TRUE), labels = c("Synthethic Control", "Arizona"))) %>% 
  group_by(treatment_state, year) %>% 
  summarize(prop_non_hispanic_below_hs = weighted.mean(prop_non_hispanic_below_hs, w = synth_weights)) %>% 
  ggplot(aes(x = year, y = prop_non_hispanic_below_hs, color = treatment_state)) +
  geom_line() + 
  geom_vline(xintercept = 2007) + 
  scale_x_continuous(breaks = scales::pretty_breaks()) + 
  annotate("text", x = 2006.7, y = 1.7, label = "Law Introduced in 2007", hjust = "right", family = "Palatino") +
  labs(color = "") +
  xlab("") + ylab("Proportion Non-Hispanic Below H.S. Education") +
  dd_theme()

## calculate_in_hull <- function(data) {
##   # NB: cannot get this function to work (GB)
##   # control_data <- data %>%
##   #   filter(post_treatment_period == FALSE & state != "AZ") %>%
##   #   transmute(year, prop_non_hispanic_below_hs, ID = 1:n())
##   #
##   # chull <- tibble(ID = with(control_data, chull(year, prop_non_hispanic_below_hs))) %>%
##   #   left_join(control_data)
##   #
##   # chull <- bind_rows(chull, chull[1, ])
##   #
##   # chull_sf <- st_sf(geom = st_sfc(st_polygon(x = list(chull %>% select(year, prop_non_hispanic_below_hs) %>% as.matrix))), crs = st_crs(27700))
##   #
##   # treat_points <- data %>%
##   #   filter(post_treatment_period == FALSE & state == "AZ") %>%
##   #   select(year, prop_non_hispanic_below_hs) %>%
##   #   # bind_rows(tibble(year = 100000, prop_non_hispanic_below_hs = 99)) %>%
##   #   st_as_sf(coords = c("year", "prop_non_hispanic_below_hs"), crs = st_crs(27700))
##   #
##   # tt <- st_join(treat_points, chull_sf, join = st_intersects)
##   #
##   #   st_join(chull_sf, join = st_within)
##   #
##   # ggplot() + geom_sf(data = chull_sf) + geom_sf(data = treat_points)
##   #
##   # data %>%
##   #   filter(post_treatment_period == FALSE) %>%
##   #   mutate(treatment_state = factor(state == "AZ", levels = c(FALSE, TRUE), labels = c("Synthetic Control", "Arizona"))) %>%
##   #   select(year, treatment_state, prop_non_hispanic_below_hs) %>%
##   #   with(., chull(x = year, y = prop_non_hispanic_below_hs)) %>%
##   #   tibble(points_in_hull)
##   #
## }

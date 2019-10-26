library(metafor)

meta_re_estimator <- function(data){
  site_estimates_df <- data %>% 
    group_by(site) %>% 
    do(tidy(lm_robust(Y ~ Z, data = .))) %>% 
    filter(term == "Z") %>% 
    ungroup 
  
  meta_fit <- rma(estimate, std.error, data = site_estimates_df, method = "REML")
  
  with(meta_fit, tibble(
    estimate = as.vector(beta), std.error = se, p.value = pval, conf.low = ci.lb, conf.high = ci.ub))
}

post_strat_estimator <- function(data) {
  if(length(unique(data$site)) > 1) {
    fit <- lm_robust(Y ~ Z*as.factor(subject_type) + as.factor(site), data = data)
    tidy(fit)
  } else {
    fit <- lm_robust(Y ~ Z*as.factor(subject_type), data = data)
  }
  
  alpha <- .05
  
  lh_fit <- try({ car::linearHypothesis(
    fit, 
    hypothesis.matrix = paste(paste(paste(pr_type[91:100][-1], "*", car::matchCoefs(fit, "Z"), sep = ""), collapse = " + "), " = 0"), 
    level = 1 - alpha) })
  
  if(!inherits(lh_fit, "try-error")) {
    tibble(estimate = drop(attr(lh_fit, "value")), 
           std.error = sqrt(diag(attr(lh_fit, "vcov"))),
           df = fit$df.residual, 
           statistic = estimate / std.error, 
           p.value = 2 * pt(abs(statistic), df, lower.tail = FALSE),
           conf.low = estimate + std.error * qt(alpha / 2, df),
           conf.high = estimate + std.error * qt(1 - alpha / 2, df))
  } else {
    tibble(estimate = NA, error = TRUE)
  }
}

pr_type <- c( # rows are sites, columns are types
  0.005, 0.005, 0.09, 0.15, 0.25, 0.1, 0, 0.1, 0.15, 0.15,
  0.1, 0.15, 0.15, 0.15, 0.25, 0.005, 0, 0.1, 0.09, 0.005,
  0.15, 0.15, 0.15, 0.005, 0.005, 0, 0.25, 0.09, 0.1, 0.1,
  0, 0.15, 0.005, 0.09, 0.005, 0.15, 0.25, 0.1, 0.1, 0.15,
  0.005, 0.1, 0.09, 0.25, 0.15, 0.15, 0.005, 0, 0.1, 0.15,
  0.005, 0.15, 0.25, 0.1, 0, 0.1, 0.005, 0.15, 0.09, 0.15,
  0.15, 0.15, 0.005, 0.25, 0.1, 0.15, 0.09, 0.005, 0.1, 0,
  0.25, 0.1, 0.15, 0, 0.005, 0.15, 0.15, 0.1, 0.005, 0.09,
  0.005, 0.1, 0.1, 0.15, 0, 0.25, 0.15, 0.09, 0.005, 0.15,
  0.005, 0.09, 0.15, 0.1, 0, 0.1, 0.15, 0.005, 0.25, 0.15)

# need to have biased sampling to get bias here
# two kinds of populations, one in which the study type determines the subject types and you select on study type
#   a second kind where study type determines study shock 
#   in second type if you adjust for subject type then you will be able to unbiased recover global

multi_site_designer <- function(
  N_sites = 10,
  n_study_sites = 5,
  n_subjects_per_site = 1000,
  subject_type_effects = seq(from = -0.1, to = 0.1, length.out = 10)
) {
  declare_population(
    site = add_level(N = N_sites, context_effect = rnorm(N, sd = 0.05)),
    subject_types = add_level(
      N = 10,
      subject_type = 1:10,
      subject_type_effect = subject_type_effects,
      type_proportion = pr_type,
      N_subjects = ceiling(2500 * type_proportion)
    ),
    subjects = add_level(N = N_subjects, noise = rnorm(N))
  ) + 
    declare_potential_outcomes(Y ~ Z * (0.1 + subject_type_effect) + noise) +
    declare_estimand(ATE_OOS = mean(Y_Z_1 - Y_Z_0), subset = site == "10") + # estimator for out-of-sample site
    declare_step(filter, site != "10") + # subset to the in-sample sites
    declare_sampling(clusters = site, n = n_study_sites) + 
    declare_sampling(strata = site, n = n_subjects_per_site) + 
    declare_assignment(blocks = site, prob = 0.5) + 
    declare_estimand(study_site_ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_estimator(handler = tidy_estimator(post_strat_estimator), label = "post-strat")
}

# try to predict out-of-sample context effect. idea is to show that from one context we don't do too well not because of context effects but because we don't have enough data on different kinds of het groups (because which types exist vary across contexts)

single_site_large_design <- multi_site_designer(n_study_sites = 1, n_subjects_per_site = 2500)

small_study_five_sites <- multi_site_designer(n_study_sites = 5, n_subjects_per_site = 500)

# now modify to *not* have het fx
single_site_large_design_no_het <- multi_site_designer(n_study_sites = 1, n_subjects_per_site = 2500, subject_type_effects = 0)
  
small_study_five_sites_no_het <- multi_site_designer(n_study_sites = 5, n_subjects_per_site = 500, subject_type_effects = 0)
  
# now modify to have context fx

single_site_large_design_context <- replace_step(
  single_site_large_design,
  step = 2, new_step = declare_potential_outcomes(Y ~ Z * (0.1 + subject_type_effect + context_effect) + noise))

small_study_five_sites_context <- replace_step(
  small_study_five_sites, 
  step = 2, new_step = declare_potential_outcomes(Y ~ Z * (0.1 + subject_type_effect + context_effect) + noise))

# sims <- simulate_design(single_site_large_design, small_study_five_sites, 
#                         single_site_large_design_no_het, small_study_five_sites_no_het, 
#                         single_site_large_design_context, small_study_five_sites_context,
#                         sims = 1000)
# diag <- diagnose_design(sims %>% filter(!is.na(estimate) & !is.na(std.error) & !is.na(statistic) & !is.na(p.value) & !is.na(conf.low) & !is.na(conf.high)), bootstrap_sims = 1000)

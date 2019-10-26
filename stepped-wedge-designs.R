p_00 <- p_W1 <- p_W2 <- p_W3 <- 1/4

design <- 
  declare_population(
    t = add_level(N = 3, u_t = rnorm(N), trend = as.numeric(t), 
                  p = c(p_W1, p_W1 + p_W2, p_W1 + p_W2 + p_W3)),
    i = add_level(N = 8, u_i = rnorm(N), nest = FALSE),
    obs = cross_levels(by = join(t, i), u_ti = rnorm(N))) + 
  declare_potential_outcomes(
    Y_Z_0 = u_i + u_t + u_ti,
    Y_Z_1 = u_i + u_t + u_ti + trend) + 
  declare_assignment(clusters = i, 
                     conditions = 1:4, 
                     prob_each = c(p_W1, p_W2, p_W3, p_00),
                     assignment_variable = "wave") + 
  declare_step(Z = as.numeric(t >= wave), 
               ip = 1 / (Z * p + (1 - Z) * (1 - p)),
               handler = fabricate) + 
  declare_reveal(Y, Z) + 
  declare_estimand(ate = mean(Y_Z_1 - Y_Z_0)) + 
  declare_estimator(Y ~ Z, model = lm_robust, label = "Unweighted SW") +
  declare_estimator(Y ~ Z, model = lm_robust, label = "Weighted SW", weights = ip) 

draw_data(design) %>% 
  mutate(i = fct_reorder(i, wave), Assignment = ifelse(Z == 1, "Treatment", "Control")) %>% 
  ggplot(aes(x = t, y = i, fill = Assignment)) +
  geom_tile(color = "white") + scale_fill_grey(start = .9,end = .5) +
  geom_text(aes(label = round(ip,1))) +
  dd_theme()





reshape_diagnosis(diagnoses) %>% kable()





reshape_diagnosis(sw_comparisons) %>% kable()

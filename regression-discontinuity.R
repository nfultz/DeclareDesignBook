# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

cutoff <- .5
control <- function(X) {
  as.vector(poly(X, 4, raw = TRUE) %*% c(.7, -.8, .5, 1))}
treatment <- function(X) {
  as.vector(poly(X, 4, raw = TRUE) %*% c(0, -1.5, .5, .8)) + .15}

rd_design <-
  # Model -------------------------------------------------------------------
declare_population(
  N = 1000,
  X = runif(N, 0, 1) - cutoff,
  noise = rnorm(N, 0, .1),
  Z = 1 * (X > 0)
) +
  declare_potential_outcomes(Y ~ Z * treatment(X) + (1 - Z) * control(X) + noise) +
  
  # Inquiry -----------------------------------------------------------------
declare_estimand(LATE = treatment(0) - control(0)) +
  
  # Data Strategy -----------------------------------------------------------------
declare_reveal(Y, Z) +
  
  # Answer Strategy ---------------------------------------------------------
declare_estimator(formula = Y ~ poly(X, 4) * Z,
                  model = lm_robust,
                  estimand = "LATE")

mock_data <- draw_data(rd_design)
X <- seq(-.5, .5, .005)
treatment_frame <-
  data.frame(
    X = X,
    Y = treatment(X),
    observed = ifelse(X > 0, "a", "b"),
    Z = 1
  )
control_frame <-
  data.frame(
    X = X,
    Y = control(X),
    observed = ifelse(X <= 0, "a", "b"),
    Z = 0
  )
plot_frame <-
  rbind(treatment_frame, control_frame)

ggplot(plot_frame, aes(x = X, y = Y, color = as.factor(Z))) +
  geom_line(aes(linetype = observed)) +
  geom_point(data = mock_data, alpha = .2, size = .5) +
  scale_linetype_discrete(name = "", labels = c("Observable", "Unobservable")) +
  #  scale_color_manual(name = "", labels = c("Untreated","Treated")) +
  geom_vline(xintercept = 0, size = .05) +
  xlab("Running Variable") +
  geom_segment(aes(
    x = 0,
    xend = 0,
    y = control(0),
    yend = treatment(0)
  ), color = "black")# +
#  dd_theme()





summary(rd_diagnosis)

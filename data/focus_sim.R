library(tidyverse)
library(here)
library(MASS)

set.seed(12345)
n_subj <- 200
days <- c(12, 19, 26, 33, 40, 47)

# 1. Generate Orthogonal Polynomial Basis
# We calculate the true time in weeks (0 to 5) to keep the simulation math clean
true_weeks <- (days - min(days)) / 7

p_basis <- as_tibble(poly(true_weeks, 2)) |>
  set_names(c("p1", "p2")) |>
  mutate(study_day = days)

# 2. Random Effects
# [1] Int, [2] Linear-Slope, [3] Quad-helper, [4] L1-Slope
u_vars <- c(25, 4, 1.5, 2)
u <- mvrnorm(n = n_subj, mu = rep(0, 4), Sigma = diag(u_vars))

l2_data <- tibble(
  subject = 1:n_subj,
  motivation = rnorm(n_subj, mean = 50, sd = 10),
  u0 = u[, 1],
  u_lin = u[, 2],
  u_quad = u[, 3],
  u_med = u[, 4]
) |>
  mutate(mot_c = motivation - 50)

# 3. Assemble the Dataset
focus_df <- expand_grid(subject = 1:n_subj, study_day = days) |>
  left_join(l2_data, by = "subject") |>
  left_join(p_basis, by = "study_day") |>
  mutate(
    # Re-calculate true weeks for the fixed effects math
    time_weeks = (study_day - min(study_day)) / 7,

    meditated = rbinom(n(), 1, prob = plogis(-1 + 0.05 * mot_c)),
    e = rnorm(n(), 0, 3.0),

    # FIXED EFFECTS: Purely Linear
    focus_score = 40 + (0.8 * mot_c) + u0 +
      (3.5 + 0.2 * mot_c + u_lin) * time_weeks +
      (u_quad * p2) +
      (4.0 + u_med) * meditated +
      e
  ) |>
  dplyr::select(subject, motivation, study_day, meditated, focus_score) |>
  mutate(focus_score = round(focus_score, 1), motivation = round(motivation, 1))

write_csv(focus_df, here("data", "focus_sim.csv"))

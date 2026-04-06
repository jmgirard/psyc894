library(tidyverse)
library(here)
library(MASS)

set.seed(8675309)
n_subj <- 150
time_pts <- 0:5

# 1. Generate the Orthogonal Polynomial Basis
# This ensures we have clear linear (p1) and quadratic (p2) signals
# to keep those specific models from crashing.
p_basis <- as_tibble(poly(time_pts, 2)) |>
  set_names(c("p1", "p2")) |>
  mutate(time = time_pts)

# 2. Random Effects (5 dimensions for absolute model stability)
# [1] Int, [2] Log-Slope, [3] Linear-helper, [4] Quad-helper, [5] Reading-Slope
# Using a diagonal matrix (zero correlations) maximizes numerical stability.
u_vars <- c(36, 14, 4, 2, 1.2)
u <- mvrnorm(n = n_subj, mu = rep(0, 5), Sigma = diag(u_vars))

l2_data <- tibble(
  subject = 1:n_subj,
  program = sample(c("A", "B"), n_subj, replace = TRUE),
  u0 = u[, 1], # Intercept
  u_log = u[, 2], # Primary Growth variation
  u_lin = u[, 3], # Linear helper (Prevents singular fit in fit_linear)
  u_quad = u[, 4], # Quadratic helper (Prevents NPD Hessian in fit_quad)
  u_read = u[, 5]  # Reading variation (For 12b random slope)
) |>
  mutate(prog_num = if_else(program == "B", 1, 0))

# 3. Assemble the Dataset
vocab_df <- expand_grid(subject = 1:n_subj, time = time_pts) |>
  left_join(l2_data, by = "subject") |>
  left_join(p_basis, by = "time") |>
  mutate(
    age_months = 120 + (time * 12),
    base_reading = rnorm(n_subj, 5, 2)[subject],
    hours_read = base_reading + rnorm(n(), 0, 1.5)
  ) |>
  mutate(
    hours_read_wp = hours_read - mean(hours_read),
    .by = subject
  ) |>
  mutate(
    e = rnorm(n(), 0, 2.0),
    # FIXED EFFECTS: Purely Logarithmic (The "True" Shape)
    # RANDOM EFFECTS: Distributed variation to satisfy all three model types
    vocab = 45 + (5 * prog_num) + u0 +
      (20 + 8 * prog_num + u_log) * log(time + 1) +
      (u_lin * time) +
      (u_quad * p2) +
      (1.5 + u_read) * hours_read_wp +
      e,
    program = factor(program, levels = c("A", "B"))
  ) |>
  dplyr::select(subject, program, age_months, vocab, hours_read)

write_csv(vocab_df, here("data", "vocab_sim.csv"))

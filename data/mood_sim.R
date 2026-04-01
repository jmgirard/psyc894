library(tidyverse)
library(MASS)

set.seed(42)
n_subjects <- 100
days <- 1:14
# Global hours: 8am, 12pm, 4pm, 8pm.
daily_fractions <- c(0.33, 0.50, 0.67, 0.83)

generate_subject_data <- function(subj_id) {
  # 1 prompt per day max, 80% compliance
  prompts <- tibble(
    subject = subj_id,
    studyday = days
  ) |>
    dplyr::filter(runif(n()) > 0.20)

  n_obs <- nrow(prompts)

  # Assign a specific time of day to each prompt
  studyday_frac <- prompts$studyday + sample(daily_fractions, n_obs, replace = TRUE)
  prompts$studyday_frac <- studyday_frac

  # True continuous DGP
  rho <- 0.40
  sigma_v <- 2

  dist_mat <- as.matrix(dist(studyday_frac))
  cov_mat <- (sigma_v^2) * (rho ^ dist_mat)

  v_ti <- mvrnorm(1, mu = rep(0, n_obs), Sigma = cov_mat)
  e_ti <- rnorm(n_obs, 0, 1)

  prompts$v_ti <- v_ti
  prompts$e_ti <- e_ti

  return(prompts)
}

mood_sim <- map_dfr(1:n_subjects, generate_subject_data) |>
  mutate(
    .by = subject,
    u_0i = rnorm(1, 0, 4),   # Random Intercept
    u_1i = rnorm(1, 0, 2.5), # Random slope for workout
    u_2i = rnorm(1, 0, 1.2), # Random slope for sleep_w (added)
    sleep_b = rnorm(1, 7, 1),
    sleep_w = rnorm(n(), 0, 1.2),
    sleep = sleep_b + sleep_w,
    workout = rbinom(n(), 1, prob = 0.5),
    mood = 50 +
      (3.0 * workout) +
      (2.5 * (sleep_b - 7)) +
      (1.8 * sleep_w) +
      (1.5 * workout * (sleep_b - 7)) +
      u_0i +
      (u_1i * workout) +
      (u_2i * sleep_w) +
      v_ti + e_ti
  ) |>
  dplyr::select(subject, studyday, studyday_frac, workout, sleep, mood)

write_csv(mood_sim, "data/mood_sim.csv")

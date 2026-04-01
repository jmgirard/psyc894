library(tidyverse)
library(glmmTMB)

set.seed(2026)

# 1. Define Population Parameters
N <- 100
max_days <- 14
prob_obs <- 0.7

gamma_00 <- 10
gamma_01 <- -2.0
gamma_10 <- -1.0
gamma_11 <- -0.3

tau_00 <- 5.0
tau_11 <- 0.5
rho <- 0.6
sigma_base <- 3.0
delta_1 <- 0.15

# 2. Simulate Level 2 (Subject) Data
l2_data <- tibble(
  subject = factor(1:N),
  stress_pm = rnorm(N, mean = 5, sd = 1.5),
  u_0i = rnorm(N, mean = 0, sd = tau_00),
  u_1i = rnorm(N, mean = 0, sd = tau_11)
)

# Custom function to generate spatial exponential error
generate_exp <- function(delta_t, rho, sigma) {
  n <- length(delta_t)
  e <- numeric(n)
  e[1] <- rnorm(1, 0, sigma[1])
  if(n > 1) {
    for(i in 2:n) {
      decay <- rho^delta_t[i]
      var_innov <- (sigma[i]^2) * (1 - decay^2)
      e[i] <- decay * e[i-1] + rnorm(1, 0, sqrt(var_innov))
    }
  }
  return(e)
}

# 3. Simulate Level 1 (Observation) Data and Final Outcome
sim_data <-
  expand_grid(
    subject = factor(1:N),
    studyday = 1:max_days
  ) |>
  filter(runif(n()) < prob_obs) |>
  left_join(l2_data, by = "subject") |>
  mutate(
    .by = subject,
    stress_pmc = rnorm(n(), mean = 0, sd = 1.2),
    stress = stress_pm + stress_pmc,

    # Restrict to 3 specific prompt times per day (9 AM, 2 PM, 7 PM)
    hour_ping = sample(c(9, 14, 19), n(), replace = TRUE),

    # Create continuous fractional time and round it to 2 decimal places
    studyday_frac = round((studyday - 1) + (hour_ping / 24), 2),

    # Calculate exact distance between observations
    delta_t = studyday_frac - lag(studyday_frac, default = first(studyday_frac)),
    current_sigma = exp(log(sigma_base) + delta_1 * stress_pmc),

    exp_error = generate_exp(delta_t, rho, current_sigma),

    performance = gamma_00 +
      (gamma_01 * stress_pm) +
      (gamma_10 * stress_pmc) +
      (gamma_11 * stress_pm * stress_pmc) +
      u_0i +
      (u_1i * stress_pmc) +
      exp_error
  ) |>
  dplyr::select(subject, studyday, studyday_frac, performance, stress)

glimpse(sim_data)

write_csv(sim_data, file = "data/ema_sim.csv")

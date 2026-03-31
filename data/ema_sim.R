library(tidyverse)
library(glmmTMB)

set.seed(2026)

# 1. Define Population Parameters
N <- 100         # Number of subjects
max_days <- 14   # Maximum duration of the study
prob_obs <- 0.7  # Probability of responding on a given day (creates unequal gaps)

gamma_00 <- 10   # Grand mean of performance
gamma_01 <- -2.0 # Between-person effect (higher average stress = lower average performance)
gamma_10 <- -1.0 # Within-person effect (higher stress today = lower performance today)
gamma_11 <- -0.3 # Cross-level interaction (auto-moderation: stress reactivity)

tau_00 <- 5.0    # SD of random intercepts
tau_11 <- 0.5    # SD of random slopes
rho <- 0.6       # Spatial exponential / OU correlation parameter
sigma_base <- 3.0 # Base residual standard deviation
delta_1 <- 0.15  # Effect of state stress on log-standard deviation (for dispformula)

# 2. Simulate Level 2 (Subject) Data
l2_data <- tibble(
  subject = factor(1:N),
  stress_pm = rnorm(N, mean = 5, sd = 1.5), # Person-mean stress (Trait)
  u_0i = rnorm(N, mean = 0, sd = tau_00),   # Random intercepts
  u_1i = rnorm(N, mean = 0, sd = tau_11)    # Random slopes
)

# Custom function to generate spatial exponential (OU) error with unequal intervals
generate_ou <- function(delta_t, rho, sigma) {
  n <- length(delta_t)
  e <- numeric(n)
  e[1] <- rnorm(1, 0, sigma[1])
  if(n > 1) {
    for(i in 2:n) {
      decay <- rho^delta_t[i]
      # Variance of the innovation depends on the time gap
      var_innov <- (sigma[i]^2) * (1 - decay^2)
      e[i] <- decay * e[i-1] + rnorm(1, 0, sqrt(var_innov))
    }
  }
  return(e)
}

# 3. Simulate Level 1 (Observation) Data and Final Outcome
sim_data <- expand_grid(
  subject = factor(1:N),
  studyday = 1:max_days
) |>
  filter(runif(n()) < prob_obs) |>
  left_join(l2_data, by = "subject") |>
  mutate(
    .by = subject,
    stress_pmc = rnorm(n(), mean = 0, sd = 1.2),
    stress = stress_pm + stress_pmc,

    delta_t = studyday - lag(studyday, default = 0),
    current_sigma = exp(log(sigma_base) + delta_1 * stress_pmc),

    ou_error = generate_ou(delta_t, rho, current_sigma),

    performance = gamma_00 +
      (gamma_01 * stress_pm) +
      (gamma_10 * stress_pmc) +
      (gamma_11 * stress_pm * stress_pmc) +
      u_0i +
      (u_1i * stress_pmc) +
      ou_error
  ) |>
  select(subject, studyday, performance, stress)

glimpse(sim_data)

write_csv(sim_data, file = "data/ema_sim.csv")

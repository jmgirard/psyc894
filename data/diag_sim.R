library(tidyverse)

set.seed(42)
n_schools <- 60
n_per_school <- 20
N <- n_schools * n_per_school

diag_sim <- tibble(
  school = factor(rep(1:n_schools, each = n_per_school)),
  homework = runif(N, 0, 10),
  iq = rnorm(N, 100, 15),
  parent_ed = rnorm(N, 12, 2)
) |>
  mutate(
    # Collinear predictor
    study_time = homework + rnorm(N, 0, 0.4),

    # Random Intercepts
    u0 = rep(rnorm(n_schools, 0, 4), each = n_per_school),

    # Errors
    e_normal = rnorm(N, 0, 3),
    # Much stronger heteroskedasticity (exponential scaling)
    e_het = rnorm(N, 0, sd = 0.5 + exp(homework * 0.35)),
    # Stronger skew (log-normal, recentered around 0)
    e_skew = rlnorm(N, meanlog = 0, sdlog = 1.2),
    e_skew = e_skew - mean(e_skew),

    # Outcome 1: Ideal
    score_ideal = 45 + 1.2 * homework + 0.1 * iq + u0 + e_normal,
    # Outcome 2: Heteroskedastic
    score_het = 45 + 1.2 * homework + u0 + e_het,
    # Outcome 3: Non-Normal (Skewed)
    score_skew = 45 + 1.2 * homework + u0 + e_skew,
    # Outcome 4: Outliers
    score_outlier = score_ideal
  )

# Add extreme outliers to 5 students to ensure tests trigger reliably
outlier_idx <- c(1, 2, 3, 400, 401)
diag_sim$score_outlier[outlier_idx] <- diag_sim$score_outlier[outlier_idx] + 150

write_csv(diag_sim, "data/diag_sim.csv")

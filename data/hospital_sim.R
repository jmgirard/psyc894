library(tidyverse)
library(lme4)
library(easystats)
library(ggeffects)
library(MASS)

# Set seed for reproducibility
set.seed(123)

# Define counts
n_hospitals <- 40
n_patients_per <- 50
n <- n_hospitals * n_patients_per

# Create Level 2 variables (one per group)
nurse_ratio <- rnorm(n_hospitals, mean = 0, sd = 1)  # Continuous L2 predictor
hospital_type <- rbinom(n_hospitals, size = 1, prob = 0.5)  # Binary L2 predictor

# Assign each observation to a Level 2 unit
hospital <- rep(1:n_hospitals, each = n_patients_per)

# Map Level 2 variables to Level 1 data
nurse_ratio <- rep(nurse_ratio, each = n_patients_per)
hospital_type <- rep(hospital_type, each = n_patients_per)

# Generate Level 1 predictors
wait_time <- rnorm(n, mean = 0, sd = 1)  # Continuous L1 predictor
visit_type <- rbinom(n, size = 1, prob = 0.5)  # Binary L1 predictor

# Define true model parameters
gamma_00 <-  3.0  # Fixed intercept
gamma_01 <-  0.8  # Fixed slope of nurse_ratio
gamma_02 <- -0.2  # Fixed slope of hospital_type
gamma_10 <- -0.8  # Fixed slope of wait_time
gamma_11 <- -0.2  # Cross-level interaction wait_time * nurse_ratio
gamma_20 <- -0.2  # Fixed slope of visit_type
gamma_21 <- -0.5  # Cross-level interaction visit_type * hospital_type

# Define random effect SDs
tau_00 <- 1.0   # SD of random intercepts
tau_11 <- 0.4   # SD of random slopes for wait_time
tau_22 <- 0.3   # SD of random slopes for visit_type

# Define random effect correlations
rho_01 <- 0.1   # Correlation between random intercept and random slope for wait_time
rho_02 <- 0.1   # Correlation between random intercept and random slope for visit_type
rho_12 <- 0.05  # Correlation between random slopes (wait_time & visit_type)

# Construct the covariance matrix for random effects
cov_matrix <- matrix(c(
  tau_00^2, rho_01 * tau_00 * tau_11, rho_02 * tau_00 * tau_22,
  rho_01 * tau_00 * tau_11, tau_11^2, rho_12 * tau_11 * tau_22,
  rho_02 * tau_00 * tau_22, rho_12 * tau_11 * tau_22, tau_22^2
), nrow = 3, byrow = TRUE)

# Generate correlated random effects
random_effects <- mvrnorm(n_hospitals, mu = c(0, 0, 0), Sigma = cov_matrix)

# Assign random effects
u_0j <- rep(random_effects[, 1], each = n_patients_per)  # Random intercepts
u_1j <- rep(random_effects[, 2], each = n_patients_per)  # Random slopes for wait_time
u_2j <- rep(random_effects[, 3], each = n_patients_per)  # Random slopes for visit_type

# Generate residual errors
sigma <- 1  # SD of the residuals
e_ij <- rnorm(n, mean = 0, sd = sigma)

# Generate outcome variable Y
satisfaction <-
  (gamma_00 + gamma_01 * nurse_ratio + gamma_02 * hospital_type + u_0j) +  # b_0j
  (gamma_10 + gamma_11 * nurse_ratio + u_1j) * wait_time +  # b_1j
  (gamma_20 + gamma_21 * hospital_type + u_2j) * visit_type +  # b_2j
  e_ij

# Combine into a dataframe
sim_data <- data.frame(satisfaction, wait_time, visit_type, hospital, nurse_ratio, hospital_type)

# Convert categorical variables for better interpretability
tidy_data <- sim_data |>
  mutate(
    hospital = factor(hospital, levels = 1:n_hospitals, labels = sprintf("H%02d", 1:n_hospitals)),
    visit_type = factor(
      visit_type,
      levels = 0:1,
      labels = c("Routine", "Emergency")
    ),
    hospital_type = factor(
      hospital_type,
      levels = 0:1,
      labels = c("Non-Teaching", "Teaching")
    )
  ) |>
  standardize()

write_csv(tidy_data, "data/hospital_sim.csv")

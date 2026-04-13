library(tidyverse)

# Set seed for reproducibility
set.seed(42)

# Define sample size
n_clinics <- 50
n_patients <- 20
clinic_id <- rep(1:n_clinics, each = n_patients)

# 1. Singular Fit Predictors (Mean Centered!)
patient_age <- rnorm(n_clinics * n_patients, mean = 50, sd = 10)
age_c <- patient_age - 50 # Centered age to prevent eigenvalue warnings

# 2. Extreme Scaling Predictors
dosage_kg <- rnorm(n_clinics * n_patients, mean = 0.001, sd = 0.0001)

# 3. Collinearity Predictors (Adjusted to trip up glmmTMB)
# glmmTMB is robust, so we tighten the correlation and add a multiplier
# to create a flat but skewed likelihood ridge that stalls the optimizer.
stress_score <- rnorm(n_clinics * n_patients, mean = 0, sd = 10)
anxiety_score <- (stress_score * 0.8) + rnorm(n_clinics * n_patients, mean = 0, sd = 0.0001)

# 4. Categorical Predictor
treatment_group <- factor(rep(c("Placebo", "Active"), times = (n_clinics * n_patients) / 2))

# Generate Random effects
u_0 <- rnorm(n_clinics, mean = 0, sd = 2)   # Random Intercepts
u_1 <- rep(0, n_clinics)                    # Zero-variance slope (For Warning 1)
u_2 <- rnorm(n_clinics, mean = 0, sd = 1.5) # TRUE variance slope (For Warning 3)
u_treat <- u_0 * 0.5
treat_val <- if_else(treatment_group == "Active", 0.5, -0.5)

# Engineer Outcomes
# 1. Singular Fit
blood_pressure <- 120 + 2*treat_val + u_0[clinic_id] + u_treat[clinic_id]*treat_val + rnorm(1000, sd = 5)

# 2. Extreme Scaling / NPD Hessian
viral_load <- 1000000 + 500000*dosage_kg + u_0[clinic_id]*10000 + rnorm(1000, sd = 1000)

# 3. Collinearity / Max|Grad
# We inflate the fixed effects slightly so the unresolved gradients remain large
cortisol_level <- 10 + 5*stress_score - 4*anxiety_score + u_0[clinic_id] + u_2[clinic_id]*stress_score + rnorm(1000, sd = 5)

# Combine and save
trouble_sim_data <- tibble(
  clinic_id, patient_age, age_c, treatment_group, dosage_kg,
  stress_score, anxiety_score, blood_pressure, viral_load, cortisol_level
)

write_csv(trouble_sim_data, "data/trouble_sim.csv")

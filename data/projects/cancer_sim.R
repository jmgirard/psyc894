library(tidyverse)
library(MASS)
conflicted::conflict_prefer_all(winner = "dplyr", losers = "MASS")

set.seed(2026)

# 1. Define Population & Design Parameters
n_participants <- 30
days <- 14
prompts_per_day <- 4

# Fixed Effects
beta_0        <- 40
beta_pain_cw  <- -2.5   # Within-person pain effect
beta_pain_cb  <- -1.0   # Between-person pain effect
beta_intent   <- 8.0
beta_social   <- 5.0
beta_age      <- -1.0
beta_surg2    <- -6.0

# Random Effects & Autocorrelation
var_u0     <- 100
var_upain  <- 2
var_ou     <- 25
ou_range   <- 1.5
shape_sn   <- 4     # Generates the positive skew
scale_sn   <- 12

# Covariance Matrix (Intercept and Pain slope)
Sigma_RE <- diag(c(var_u0, var_upain))

# 2. Simulate Level-2 Data (Participants)
u_i <- mvrnorm(n_participants, mu = c(0, 0), Sigma = Sigma_RE) |>
  as.data.frame() |>
  setNames(c("u0", "u_pain"))

participants <- tibble(
  participant = paste0("ID", str_pad(1:n_participants, width = 2, pad = "0")),
  age = runif(n_participants, 12, 18),
  surgery_type = sample(c(1, 2), n_participants, replace = TRUE)
) |>
  bind_cols(u_i) |>
  mutate(age_c = age - mean(age))

# 3. Simulate Level-1 Data (EMA Prompts)
sim_data_full <- expand_grid(
  participant = participants$participant,
  day = 1:days,
  prompt = 1:prompts_per_day
) |>
  slice_sample(prop = 0.85) |> # Keep 85% of prompts to simulate missingness
  left_join(participants, by = "participant") |>
  mutate(
    # Time code: 0.2, 0.4, 0.6, 0.8, 1.2, 1.4...
    # Accounts for exact timing and the overnight gap!
    time_code = (day - 1) + (prompt * 0.20),
    pain_raw = runif(n(), 0, 10),
    intent_to_exercise = rbinom(n(), 1, prob = 0.3),
    recent_socialization = rbinom(n(), 1, prob = 0.5),
    .by = participant
  ) |>
  mutate(
    pain_cw = pain_raw - mean(pain_raw),
    pain_cb = mean(pain_raw),
    .by = participant
  )

# 4. Generate Autocorrelation & Skew-Normal Outcome
sim_data_final <- sim_data_full |>
  group_split(participant) |>
  map_dfr(function(person_df) {

    # Distance matrix using the specific time_code
    dist_mat <- as.matrix(dist(person_df$time_code))
    Cov_OU <- var_ou * exp(-dist_mat / ou_range)
    person_df$ou_error <- mvrnorm(1, mu = rep(0, nrow(person_df)), Sigma = Cov_OU)

    # Skew-Normal noise generation
    delta <- shape_sn / sqrt(1 + shape_sn^2)
    u0_sn <- rnorm(nrow(person_df))
    u1_sn <- rnorm(nrow(person_df))
    sn_noise <- delta * abs(u0_sn) + sqrt(1 - delta^2) * u1_sn

    person_df |>
      mutate(
        mu = (beta_0 + u0) +
          (beta_pain_cw + u_pain) * pain_cw +
          (beta_pain_cb) * pain_cb +
          beta_intent * intent_to_exercise +
          beta_social * recent_socialization +
          beta_age * age_c +
          beta_surg2 * (surgery_type == 2) +
          ou_error,
        mvpa = pmax(0, mu + scale_sn * sn_noise)
      )
  }) |>
  select(participant, day, prompt, time_code, mvpa, pain = pain_raw,
         intent_to_exercise, recent_socialization, age, surgery_type)

write_csv(sim_data_final, "data/projects/cancer_sim.csv")

library(glmmTMB)

ready_for_model <- sim_data_final |>
  mutate(
    age_c = age - mean(age),
    surgery_fct = factor(surgery_type)
  ) |>
  mutate(
    pain_cb = mean(pain),
    pain_cw = pain - pain_cb,
    .by = participant
  ) |>
  mutate(time_factor = numFactor(time_code))

fit_ema <- glmmTMB(
  mvpa ~ 1 + pain_cw + pain_cb + intent_to_exercise + recent_socialization +
    age_c + surgery_fct +
    (1 + pain_cw | participant) +
    exp(time_factor + 0 | participant),
  data = ready_for_model
)

model_parameters(fit_ema)
check_predictions(fit_ema)
check_normality(fit_ema, effects = "fixed") |> plot()

library(tidyverse)
library(MASS)
conflicted::conflict_prefer_all(winner = "dplyr", losers = "MASS")

set.seed(2026)

# 1. Define Population Parameters
n_per_group <- 50
n_total <- n_per_group * 2
possible_ages <- c(15, 17, 19, 21, 23) # Starting at age 15, every 2 years

# Fixed Effects for Intercept (Baseline at age 15)
beta_0_int   <- 60    # Baseline xmotor for TD, Female, FSIQ 100
gamma_group_int <- -15   # ASD starts 15 points lower
gamma_fsiq_int  <- 0.2   # +1 FSIQ = +0.2 baseline xmotor
gamma_sex_int   <- 3     # Males start 3 points higher

# Fixed Effects for log-slope (Growth Rate)
beta_1_slope   <- 20    # Base growth rate for TD, Female, FSIQ 100
gamma_group_slope <- -8    # ASD grows slower (-8 to the log slope)
gamma_fsiq_slope  <- 0.4   # Higher FSIQ grows faster
gamma_sex_slope   <- -2    # Males grow slightly slower

# Random Effects (Intercept and log-slope)
var_u0 <- 30
var_u1 <- 15
cov_01 <- 0.2 * sqrt(var_u0 * var_u1)
sigma_res <- 4

Sigma <- matrix(c(
  var_u0, cov_01,
  cov_01, var_u1
), nrow = 2)

# 2. Simulate Level-2 Data (Participants)
u_i <- mvrnorm(n_total, mu = c(0, 0), Sigma = Sigma) |>
  as.data.frame() |>
  setNames(c("u0", "u_slope"))

participants <- tibble(
  participant = paste0("P", str_pad(1:n_total, width = 3, pad = "0")),
  group = rep(c("TD", "ASD"), each = n_per_group),
  sex = sample(c("Female", "Male"), size = n_total, replace = TRUE),
  FSIQ = if_else(group == "TD",
                 rnorm(n_total, mean = 100, sd = 15),
                 rnorm(n_total, mean = 92, sd = 15)) |> round()
) |>
  bind_cols(u_i) |>
  mutate(
    group_num = if_else(group == "ASD", 1, 0),
    sex_num = if_else(sex == "Male", 1, 0),
    fsiq_c = FSIQ - 100
  )

# 3. Simulate Level-1 Data (Ragged Observations)
sim_data_full <- expand_grid(
  participant = participants$participant,
  age = possible_ages
) |>
  left_join(participants, by = "participant") |>
  # Randomly keep between 3 and 5 timepoints per person
  slice_sample(n = sample(3:5, 1), by = participant) |>
  mutate(
    # Math happens here: we shift age so baseline (15) equals 1, making log(1) = 0
    time_metric = age - 14,
    log_time = log(time_metric),
    ind_error = rnorm(n(), mean = 0, sd = sigma_res),
    .by = participant
  ) |>
  mutate(
    int_i = beta_0_int +
      (gamma_group_int * group_num) +
      (gamma_fsiq_int * fsiq_c) +
      (gamma_sex_int * sex_num) +
      u0,
    slope_i = beta_1_slope +
      (gamma_group_slope * group_num) +
      (gamma_fsiq_slope * fsiq_c) +
      (gamma_sex_slope * sex_num) +
      u_slope,
    xmotor = int_i + (slope_i * log_time) + ind_error
  )

# 4. Wrangle into "Messy" Student Format
student_data_wide <- sim_data_full |>
  select(participant, group, sex, FSIQ, age, xmotor) |>
  mutate(age = paste0("age_", age)) |>
  pivot_wider(
    names_from = age,
    values_from = xmotor
  ) |>
  select(participant, group, sex, FSIQ, age_15, age_17, age_19, age_21, age_23)

# 5. Write for student
write_csv(student_data_wide, file = "data/projects/xmotor_sim.csv")

# ------------------------------------------------------------------------------

# Student Wrangling Step
ready_for_model <-
  student_data_wide |>
  pivot_longer(
    cols = starts_with("age_"),
    names_to = "age",
    values_to = "xmotor",
    names_prefix = "age_",
    values_drop_na = TRUE
  ) |>
  mutate(
    age = as.numeric(age),
    time = age - 15,
    FSIQ_c = FSIQ - mean(FSIQ)
  )

# Student Target Model
library(glmmTMB)

fit_growth <- glmmTMB(
  xmotor ~ 1 + log(time + 1) * (group + sex + FSIQ_c) +
    (1 + log(time + 1) | participant),
  data = ready_for_model
)

model_parameters(fit_growth)

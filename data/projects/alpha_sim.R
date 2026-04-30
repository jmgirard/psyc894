library(tidyverse)
library(MASS)
conflicted::conflict_prefer_all(winner = "dplyr", losers = "MASS")

set.seed(2026)

# 1. Define Population Parameters
n_participants <- 40
timepoints <- c("pre", "mid", "post")

# Alpha time effects
beta_0    <- 50
beta_mid  <- 12
beta_post <- 14

# VQ time effects (New!)
vq_mid    <- 6    # VQ jumps by 6 at mid
vq_post   <- 8    # VQ plateaus, gaining only 2 more at post

beta_vqw  <- 2
beta_vqb  <- 3

var_u0 <- 25
var_u2 <- 4
sigma_res <- 4

cov_02 <- 0.2 * sqrt(var_u0 * var_u2)

Sigma <- matrix(c(
  var_u0, cov_02,
  cov_02, var_u2
), nrow = 2)

# 2. Simulate Level-2 Data
u_i <- mvrnorm(n_participants, mu = c(0, 0), Sigma = Sigma) |>
  as.data.frame() |>
  setNames(c("u0", "u_vqw"))

participants <- tibble(
  participant = paste0("P", str_pad(1:n_participants, width = 2, pad = "0")),
  VQ_between_true = rnorm(n_participants, mean = 100, sd = 15)
) |>
  bind_cols(u_i)

# 3. Simulate Level-1 Data
sim_data_full <- expand_grid(
  participant = participants$participant,
  time_fct = factor(timepoints, levels = c("pre", "mid", "post"))
) |>
  left_join(participants, by = "participant") |>
  mutate(
    # Create the non-linear time effects for both VQ and Alpha
    time_effect_alpha = case_when(
      time_fct == "pre"  ~ 0,
      time_fct == "mid"  ~ beta_mid,
      time_fct == "post" ~ beta_post
    ),
    time_effect_vq = case_when(
      time_fct == "pre"  ~ 0,
      time_fct == "mid"  ~ vq_mid,
      time_fct == "post" ~ vq_post
    ),
    # Generate the actual within-person VQ data with the time trend included
    VQ_noise = rnorm(n(), mean = 0, sd = 5),
    VQ_within_true = time_effect_vq + VQ_noise,
    ind_error = rnorm(n(), mean = 0, sd = sigma_res),
    .by = participant
  ) |>
  # Inject extreme outliers for DHARMa to detect
  mutate(
    is_outlier = row_number() %in% sample(n(), 4), # Pick 4 random rows
    ind_error = if_else(is_outlier, ind_error + sample(c(-25, 25), n(), replace = TRUE), ind_error)
  ) |>
  mutate(
    # Calculate the raw observed VQ that the students will see
    VQ = VQ_between_true + VQ_within_true,

    # Generate Alpha using the true within-person components
    alpha = (beta_0 + u0) +
      time_effect_alpha +
      (beta_vqw + u_vqw) * VQ_within_true +
      (beta_vqb) * VQ_between_true +
      ind_error
  )

# 4. Wrangle into "Messy" Student Format
student_data_wide <- sim_data_full |>
  select(participant, time_fct, VQ, alpha) |>
  pivot_wider(
    names_from = time_fct,
    values_from = c(VQ, alpha)
  )

# 5. Write for student
write_csv(student_data_wide, file = "data/projects/alpha_sim.csv")


# ------------------------------------------------------------------------------

# Data Preparation
ready_for_model <- student_data_wide |>
  pivot_longer(
    cols = -participant,
    names_to = c(".value", "task"),
    names_sep = "_"
  ) |>
  mutate(
    task = factor(task, levels = c("pre", "mid", "post")),
    VQ_between = mean(VQ),
    VQ_within = VQ - VQ_between,
    .by = participant
  )

# The Target Model
library(glmmTMB)
library(easystats)

fit <- glmmTMB(
  alpha ~ 1 + task + VQ_within + VQ_between + (1 + VQ_within | participant),
  data = ready_for_model
)

model_parameters(fit)
check_predictions(fit)
check_collinearity(fit)
check_normality(fit, effects = "fixed") |> plot()
check_normality(fit, effects = "random") |> plot()

library(DHARMa)
testResiduals(fit)

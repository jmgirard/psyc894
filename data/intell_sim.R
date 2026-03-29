# Load packages
library(tidyverse)
library(MASS)
conflicted::conflict_prefer("select", "dplyr")
set.seed(123)

# Parameters
n_participants <- 150
n_tasks <- 4
n_trials_per_task <- 3
total_trials <- n_tasks * n_trials_per_task
task_names <- c("Verbal Reasoning", "Working Memory", "Perceptual Reasoning", "Processing Speed")

# Create dataset based on trials first
df <- expand.grid(trial = 1:total_trials, subject = 1:n_participants)

# Randomly assign tasks to trials for each subject (3 of each task per person)
df <- df |>
  mutate(
    .by = subject,
    task = sample(rep(factor(task_names, levels = task_names), times = n_trials_per_task))
  )

# Participant-level predictor
participant_data <- data.frame(
  subject = 1:n_participants,
  icontrol = rnorm(n_participants, mean = 0, sd = 1)
)
df <- df |> left_join(participant_data, by = "subject") |> as_tibble()

# Dummy codes for tasks (reference = task1)
df <- df |>
  mutate(
    memory = ifelse(task == task_names[[2]], 1, 0),
    perceptual = ifelse(task == task_names[[3]], 1, 0),
    speed = ifelse(task == task_names[[4]], 1, 0)
  )

# Fixed effects
intercept <- 10
b_wm <- 5
b_pr <- -3
b_ps <- 0
b_icontrol <- 4
b_icontrol_wm <- 2  # interaction effect

# Random effects: intercept + slopes for task2, task3, task4
rand_effects_cov <- matrix(c(
  25,  3,  3,  3,
  3, 10,  2,  2,
  3,  2, 10,  2,
  3,  2,  2, 10
), nrow = 4)

rand_effects <- MASS::mvrnorm(n = n_participants, mu = rep(0, 4), Sigma = rand_effects_cov)
colnames(rand_effects) <- c("ri", "rs_wm", "rs_pr", "rs_ps")

rand_df <- data.frame(subject = 1:n_participants, rand_effects)
df <- df |> left_join(rand_df, by = "subject")

# Autoregressive Error generation (now across 12 trials)
sigma <- 3
rho <- 0.45 # Moderate autocorrelation
times <- 1:total_trials
H <- abs(outer(times, times, "-"))
V <- sigma^2 * (rho^H)

# Generate AR1 errors
ar1_errors <- mvrnorm(n = n_participants, mu = rep(0, total_trials), Sigma = V)

# Format and join errors
ar1_df <- data.frame(subject = 1:n_participants, ar1_errors)
colnames(ar1_df)[2:(total_trials + 1)] <- paste0("err_", 1:total_trials)
ar1_long <- ar1_df |>
  pivot_longer(cols = starts_with("err_"), names_to = "trial_char", values_to = "error") |>
  mutate(trial = as.integer(str_extract(trial_char, "\\d+"))) |>
  select(subject, trial, error)

df <- df |> left_join(ar1_long, by = c("subject", "trial"))

# Outcome variable
df <- df |>
  mutate(
    performance = intercept +
      b_wm * memory +
      b_pr * perceptual +
      b_ps * speed +
      b_icontrol * icontrol +
      b_icontrol_wm * icontrol * memory +
      ri +
      rs_wm * memory +
      rs_pr * perceptual +
      rs_ps * speed +
      error
  )

out <-
  df |>
  select(subject, icontrol, trial, task, performance) |>
  arrange(subject, trial)

out |> write_csv(file = "data/intell_sim.csv")

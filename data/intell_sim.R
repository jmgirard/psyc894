set.seed(123)

# Load packages
library(MASS)  # for mvrnorm
library(tidyverse)

# Parameters
n_participants <- 150
n_tasks <- 4
task_names <- c("verb_reason", "work_memory", "perc_reason", "proc_speed")

# Create long-format dataset
df <- expand.grid(id = 1:n_participants, task = factor(task_names, levels = task_names))

# Participant-level predictor
participant_data <- data.frame(
  id = 1:n_participants,
  icontrol = rnorm(n_participants, mean = 0, sd = 1)
)
df <- df %>% left_join(participant_data, by = "id") |> as_tibble()

# Dummy codes for tasks (reference = task1)
df <- df %>%
  mutate(
    memory = ifelse(task == task_names[[2]], 1, 0),
    perceptual = ifelse(task == task_names[[3]], 1, 0),
    speed = ifelse(task == task_names[[4]], 1, 0)
  )

# Fixed effects
intercept <- 50
b_wm <- 5
b_pr <- -3
b_ps <- 0
b_icontrol <- 4
b_icontrol_wm <- 2  # interaction effect

# Random effects: intercept + slopes for task2, task3, task4
# 4-dimensional multivariate normal: intercept, task2, task3, task4
rand_effects_cov <- matrix(c(
  25,  3,  3,  3,
  3, 10,  2,  2,
  3,  2, 10,  2,
  3,  2,  2, 10
), nrow = 4)

rand_effects <- MASS::mvrnorm(n = n_participants, mu = rep(0, 4), Sigma = rand_effects_cov)
colnames(rand_effects) <- c("ri", "rs_wm", "rs_pr", "rs_ps")

rand_df <- data.frame(id = 1:n_participants, rand_effects)
df <- df %>% left_join(rand_df, by = "id")

# Residual error
df$error <- rnorm(nrow(df), mean = 0, sd = 3)

# Outcome variable
df <- df %>%
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

write_csv(df |> select(-c(ri:error)), "data/intell_sim.csv")

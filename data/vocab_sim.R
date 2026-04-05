library(tidyverse)
library(here)

set.seed(8675309)
n_subj <- 150
raw_months <- c(120, 132, 144, 156, 168, 180) # Ages 10 to 15 in months

l2_data <- tibble(
  subject = 1:n_subj,
  program = sample(c("Control", "Intervention"), n_subj, replace = TRUE)
) |>
  mutate(
    prog_num = ifelse(program == "Intervention", 1, 0),
    u0 = rnorm(n_subj, 0, 8),
    u1 = rnorm(n_subj, 0, 2)
  )

vocab_df <- expand_grid(subject = 1:n_subj, age_months = raw_months) |>
  left_join(l2_data, by = "subject") |>
  mutate(
    # Create the true "time" under the hood for the math
    true_time = (age_months - 120) / 12,
    e = rnorm(n(), 0, 4),
    vocab = 50 + u0 + (5 + 3 * prog_num + u1) * true_time - 0.4 * (true_time^2) + e,
    program = factor(program, levels = c("Control", "Intervention"))
  ) |>
  select(subject, age_months, program, vocab)

write_csv(vocab_df, here("data", "vocab_sim.csv"))

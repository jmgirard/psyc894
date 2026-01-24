library(tidyverse)

fscores <-
  read_rds(r"(X:\BLS_Diary\BLS_DSEM\7_tidy_ema_cfa_fs_berge.rds)") |>
  transmute(
    subject = file,
    studyday = day,
    positive = rowSums(pick(energetic, alert, inspired, determined, happy)) - 5,
    negative = rowSums(pick(hostile, irritable, ashamed, upset, afraid, lonely)) - 6,
    sleep = sleep - 1,
    social = socialp - 1
  )

demo <-
  read_csv(r"(X:\BLS_Diary\BLS_DSEM\BLS_data_release_demographics.csv)") |>
  transmute(
    subject = ID,
    age = Age,
    sex = Sex
  )

merged <-
  right_join(demo, fscores, by = join_by(subject)) |>
  mutate(subject = sprintf("P%02d", as.integer(factor(subject)))) |>
  filter(studyday <= 100) |>
  arrange(subject, studyday) |>
  drop_na() |>
  print()

write_csv(merged, "data/daily.csv")

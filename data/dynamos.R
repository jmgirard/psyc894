library(tidyverse)
holi <- read_csv(r"(Z:\DynAMoS\1_RAWDATA\Ratings\holistic_by_scale.csv)")
demo <- read_csv(r"(Z:\DynAMoS\1_RAWDATA\Demographics\demographics.csv)")

merged <-
  left_join(holi, demo, by = join_by(Rater)) |>
  mutate(Valence = Positive - Negative) |>
  rename(Clip = Abbrev) |>
  select(-c(Positive, Negative, Ethnicity)) |>
  print()

write_csv(merged, "data/dynamos.csv")

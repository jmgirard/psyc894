library(tidyverse)
library(readxl)
library(vroom)

sheet1 <- read_xlsx(r"(Z:\MMSE\1_RAWDATA\Self_Report\self_report.xlsx)", sheet = 1)
sheet3 <- read_xlsx(r"(Z:\MMSE\1_RAWDATA\Self_Report\self_report.xlsx)", sheet = 3)

occfiles <- dir(r"(Z:\MMSE\1_RAWDATA\FACS_Codes\AU_OCC)",
                pattern = ".csv$", full.names = TRUE)
occ <- vroom(file = occfiles, delim = ",", id = "file") |>
  print()

occt <-
  occ |>
  select(file, smile = `12`) |>
  mutate(smile = na_if(smile, 9)) |>
  summarize(.by = file, smile = mean(smile, na.rm = TRUE)) |>
  mutate(file = str_remove(basename(file), ".csv")) |>
  separate_wider_delim(file, delim = "_", names = c("subject", "task")) |>
  print()

s1t <-
  sheet1 |>
  mutate(
    positive = rowSums(pick(Relaxed, Amused), na.rm = TRUE),
    negative = rowSums(pick(Disgusted, Afraid, Angry, Frustrated, Sad, Nervous,
                            Pained, Embarrassed, Startled, Skeptical), na.rm = TRUE),
    valence = positive - negative
  ) |>
  select(
    subject = Subject,
    task = Task,
    valence
  ) |>
  filter(task %in% c("T1", "T6", "T7", "T8")) |>
  print()

s3t <-
  sheet3 |>
  transmute(
    subject = Subject,
    sex = str_to_title(Gender),
    age = Age,
    usyears = USA_Years
  ) |>
  print()

merged <-
  left_join(s1t, occt, by = join_by(subject, task)) |>
  left_join(s3t, by = join_by(subject)) |>
  mutate(subject = sprintf("S%03d", as.integer(factor(subject)))) |>
  print()

write_csv(merged, "data/emolab.csv")

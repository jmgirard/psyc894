library(tidyverse)
d1 <- read_tsv("C:/Users/j553g371/Downloads/title.basics.tsv/title.basics.tsv",
               col_types = cols(.default = col_character()),
               na = c("", "NA", "\\N"))
d2 <- read_tsv("C:/Users/j553g371/Downloads/title.ratings.tsv/title.ratings.tsv",
               col_types = cols(.default = col_character()))

dm <-
  inner_join(d1, d2, by = "tconst") |>
  mutate(
    across(c(startYear, runtimeMinutes, numVotes, averageRating), as.numeric)
  ) |>
  filter(
    startYear %in% c(2021, 2022, 2023, 2024),
    titleType == "movie",
    isAdult == "0",
    numVotes >= 100,
    runtimeMinutes >= 30,
    runtimeMinutes <= 60*3,
    !is.na(genres)
  ) |>
  mutate(firstGenre = str_extract(genres, "^[^,]+")) |>
  select(
    title = primaryTitle,
    year = startYear,
    runtime = runtimeMinutes,
    votes = numVotes,
    rating = averageRating,
    genre = firstGenre
  ) |>
  filter(
    .by = genre,
    n() >= 10
  ) |>
  arrange(year, title) |>
  print()

write_csv(dm, "data/movies.csv")

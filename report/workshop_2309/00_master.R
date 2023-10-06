# load necessary packages
pacman::p_load("tidyverse", "treemap", "RColorBrewer", "data.table")


output <- "report/workshop_2309/plots/"


data_source <- read_csv("data/data_all.csv") |>
  distinct(country, source_type, document, comment_id, .keep_all = TRUE) |>
  filter(!is.na(comment_ideation)) |>
  mutate(
    country = factor(country, levels = c("EN", "DE", "FR")) |>
      recode(EN = "UK"),
    comment_ideation = factor(comment_ideation,
      levels = c("I0", "I0c", "I1", "I1ASC", "I1a", "I2"),
      labels = c(
        "Not AS", "Counter Speech", "Antisemitism", "Contextual AS",
        "Confirmation", "Unclear Ideation"
      )
    ),
    comment_antisem = case_when(
      as.integer(comment_ideation) %in% c(1, 2) ~ FALSE,
      as.integer(comment_ideation) %in% c(3, 4, 5) ~ TRUE,
      .default = NA
    )
  )

table(data_source$comment_ideation, useNA = "ifany")
proportions(table(data_source$comment_ideation))

discourse_same <- data_source |>
  distinct(country, discourse) |>
  filter(n() == 3, .by = discourse) |>
  pull(discourse) |>
  unique()

col_ideation <- c(
  "#598b4dde", "#2d7227de", "#992222de", "#b44403de", "#da6639de"
)

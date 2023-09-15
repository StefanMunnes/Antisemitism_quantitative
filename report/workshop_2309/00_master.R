# load necessary packages
pacman::p_load("tidyverse", "treemap", "RColorBrewer", "data.table")


output <- "report/workshop_2309/plots/"


data_source <- read_csv("data/data_all.csv") |>
  distinct(country, source_type, document, comment_id, .keep_all = TRUE) |>
  mutate(comment_ideation = factor(comment_ideation,
    levels = c("I0", "I0c", "I1", "I1ASC", "I1a")
  ))


col_ideation <- c(
  "#598b4dde", "#2d7227de", "#992222de", "#b44403de", "#da6639de"
)

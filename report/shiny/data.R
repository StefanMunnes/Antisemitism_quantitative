library(dplyr)
library(stringr)
library(shiny)


# ---- 1. prep data ----
data_all <- read_csv("data/data_all.csv")


# 1.1 discourse event information
discourse_title <- read_csv2("data/doc/discourse_events.csv",
  col_names = c("discourse", "title")
)

discourse_list <- list(
  "2021" = discourse_title$discourse[grepl("21", discourse_title$discourse)] |>
    str_remove("^2._"),
  "2022" = discourse_title$discourse[grepl("22", discourse_title$discourse)] |>
    str_remove("^2._"),
  "2023" = discourse_title$discourse[grepl("23", discourse_title$discourse)] |>
    str_remove("^2._")
)

write.csv(discourse_list, "report/shiny/data/discourse_list.csv")


data_de <- data_all |>
  distinct(country, document, comment_id, .keep_all = TRUE) |>
  summarize(
    date_min = min(source_date, na.rm = TRUE) |> format("%d.%m.%Y"),
    date_max = max(source_date, na.rm = TRUE) |> format("%d.%m.%Y"),
    cntrs = paste(unique(country), collapse = ", "),
    docs = n_distinct(document),
    srcs = n_distinct(source_outlet),
    comms = n(),
    comms_as = sum(str_detect(comment_ideation, "^I1"), na.rm = TRUE),
    comms_as_prop = round(comms_as / comms, 4) * 100,
    .by = discourse
  ) |>
  left_join(discourse_title)

write.csv(data_de, "report/shiny/data/data_de.csv")


# 1.2 lexicon labels
lexicon_fct_labels <- read.csv2(
  "data/doc/lexicon_dictionary.csv",
  header = FALSE,
  fileEncoding = "UTF-8-BOM"
) |>
  transmute(
    code_lex = str_extract(str_squish(V2), "(.+?) ", group = 1),
    code_lex_main = str_sub(code_lex, 1, 1) |> as.numeric(),
    code_lex_name = str_extract(str_squish(V2), " (.+)", group = 1) |>
      str_wrap(width = 30) |>
      str_replace_all("\\n", "<br>")
  ) |>
  unique() |>
  arrange(code_lex)

write.csv(lexicon_fct_labels, "report/shiny/data/lexicon_fct_labels.csv")


# 1.3 code data (proportion, code_group, colors)

# create template for all valid combinations of codes, country and discourse

tmp_data_de_cntry <- separate_rows(data_de, cntrs, sep = ", ") |>
  rename(country = cntrs) |>
  select(country, discourse)

tmp_data_de_code <- expand.grid(
  data_de$discourse,
  lexicon_fct_labels$code_lex
) |>
  set_names(c("discourse", "code_lex"))

tmp_data_de_cntry_code <- left_join(
  tmp_data_de_cntry, tmp_data_de_code,
  by = "discourse",
  relationship = "many-to-many"
)


data_code <- data_all |>
  # keep just antisemitic comments
  filter(str_detect(comment_ideation, "I1")) |>
  # count observations for discourse events, and discourse events by country
  mutate(
    N_de_ctr = n_distinct(document, comment_id),
    N_de_ctr_first = ifelse(row_number() == 1, N_de_ctr, NA),
    .by = c(country, discourse)
  ) |>
  mutate(N_de = sum(N_de_ctr_first, na.rm = TRUE), .by = discourse) |>
  summarize(
    N_de = first(N_de),
    N_de_ctr = first(N_de_ctr),
    n_de_ctr = n(),
    .by = c(country, discourse, code_lex, code_lex_name)
  ) |>
  na.omit() |>
  mutate(n_de = sum(n_de_ctr), .by = c(discourse, code_lex)) |>
  # calculate proportions by discourse events and by country
  mutate(
    per_de = (n_de / N_de) * 100,
    per_de_ctr = (n_de_ctr / N_de_ctr) * 100
  ) |>
  # add code template for missing codes = 0 value
  full_join(tmp_data_de_cntry_code) |>
  # fill missing code_lex_name (join just by code)
  group_by(code_lex) |>
  fill(code_lex_name) |>
  ungroup() |>
  # add zero values for missing combinations of code and country/discourse
  mutate(across(n_de_ctr:per_de_ctr, ~ ifelse(is.na(.x), 0, .x))) |>
  mutate(
    code_lex_main = str_sub(code_lex, 1, 1) |> as.numeric(),
    color = case_when(
      country == "DE" ~ "#ED874E",
      country == "EN" ~ "#8da0cb",
      country == "FR" ~ "#66c2a5"
    )
  )

write.csv(data_code, "report/shiny/data/data_code.csv")

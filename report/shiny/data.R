library(tidyverse)
library(quanteda)
library(quanteda.textstats)


clr6 <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#e0b807")


# ---- 1. prep data ----
data_all <- read_csv("data/data_all.csv")


# 1.1 discourse event information
discourse_title <- read_csv2(
  file = "data/doc/discourse_events.csv",
  col_names = c("discourse", "title", "description")
)

tmp_discourse <- discourse_title$discourse
names(tmp_discourse) <- ifelse(
  !is.na(discourse_title$title),
  discourse_title$title,
  discourse_title$discourse
)

discourse_list <- list(
  "2021" = tmp_discourse[grepl("21", tmp_discourse)],
  "2022" = tmp_discourse[grepl("22", tmp_discourse)],
  "2023" = tmp_discourse[grepl("23", tmp_discourse)]
)



# add information about discourse for table header
data_de <- data_all |>
  distinct(country, document, comment_id, .keep_all = TRUE) |>
  summarize(
    date_min = min(source_date, na.rm = TRUE) |> format("%b %Y"),
    date_max = max(source_date, na.rm = TRUE) |> format("%b %Y"),
    cntrs = paste(unique(country), collapse = ", "),
    docs = n_distinct(document),
    srcs = n_distinct(source_outlet),
    comms = n(),
    comms_as = sum(str_detect(comment_ideation, "^I1"), na.rm = TRUE),
    comms_as_prop = round(comms_as / comms, 4) * 100,
    .by = discourse
  ) |>
  left_join(discourse_title)


# 1.2 lexicon codes labels
codes_list <- read_csv2(
  "data/doc/codes_lab_short.csv",
  col_names = c("code", "code_lab_short", "code_lab")
) |>
  mutate(
    code_main = str_sub(code, 1, 1) |> as.numeric(),
    code_clr = sapply(code_main, function(x) clr6[x]),
    Loc = code_main,
    ID = paste0(Loc, ".", code_lab_short)
  )


# 1.3 code data (proportion, code_group, colors)

# create template for all valid combinations of codes, country and discourse
tmp_data_de_cntry <- separate_rows(data_de, cntrs, sep = ", ") |>
  rename(country = cntrs) |>
  select(country, discourse)

tmp_data_de_cntry_code <-
  expand.grid(
    data_de$discourse,
    codes_list$code
  ) |>
  set_names(c("discourse", "code")) |>
  left_join(codes_list, by = "code") |>
  select(!c(Loc, ID)) |>
  left_join(
    tmp_data_de_cntry,
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
    per_de = round((n_de / N_de) * 100, 1),
    per_de_ctr = round((n_de_ctr / N_de_ctr) * 100, 1)
  ) |>
  rename(code = code_lex) |>
  select(!code_lex_name) |>
  # add code template for missing codes = 0 value
  full_join(tmp_data_de_cntry_code) |>
  # add zero values for missing combinations of code and country/discourse
  mutate(across(n_de_ctr:per_de_ctr, ~ ifelse(is.na(.x), 0, .x))) |>
  # create country color
  mutate(country_clr = sapply(as.factor(country), function(x) clr6[x]))



# 2. code network plot data

data_multicodes <- data_all |>
  select(country, discourse, document, comment_id, code_lex, code_lex_name) |>
  left_join(codes_list, by = c("code_lex_name" = "code_lab")) |>
  filter(!is.na(code_lex)) |>
  mutate(
    group = cur_group_id(),
    code_n = n(),
    .by = c(country, discourse, document, comment_id)
  ) |>
  filter(code_n > 1)


data_relations <- lapply(
  split(data_multicodes, data_multicodes$group), function(d) {
    do.call("rbind", combn(d$ID, 2, simplify = FALSE)) |> as.data.frame()
  }
) |>
  bind_rows(.id = "group") |>
  mutate(group = as.numeric(group)) |>
  left_join(
    distinct(data_multicodes, country, discourse, group),
    by = "group"
  ) |>
  filter(V1 != V2)



# prepare data for keyness and network plot

data_comments <- data_all |>
  distinct(country, document, comment_id, .keep_all = TRUE) |>
  select(country, discourse, comment_ideation, comment, document, comment_id) |>
  filter(!is.na(comment_ideation) & comment_ideation != "I2") |>
  mutate(
    antisemetic = str_detect(comment_ideation, "I0", TRUE),
    comment = str_remove_all(comment, "\\(reply:\\s.+?\\)"),
    comment = str_remove_all(comment, "http[s]+:.+?(\\s|$)"),
    comment = str_replace_all(comment, "(?!\\w)\\.(?=\\w)", " ")
  )


data_corpus <- corpus(data_comments, text_field = "comment")


tmp_ctr_discourses <- separate_rows(data_de, cntrs, sep = ", ") |>
  select(cntrs, discourse)


ctrs <- c("de" = "DE", "fr" = "FR", "en" = "UK")

ctrs_dscs <- lapply(ctrs, function(ctr) {
  tmp_ctr_discourses$discourse[tmp_ctr_discourses$cntrs == (ctr)]
})


data_dfm_keyw_ls <- lapply(names(ctrs), function(ctr) {
  ls <- lapply(ctrs_dscs[[ctr]], function(dsc) {
    message(ctr, " - ", dsc)

    dfm <- data_corpus |>
      corpus_subset(
        country == ctrs[ctr] & discourse == dsc,
        drop_docid = FALSE
      ) |>
      tokens(remove_punct = TRUE, remove_url = TRUE) |>
      tokens_remove(pattern = "Bild") |>
      tokens_keep(min_nchar = 2) |>
      tokens_tolower() |>
      tokens_remove(pattern = stopwords(ctr)) |>
      dfm()

    textst <- textstat_frequency(dfm) |> select(!c("rank", "group"))

    keywords <- dfm |>
      textstat_keyness(target = docvars(dfm, field = "antisemetic")) |>
      mutate(
        target = chi2 > 0,
        color = ifelse(target, clr6[2], clr6[1]),
        pos = ifelse(target == TRUE, row_number(), n() - row_number() + 1),
        emoji = iconv(feature, "latin1", "ASCII", sub = "") == "",
        text = str_c(
          "N (antisemitic): ", n_target, "\n",
          "N (non-antisemitic): ", n_reference
        )
      ) |>
      filter(pos < 300) |>
      left_join(textst, by = "feature")

    list("dfm" = dfm, "keywords" = keywords)
  })

  names(ls) <- ctrs_dscs[[ctr]]

  return(ls)
})

names(data_dfm_keyw_ls) <- ctrs


save(
  discourse_title, discourse_list, data_de, codes_list, data_code,
  data_multicodes, data_relations, data_dfm_keyw_ls,
  file = "report/shiny/data.Rdata"
)

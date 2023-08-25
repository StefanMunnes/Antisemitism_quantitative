pacman::p_load(
  "tidyverse", "xlsx"
)


# 1. import csv and create document and source related variables
source("scripts/01_create_document_data.R", echo = TRUE)


# 2. clean and filter code, extract metadata for comment and combine to data_all
source("scripts/02_create_code_data.R", echo = TRUE)


# 3. create and export all and level specific data

## 3.0 all comments and codes data
write_csv(data_all, "data/data_all.csv")

## 3.1 document data
data_documents <- data_all |>
  distinct(pick(
    country, discourse, document, source_type, source_outlet, source_date,
    source_title, source_comments_n, source_url, source_text
  ))

write_csv(data_documents, "data/data_level_documents.csv")

## 3.2 comments data
data_comments <- data_all |>
  distinct(pick(
    country, discourse, document, comment_id, comment, comment_user,
    comment_level, comment_date_time, comment_codes_all, comment_codes_n,
    comment_ideation
  ))

write_csv(data_comments, "data/data_level_comments.csv")

## 3.3 codes data
data_codes <- data_all |>
  distinct(pick(
    country, discourse, document, comment_id, code_main, code, code_name,
    code_created, code_system, code_orig, code_segment
  ))

write_csv(data_codes, "data/data_level_codes.csv")


save(
  data_all, data_documents, data_comments, data_codes,
  file = "data/all_files.RData"
)

load("data/all_files.RData")

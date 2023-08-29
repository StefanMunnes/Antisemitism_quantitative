# load necessary packages
pacman::p_load(
  "tidyverse", "xlsx"
)

# define global objects
file_check <- "data/tmp/codes_check.xlsx"



# ---- 1. scripts for different steps of data import, cleaning and creation ----

# 1.1 import csv and create document and source related variables
source("scripts/01_data_document.R", echo = TRUE)

# 1.2 clean and filter code variables (use external code system for validation)
source("scripts/02_data_code.R", echo = TRUE)

# 1.3 extract metadata for comment related variables
source("scripts/03_data_comment.R", echo = TRUE)

# 1.4 combine all code obs with cleaned comment variables to data_all
source("scripts/04_data_all.R", echo = TRUE)



# ---- 2. create and export all and level specific data ----

## 2.0 all comments and codes data
data_all <- readRDS("data/tmp/data_all.RDS") |>
  arrange(country, discourse, document, comment_id, code_main) |>
  # 3.5 select and order all important variables
  select(
    country, discourse, document, code_main, code, code_name, comment,
    comment_id, code_segment, comment_user, comment_level,
    comment_date_time, comment_codes_all, comment_codes_n, comment_ideation,
    code_created, code_system, code_orig,
    source_type, source_outlet, source_date, source_title, source_comments_n,
    source_comments_n, source_url, source_text
  )

write_csv(data_all, "data/data_all.csv")

## 2.1 document data
data_documents <- data_all |>
  distinct(pick(
    country, discourse, document, source_type, source_outlet, source_date,
    source_title, source_comments_n, source_url, source_text
  ))

write_csv(data_documents, "data/data_level_documents.csv")

## 2.2 comments data
data_comments <- data_all |>
  distinct(pick(
    country, discourse, document, comment_id, comment, comment_user,
    comment_level, comment_date_time, comment_codes_all, comment_codes_n,
    comment_ideation
  ))

write_csv(data_comments, "data/data_level_comments.csv")

## 2.3 codes data
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

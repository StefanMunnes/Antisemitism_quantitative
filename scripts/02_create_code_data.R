# ---- 1. create new variable code: extract from MAXQDA codesystem ----

## 1.1 load and prepare code system data (to merge with prepared MAXQDA data)
code_system <- read.csv2(
  "data/doc/code_system.csv",
  col.names = "code_system"
) |>
  mutate(
    # 1. extract (first) main category of code: first element
    code_main = str_split_i(code_system, "\\\\", 1),
    # 2. extract code: last element -> first element
    code = str_split_i(code_system, "\\\\", -1) |>
      str_split_i(" ", 1),
    # 3. extract name of code: last element -> after first element
    code_name = str_split_i(code_system, "\\\\", -1) |>
      str_split(" ", n = 2) |>
      sapply(last),
    .before = 1
  ) |>
  bind_rows()


## 1.2 prepare regex's for validation and extraction of minimal codes
regex_codes <- c(
  "RU-IL comparison",
  "Article",
  "FB( |-)[Pp]ost",
  "YT( |-)[Vv]ideo",
  "Tweet",
  "Example for illustration",
  "\\?\\?\\?",
  "Metadata"
) |> paste(collapse = "|")


## 1.3 create code: use long codes from MAXQDA output and extract last valid code
data_code <- data_document |>
  mutate(
    # 1.3.1 extract short codes that are in the last part of long nested codes
    code = str_split_i(code_orig, "\\\\", -1) |>
      str_split_i(" ", 1),
    # 1.3.2 change typos in codes
    code = case_when(
      code == "I0c/CS" ~ "I0c",
      code == "3b)" ~ "OA",
      code == "(D10a" ~ "D10a",
      .default = code
    ),
    # 1.3.3 keep just valid extractions
    code = if_else(
      str_detect(
        code,
        paste0("^(", paste(code_system$code, collapse = "|"), ")$")
      ),
      code,
      NA
    ),
    # 1.3.4 add special codes
    code = if_else(
      is.na(code) & str_detect(code_orig, paste0(".*(", regex_codes, ").*")),
      str_extract(code_orig, regex_codes),
      code
    )
  ) |>
  # 1.3.5 add more information from prepared external code system
  left_join(code_system, by = "code")


## temporary output files to check if code extraction was correct
file_check <- "data/tmp/codes_check.xlsx"

filter(data_code, !is.na(code)) |>
  count(code_orig, code, code_name, code_main, code_system) |>
  arrange(code_orig, code, code_name, code_main, code_system) |>
  write.xlsx(file_check, "codes_final", row.names = FALSE)

filter(data_code, is.na(code)) |>
  count(code_orig) |>
  arrange(desc(n)) |>
  write.xlsx(file_check, "codes_remove", append = TRUE, row.names = FALSE)



# ---- 2. create comment-wise data and variables from metadata ----

## each comment coded with two important main codes:
## 1. metadata (just first part) (given every time)
## 2. ideation (whole comment): I0, I0c, I1, I0ASC (given every time)

data_comment <- data_code |>
  # 2.1 filter just metadata and ideation coded comments
  filter(code_main %in% c("Metadata", "1) Ideation")) |>
  # 2.2 create helper variable for wrong double coding of ideations
  arrange(document, code_start, code) |> # get right order for collapse
  mutate(
    CodesComb = paste(code, collapse = ";"), # combine all code per comment
    .by = c(document, code_start)
  ) |>
  # 2.3 remove duplicates (most I1ASC & I1a, some I1 & I0)
  distinct(document, code_start, code_main, .keep_all = TRUE) |>
  # 2.4 transform to wide: two columns: one for metadata, one for whole comment
  pivot_wider(
    id_cols = c(country, discourse, document, code_start),
    names_from = code_main,
    values_from = comment_code_segment,
    unused_fn = list(CodesComb = first, code_end = max)
  ) |>
  rename(Ideation = "1) Ideation") |>
  mutate(
    # 2.5 get comment (remove metadata block from full segment)
    comment =
      str_remove(
        Ideation, # Ideation = whole segment (includes metadata)
        Metadata |> # metadata = just first part
          str_replace_all("(\\W)", "\\\\\\1?") |> # escape special characters
          # first and last part flexible (coding error, not all marked)
          str_replace("^.{4,4}", "^.*") |>
          # str_replace("(.{4,4}$)", ".*?") |>
          paste0("\\s*\\n?") # add space and linebreak to remove (if comment)
      ),
    # 2.6 get different information from metadata (doesn't work for YT)
    comment_user = str_extract(Metadata, "by (.*?) (?:\\([0-9]+ up|- lev)", 1),
    comment_level = str_extract(Metadata, "level_([0-9]+)", 1) |> as.numeric(),
    comment_date_time = str_extract(Metadata, "[0-9\\-:, ]{15,17}") |> ymd_hm()
  ) |>
  # 2.6 create unique comment id (per document)
  mutate(comment_id = row_number(), .by = c(document))


## check wrong or missing ideations/metadata
as.data.frame(data_comment) |>
  # multiple metadata per End -> creates error for unique join with full data
  mutate(wrong_codings = n() > 1, .by = c("document", "code_end")) |>
  mutate(
    # duplicates of ideation (except I1ASC + I1a)
    dup_ideation = str_count(CodesComb, "I") > 1 &
      !str_detect(CodesComb, "I1ASC.+I1a"),
    # empty or to short ideation coding
    no_ideation = is.na(Ideation) | str_length(Ideation) < 3,
    # empty or to short metadata coding (to short for YT export)
    no_metadata = is.na(Metadata) | str_length(Metadata) < 6
  ) |>
  filter(wrong_codings | dup_ideation | no_metadata | no_ideation) |>
  select(
    country, discourse, document,
    wrong_codings, dup_ideation, no_ideation, no_metadata,
    code_start, code_end, CodesComb, Metadata, Ideation
  ) |>
  arrange(country, discourse, document, code_start) |>
  write.xlsx(file_check, "codes_missing_error", append = TRUE, row.names = FALSE)



# ---- 3. final data: combine comment-wise with full code data ----
data_all <- data_comment |>
  # ! 3.1 filter if not unique: wrong codings with same end (7 cases -> 14 dups)
  filter(
    n() == 1 | (str_detect(CodesComb, "I") & str_detect(CodesComb, "Meta")),
    .by = c("document", "code_end")
  ) |>
  # 3.2 join comment-wise with code data (choose just missing variables)
  select(!c(Ideation, Metadata, CodesComb, country, discourse)) |>
  full_join(
    select(data_code, !c(comment, code_start)),
    by = c("document", "code_end"),
    relationship = "one-to-many"
  ) |>
  # 3.3 remove metadata and missing codes (see validation in 1.3.3)
  filter(code != "Metadata", !is.na(code)) |>
  # create
  mutate(
    comment_codes_all = unique(code) |> paste(collapse = ", "),
    comment_codes_n = n(),
    .by = c(document, comment_id)
  )
# |>
# # 3.4 select and order all important variables
# select(
#   country, discourse, document, code_main, code, code_name, comment,
#   comment_id, comment_code_segment, comment_user, comment_level,
#   comment_date_time, comment_codes_all, comment_codes_n, code_created,
#   code_system, code_orig, source_type, source_outlet, source_date,
#   source_title, source_comments_n, source_comments_n, source_url, source_text
# )

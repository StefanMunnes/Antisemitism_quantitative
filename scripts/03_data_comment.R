## create comment-wise data and variables from metadata ##

## each comment coded with two important main codes:
## 1. metadata (just first part) (given every time)
## 2. ideation (whole comment): I0, I0c, I1, I0ASC (given every time)

# ---- 1. keep just meta & ideation and transform to wide -----
data_comment_raw <- readRDS("data/tmp/data_code.RDS") |>
  # 1.1 filter just metadata and ideation coded comments (removes if both miss)
  filter(code_main %in% c("Metadata", "1) Ideation")) |>
  # 1.2 create helper variable for wrong double coding of ideations
  arrange(document, code_start, code) |> # get right order for collapse
  mutate(
    CodesComb = paste(code, collapse = ";"), # combine all code per comment
    .by = c(document, code_start)
  ) |>
  # 1.3 remove duplicates (most I1ASC & I1a, some I1 & I0)
  distinct(document, code_start, code_main, .keep_all = TRUE) |>
  # 1.4 correct ideation without metadata (start not overlapping with metadata)
  arrange(document, code_end, desc(code_main)) |>
  mutate(code_start = ifelse(
    code_main == "1) Ideation" & lag(code) == "Metadata" & # just ideation
      code_start == code_end & code_start - lag(code_start) == 1, # starts later
    code_start - 1,
    code_start
  )) |>
  # 1.5 transform to wide: two columns: one for metadata, one for whole comment
  pivot_wider(
    id_cols = c(country, discourse, document, code_start),
    names_from = code_main,
    values_from = code_segment,
    # how to deal with variables to keep
    unused_fn = list(
      CodesComb = first,
      code_end = max,
      source_crawl_date = first
    )
  ) |>
  rename(Ideation = "1) Ideation")



# ---- 2. clean and create comment related variables from metadata -----
data_comment <- data_comment_raw |>
  mutate(
    # 2.1 get comment (remove metadata block from full segment)
    comment =
      str_remove(
        Ideation, # Ideation = whole segment (includes metadata)
        Metadata |> # metadata = just first part
          str_replace_all("(\\W)", "\\\\\\1?") |> # escape special characters
          # first and last part flexible (coding error, not all marked)
          str_replace("^.{4,4}", "^.*") |>
          # str_replace("(.{4,4}$)", ".*?") |>
          paste0(":+\\s*\\n*") # add space and linebreak to remove (if comment)
      ),
    # 2.2 get different information from metadata (doesn't work for YT)
    comment_user = str_extract(Metadata, "by (.*?) (\\([0-9]+ |- level)", 1),
    comment_user = str_remove(comment_user, "\ufffc"),
    comment_level = str_extract(Metadata, "level_([0-9]+)", 1) |> as.numeric(),
    # 2.3 extract & calculate date of comment from scrape date and metadata
    tmp = str_extract(Metadata, '^\"(.+?)\"', 1),
    comment_date_time = case_when(
      is.na(tmp) ~ str_extract(Metadata, "^[0-9\\-:, ]{15,17}") |> ymd_hm(),
      str_detect(tmp, "Std") ~
        source_crawl_date - hours(str_extract(tmp, "[0-9]+")),
      str_detect(tmp, "Tag|[dD]ay") ~
        source_crawl_date - days(str_extract(tmp, "[0-9]+")),
      str_detect(tmp, "Wo\\.") ~
        source_crawl_date - weeks(str_extract(tmp, "[0-9]+")),
      str_detect(tmp, "Monat") ~
        source_crawl_date - (weeks(str_extract(tmp, "[0-9]+")) * 4),
      str_detect(tmp, "J\\.") ~
        source_crawl_date - years(str_extract(tmp, "[0-9]+")),
      !is.na(tmp) ~ paste(tmp, str_extract(source_crawl_date, "^[0-9]{4,4}")) |>
        dmy(),
      .default = NA
    )
  ) |>
  # 2.4 mark user names in comments for @-reply (easy to remove)
  mutate(
    tmp = unique(comment_user) |>
      str_replace_all("(\\W)", "\\\\\\1") |>
      paste(collapse = "|"),
    comment = str_replace(comment, paste0("^(", tmp, ")"), "\\(reply: \\1\\)"),
    .by = document
  ) |>
  select(!tmp) |>
  # 2.5 create unique comment id (per document)
  mutate(comment_id = row_number(), .by = c(document))

saveRDS(data_comment, "data/tmp/data_comment.RDS")



# ---- 3. check wrong or missing ideations/metadata ----
as.data.frame(data_comment) |>
  # multiple metadata per End -> creates error for unique join with full data
  mutate(wrong_codings = n() > 1, .by = c("document", "code_end")) |>
  mutate(
    # duplicates of ideation (except I1ASC + I1a)
    dup_idea = str_count(CodesComb, "I") > 1 &
      !str_detect(CodesComb, "I1ASC.+I1a"),
    # filter by CodesComb, b/c error handling or short ideation text
    no_idea = CodesComb == "Metadata" | str_length(Ideation) < 3,
    # empty or to short metadata coding (to short for YT export)
    no_meta = is.na(Metadata) | str_length(Metadata) < 6,
    # multi comment coding
    multi_com = code_end - code_start > 2
  ) |>
  filter(wrong_codings | dup_idea | no_meta | no_idea | multi_com) |>
  select(
    country, discourse, document,
    wrong_codings, dup_idea, no_idea, no_meta, multi_com,
    code_start, code_end, CodesComb, Metadata, Ideation
  ) |>
  arrange(country, discourse, document, code_start) |>
  write.xlsx(
    file_check, "codes_missing_error",
    append = TRUE, row.names = FALSE
  )

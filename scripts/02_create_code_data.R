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
data_code <- readRDS("data/tmp/data_document.RDS") |>
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
  # 2.1 filter just metadata and ideation coded comments (removes if both miss)
  filter(code_main %in% c("Metadata", "1) Ideation")) |>
  # 2.2 create helper variable for wrong double coding of ideations
  arrange(document, code_start, code) |> # get right order for collapse
  mutate(
    CodesComb = paste(code, collapse = ";"), # combine all code per comment
    .by = c(document, code_start)
  ) |>
  # 2.3 remove duplicates (most I1ASC & I1a, some I1 & I0)
  distinct(document, code_start, code_main, .keep_all = TRUE) |>
  # correct ideation without metadata (start not overlapping with metadata)
  arrange(document, code_end, desc(code_main)) |>
  mutate(code_start = ifelse(
    code_main == "1) Ideation" & lag(code) == "Metadata" & # just ideation
      code_start == code_end & code_start - lag(code_start) == 1, # starts later
    code_start - 1,
    code_start
  )) |>
  # 2.4 transform to wide: two columns: one for metadata, one for whole comment
  pivot_wider(
    id_cols = c(country, discourse, document, code_start),
    names_from = code_main,
    values_from = code_segment,
    unused_fn = list(
      CodesComb = first,
      code_end = max,
      source_crawl_date = first
    )
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
          paste0(":+\\s*\\n*") # add space and linebreak to remove (if comment)
      ),
    # 2.6 get different information from metadata (doesn't work for YT)
    comment_user = str_extract(Metadata, "by (.*?) (?:\\([0-9]+ )", 1),
    comment_user = str_remove(comment_user, "\ufffc"),
    comment_level = str_extract(Metadata, "level_([0-9]+)", 1) |> as.numeric(),
    # extract & calculate date of comment from scrape date and metadata
    tmp = str_extract(Metadata, '^\"(.+?)\"', 1),
    comment_date = case_when(
      is.na(tmp) ~ str_extract(Metadata, "^[0-9\\-:, ]{15,17}") |>
        ymd_hm(),
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
  select(!tmp) |>
  # 2.6 create unique comment id (per document)
  mutate(comment_id = row_number(), .by = c(document))


## check wrong or missing ideations/metadata
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
  write.xlsx(file_check, "codes_missing_error", append = TRUE, row.names = FALSE)



# ---- 3. combine comment-wise with full code data ----
data_combined <- data_comment |>
  # ! 3.1 filter if not unique: wrong codings with same end (7 cases -> 14 dups)
  filter(
    n() == 1 | (str_detect(CodesComb, "I") & str_detect(CodesComb, "Meta")),
    .by = c("document", "code_end")
  ) |>
  # 3.2 correct wrong end for comments w\ missing ideation (for joining by end)
  # BUT: deal with cases where it would result in two identical endings
  mutate(code_end = ifelse(
    code_start == code_end & is.na(Ideation) &
      lag(code_end) - code_end != 1 & lead(code_end) - code_end != 1,
    code_end + 1,
    code_end
  )) |>
  # 3.3 join comment-wise with code data (choose just missing variables)
  select(!c(Ideation, Metadata, CodesComb, country, discourse)) |>
  full_join(
    select(data_code, !c(comment, code_start)),
    by = c("document", "code_end"),
    relationship = "one-to-many"
  ) |>
  # 3.4 remove metadata and missing codes (see validation in 1.3.3)
  filter(code != "Metadata", !is.na(code))



# ---- 4. extract incomplete information ----
###  comments located between start and end, didn't merge with comment data

# 4.1 loop over documents and extract incomplete codes (comment_id missing)
data_combined_incom <- lapply(unique(data_combined$document), function(doc) {
  data <- filter(data_combined, document == doc)
  mis <- unique(data$code_end[is.na(data$comment_id)])

  if (length(mis) == 0) { # end loop if no incomplete codes
    return(NULL)
  } else {
    message(doc)

    # 4.2 otherwise: loop over unique missing code_end and get comment data
    matching <- lapply(mis, function(x) {
      filter(data, code_start <= x, code_end >= x) |>
        select(comment_id, comment_user, comment_level, comment_date_time) |>
        first()
    }) |>
      bind_rows() |>
      mutate(code_end = mis, document = doc) |>
      rename_with(~ paste0(.x, "_add"), starts_with("comment"))

    return(matching)
  }
}) |>
  bind_rows()



# ---- 5. create finale (all) data; add comment wise data; clean all text ----
data_all <-
  # 5.1 add incomplete information to main data
  left_join( # keep just valid (part of all combined data)
    data_combined, data_combined_incom,
    by = c("document", "code_end")
  ) |>
  mutate(comment_id = ifelse(is.na(comment_id), comment_id_add, comment_id)) |>
  mutate(
    across(
      c(comment_user, comment_level, comment_date_time),
      ~ first(.x, na_rm = TRUE)
    ),
    .by = c("document", "comment_id")
  ) |>
  # 5.2 create variable with all codes per comment, count, and ideation variable
  mutate(
    comment_codes_all = unique(code) |> paste(collapse = ", "),
    comment_codes_n = n(),
    comment_ideation = case_when(
      str_count(comment_codes_all, "I[012]") == 1 ~
        str_extract(comment_codes_all, "I[012].*?\\b"),
      str_detect(comment_codes_all, "I1ASC") &
        str_detect(comment_codes_all, "I1a") ~ "I1a",
      .default = NA
    ),
    .by = c(document, comment_id)
  ) |>
  # 5.3 clean comment and segment variable (unicode placeholder and metadata)
  mutate(
    comment = str_remove_all(comment, "\ufffc") |> str_squish(),
    code_segment = str_remove(code_segment, "(^.+?level_.:\\s*\\n*)*") |>
      str_remove_all("\ufffc") |> str_squish(),
    code_segment_len = str_length(code_segment) # helper variable for 5.4
  ) |>
  # 5.4 extract comment from segment (e.g. ideation code was missing (= NA))
  group_by(document, comment_id) |>
  arrange(desc(code_segment_len)) |>
  mutate(
    comment = ifelse(
      is.na(comment), # & !str_detect(comment_codes_all, "I[012]")
      first(code_segment),
      comment
    )
  ) |>
  ungroup() |>
  arrange(country, discourse, document, comment_id, code_main) |>
  # 5.5 select and order all important variables
  select(
    country, discourse, document, code_main, code, code_name, comment,
    comment_id, code_segment, comment_user, comment_level,
    comment_date_time, comment_codes_all, comment_codes_n, comment_ideation,
    code_created, code_system, code_orig,
    source_type, source_outlet, source_date, source_title, source_comments_n,
    source_comments_n, source_url, source_text
  )

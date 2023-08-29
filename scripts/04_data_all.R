# ---- 1. combine comment-wise with full code data ----
data_combined <- readRDS("data/tmp/data_comment.RDS") |>
  # ! 1.1 filter if not unique: wrong codings with same end (7 cases -> 14 dups)
  filter(
    n() == 1 | (str_detect(CodesComb, "I") & str_detect(CodesComb, "Meta")),
    .by = c("document", "code_end")
  ) |>
  # 1.2 correct wrong end for comments w\ missing ideation (for joining by end)
  # BUT: deal with cases where it would result in two identical endings
  mutate(code_end = ifelse(
    code_start == code_end & is.na(Ideation) &
      lag(code_end) - code_end != 1 & lead(code_end) - code_end != 1,
    code_end + 1,
    code_end
  )) |>
  # 1.3 join comment-wise with code data (choose just missing variables)
  select(!c(Ideation, Metadata, CodesComb, country, discourse)) |>
  full_join(
    select(readRDS("data/tmp/data_code.RDS"), !c(comment, code_start)),
    by = c("document", "code_end"),
    relationship = "one-to-many"
  ) |>
  # 1.4 remove metadata and missing codes (see validation in 1.3.3)
  filter(code != "Metadata", !is.na(code))



# ---- 2. extract incomplete information ----
###  comments located between start and end, didn't merge with comment data

# 2.1 loop over documents and extract incomplete codes (comment_id missing)
data_combined_incomp <- lapply(unique(data_combined$document), function(doc) {
  data <- filter(data_combined, document == doc)
  mis <- unique(data$code_end[is.na(data$comment_id)])

  if (length(mis) == 0) { # end loop if no incomplete codes
    return(NULL)
  } else {
    message(doc)

    # 2.2 otherwise: loop over unique missing code_end and get comment data
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



# ---- 3. create finale (all) data; add comment wise data; clean all text ----
data_all <-
  # 3.1 add incomplete information to main data
  left_join( # keep just valid (part of all combined data)
    data_combined, data_combined_incomp,
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
  # 3.2 create variable with all codes per comment, count, and ideation variable
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
  # 3.3 clean comment and segment variable (Unicode placeholder and metadata)
  mutate(
    comment = str_remove_all(comment, "\ufffc") |> str_squish(),
    code_segment = str_remove(code_segment, "(^.+?level_.:\\s*\\n*)*") |>
      str_remove_all("\ufffc") |> str_squish(),
    code_segment_len = str_length(code_segment) # helper variable for 3.4
  ) |>
  # 3.4 extract comment from segment (e.g. ideation code was missing (= NA))
  group_by(document, comment_id) |>
  arrange(desc(code_segment_len)) |>
  mutate(
    comment = ifelse(
      is.na(comment), # & !str_detect(comment_codes_all, "I[012]")
      first(code_segment),
      comment
    )
  ) |>
  ungroup()

saveRDS(data_all, "data/tmp/data_all.RDS")

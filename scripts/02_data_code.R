# ---- create new variable code: extract from MAXQDA codesystem ----

## 1 load and prepare code system data (to merge with prepared MAXQDA data)
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


## 2. prepare regex's for validation and extraction of minimal codes
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


## 3. create code: use long codes from MAXQDA output & extract last valid code
data_code <- readRDS("data/tmp/data_document.RDS") |>
  mutate(
    # 3.1 extract short codes that are in the last part of long nested codes
    code = str_split_i(code_orig, "\\\\", -1) |>
      str_split_i(" ", 1),
    # 3.2 change typos in codes
    code = case_when(
      code == "I0c/CS" ~ "I0c",
      code == "3b)" ~ "OA",
      code == "(D10a" ~ "D10a",
      .default = code
    ),
    # 3.3 remove wrong old code (new code added, old was private code)
    code = ifelse(str_detect(code_orig, "L15 Self-reference"), "L15old", code),
    # 3.4 keep just valid extractions
    code = if_else(
      str_detect(
        code,
        paste0("^(", paste(code_system$code, collapse = "|"), ")$")
      ),
      code,
      NA
    ),
    # 3.5 add special codes
    code = if_else(
      is.na(code) & str_detect(code_orig, paste0(".*(", regex_codes, ").*")),
      str_extract(code_orig, regex_codes),
      code
    )
  ) |>
  # 3.6 add more information from prepared external code system
  left_join(code_system, by = "code")

saveRDS(data_code, "data/tmp/data_code.RDS")


## 4. temporary output files to check if code extraction was correct
filter(data_code, !is.na(code)) |>
  count(code_orig, code, code_name, code_main, code_system) |>
  arrange(code_orig, code, code_name, code_main, code_system) |>
  write.xlsx(file_check, "codes_final", row.names = FALSE)

filter(data_code, is.na(code)) |>
  count(code_orig) |>
  arrange(desc(n)) |>
  write.xlsx(file_check, "codes_remove", append = TRUE, row.names = FALSE)

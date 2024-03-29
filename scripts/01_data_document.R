# ---- 1. import all csv (exported from MAXQDA) files and create raw data ----

## 1.1 get vector with all csv files (including country subfolder)
file_paths <- list.files(
  path = "data/raw",
  pattern = ".csv",
  full.names = TRUE,
  recursive = TRUE
)

## 1.2 loop over every csv file, create document variables and combine
data_raw <- lapply(file_paths, function(path) {
  # 1.2.1 import csv and create path variabel
  file <- tryCatch(
    {
      read.csv(path, header = TRUE, encoding = "UTF-8") |> mutate(path = path)
    },
    warning = function(w) {
      stop("\nImport warning: ", conditionMessage(w), "\n", path)
    }
  )

  # 1.2.2 create message for imported file and size
  message(
    str_extract(path, "((DE|FR|UK)/.+)\\.csv", 1), ": ", nrow(file), " rows"
  )

  # 1.2.3 for YT-Export: transform wrong Beginning/End, e.g. 2/20 -> 20 (num.)
  if (class(file$Beginning) == "character") {
    file <- file |>
      mutate(
        across(
          c(Beginning, End),
          ~ str_extract(.x, "\\|?([0-9]+$)", group = 1) |> as.numeric()
        )
      )
  }

  return(file)
}) |>
  # 1.2.4 combine all unique country/discourse_events to one data.frame
  bind_rows()



# ---- 2. create document vars: country, discourse, source outlet/type/text ----
data_document <- data_raw |>
  # 2.1 rename to lower and new, more systematical names
  rename(
    document = Document.name,
    code_orig = Code,
    code_start = Beginning,
    code_end = End,
    code_created = Created,
    code_segment = Segment,
    comment = Comment
  ) |>
  # 2.2 keep and order important variables
  select(document, starts_with("code_"), starts_with("comment"), path) |>
  # 2.3 create basic document variables
  mutate(
    country = str_extract(path, "(/)(DE|FR|UK)(/)", group = 2),
    discourse = basename(gsub(".csv", "", path)),
    # combine names of two separate files for UK/23_AIC_1/2
    discourse = ifelse(str_detect(discourse, "23_AIC"), "23_AIC", discourse),
    code_created = dmy_hms(code_created)
  ) |>
  # 2.4 create outlet variable from document name
  mutate(
    # 2.4.1 extract & clean outlet from document name (first part, after source)
    source_outlet = str_extract(document, "^(.*?)_", group = 1) |>
      str_remove("^(FB-|TWITT-|YT-|IN-)") |> # remove prefix of source_type
      str_replace("^(BBC).*", "\\1") |> # transform multiple variations of BBC
      str_replace("S.{1,2}DDE", "SUDDE") |> # transform Süddeu. -> error with Ü
      str_replace_all( # correct multiple typos at once
        c(
          "DERSP" = "SPIEG", "LCI.F" = "LCI", "N-TV." = "NTV",
          "MIRROR" = "MIRRO", "TF1IN" = "TF1"
        )
      ) |>
      # replace important Twitter names to just first letter capital
      str_replace_all(c("AMRO" = "Amro", "YARAH" = "Yara")),
    # 2.4.2 extract outlet from twitter document names
    source_outlet = case_when(
      str_detect(document, "_BBC_") ~ "BBC",
      str_detect(document, "_PERMA_") ~ "BBC",
      str_detect(document, "_Guardian_") ~ "GUARD",
      str_detect(document, "_Spectator_") ~ "SPECT",
      str_detect(document, "_Figaro_") ~ "LEFIG",
      str_detect(document, "_franceinfo_") ~ "FRANC",
      str_detect(document, "_Lib.ration_") ~ "LIBER",
      str_detect(document, "_Monde_") ~ "MONDE",
      str_detect(document, "_Parisien_") ~ "LEPA",
      str_detect(document, "_Valeurs_") ~ "VALEU",
      str_detect(document, "_BILD_") ~ "BILD",
      str_detect(document, "_Frankfurter_") ~ "FRANK",
      str_detect(document, "_ntv_") ~ "NTV",
      str_detect(document, "_SPIEGEL_") ~ "SPIEG",
      str_detect(document, "_WELT_") ~ "WELT",
      # str_detect(document, "_FRIE_") ~ "FRIE",
      # str_detect(document, "_AMRO_") ~ "AMRO",
      str_detect(document, "FRANCE 24") ~ "FR24",
      .default = source_outlet
    ),
    source_outlet = na_if(source_outlet, "TWITT"), # rest of twitter to NA
    # 2.5.1 extract date of publication
    source_date = str_extract(document, "[0-9]{8,8}") |> ymd() |> as_date(),
    # 2.5.2 wrong date to NA (UK/23_AIC)
    source_date = ifelse(source_date == "2021-04-18", NA, source_date) |>
      as_date()
  ) |>
  # 2.6.1 create source(type) from code_orig (first row with trigger)
  mutate(
    # correct wrong discourse trigger codes
    code_orig = case_when(
      str_detect(code_orig, "FB[\\- ][Pp]ost$") ~ "FB-post",
      str_detect(code_orig, "YT[\\- ][Vv]ideo$") ~ "YT-video",
      str_detect(code_orig, "Insta(gram)*[\\- ][Pp]ost$") ~ "Instagram",
      .default = code_orig
    ),
    # match trigger code for source_type variable and set for whole document
    source_type = case_when(
      str_detect(code_orig, "FB-post$") ~ "Facebook",
      str_detect(code_orig, "Tweet$") ~ "Twitter",
      str_detect(code_orig, "Article$") ~ "Newspage",
      str_detect(code_orig, "YT-video$") ~ "YouTube",
      code_orig == "Instagram" ~ "Instagram"
    ),
    source_text = ifelse(!is.na(source_type), code_segment, NA) |>
      first(na_rm = TRUE),
    source_type = first(source_type, na_rm = TRUE),
    .by = document
  ) |>
  # extract further source related information from raw crawler text
  mutate(
    source_url = str_extract(source_text, "(http.+?)(\\n| )", 1),
    source_comments_n = str_extract(source_text, "([0-9]{1,5}) Comments", 1),
    source_crawl_date = str_extract(
      source_text, "Crawling:.+?([0-9.]{10,10})",
      group = 1
    ) |>
      dmy() |>
      as_date(),
    source_title = str_split(source_text, "\\n") |> # split on space
      sapply(first),
    source_title =
      case_when(
        str_detect(source_text, "^Facebook-Posts") ~ NA,
        source_type == "YouTube" ~ NA,
        source_type == "Twitter" ~ str_remove(source_title, "^.+?: "),
        .default = source_title
      )
  ) |>
  # 2.6.2 remove (first) discourse trigger row and not valid Source
  filter(
    !str_detect(code_orig, "(FB-post|Twitter|Article|YT-video|Instagram)$")
  )

saveRDS(data_document, "data/tmp/data_document.RDS")

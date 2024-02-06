fnct_data <- function(de_in, code_in, cntry_in, freq_in) {
  # filter lexicon code list to label factor just for values
  fct_labs <- codes_list |>
    filter(.data$code_main %in% code_in)

  # filter for discourse event and code group
  data <- filter(
    data_code,
    .data$discourse == de_in,
    .data$code_main %in% code_in
  ) |>
    # add factor label for lexicon codes
    mutate(code_fct = ordered(
      .data$code,
      levels = fct_labs$code,
      labels = fct_labs$code_lab_short
    ))

  # create freq OR prop variables for overall discourse or separated by country
  if (cntry_in && freq_in) {
    data$value <- data$n_de_ctr
  }
  if (cntry_in && !freq_in) {
    data$value <- data$per_de_ctr
  }
  if (!cntry_in && freq_in) {
    data$value <- data$n_de
    data <- distinct(
      data, .data$discourse, .data$code_fct, .data$value
    )
  }
  if (!cntry_in && !freq_in) {
    data$value <- data$per_de
    data <- distinct(
      data, .data$discourse, .data$code_fct, .data$value
    )
  }

  return(data)
}


# create keyword data from prepared data and filter by input (create new pos)
fct_keyw_data <- function(
    discourse, country = input$keyw_cntry_in,
    ref = input$keyw_reference, min = input$keyw_min_slider,
    emoji = input$keyw_emoji, max = input$keyw_num_slider) {
  data <- data_dfm_keyw_ls[[country]][[discourse]][["keywords"]]

  # keep just target (antisemitic) keywords if input for reference is false
  if (ref == FALSE) data <- filter(data, .data$target == TRUE)

  # filter by minimum number of absolut observations (default = 5)
  data <- data |>
    filter(.data$docfreq >= min) |>
    mutate(pos = ifelse(
      .data$target == TRUE, row_number(), n() - row_number() + 1
    ))

  # filter emojis if input is FALSE
  if (emoji == FALSE) {
    data <- data |>
      filter(.data$emoji == FALSE) |>
      mutate(pos = ifelse(
        .data$target == TRUE, row_number(), n() - row_number() + 1
      ))
  }

  # filter by max number of keywords (via input slider)
  data <- filter(data, .data$pos <= max)

  return(data)
}


# small rescale function to normalize size and width between two values
rescale <- function(x, min, max) {
  x <- (x - min(x)) / (max(x) - min(x))
  x <- min + x * (max - min)
  return(x)
}


# create network data (data.frame of nodes and edges in list)
fct_graph_data <- function(graph, keyw_data, top) {
  nodes_df <- data.frame(
    id = V(graph)$name,
    keyword = V(graph)$name
  ) |>
    left_join(
      keyw_data[c("feature", "docfreq", "color")],
      by = c("keyword" = "feature")
    ) |>
    mutate(
      size = rescale(docfreq, 10, 30),
      font.size = size * 1.5 + 15
    ) |>
    arrange(keyword) # sort alphabetically

  edges_df <- graph |>
    get.edgelist() |>
    as.data.frame() |>
    setNames(c("from", "to")) |>
    summarize(width = n(), .by = c("from", "to")) |>
    mutate(width = rescale(width, 2, 20)) |>
    arrange(desc(width)) |>
    mutate(row_per = (row_number() / n()) * 100) |>
    mutate(row_per_max = max(row_per) |> round(0), .by = width) |>
    filter(row_per_max <= top)

  list("nodes" = nodes_df, "edges" = edges_df)
}


# network plot: nodes & edges data as input and different predefined options
fct_visNet <- function(nodes_df, edges_df) {
  visNetwork(nodes_df, edges_df) |>
    visIgraphLayout(
      layout = "layout.kamada.kawai", # "layout.fruchterman.reingold",
      randomSeed = 161
    ) |>
    visOptions(
      highlightNearest = list(
        enabled = TRUE, hover = TRUE, hideColor = "#9c9c9c",
        algorithm = "all", labelOnly = FALSE, degree = 1
      ),
      selectedBy = "keyword"
      # nodesIdSelection = list(
      #   enabled = TRUE
      # )
    ) |>
    visInteraction(
      hideEdgesOnDrag = TRUE,
      # navigationButtons = TRUE,
      tooltipDelay = 300
    ) |>
    visNodes(
      # color.highlight.background = "red",
      physics = FALSE,
      font = list(strokeWidth = 7),
      shadow = list(enabled = TRUE, size = 10)
    ) |>
    visEdges(
      color = list(
        color = "#7293ff",
        highlight = "#db1a1a",
        hover = "#f14646",
        opacity = 0.6
      ),
      hoverWidth = 2,
      smooth = list(enabled = TRUE, type = "continious")
    )
}
fct_de_text <- function(data, discourse) {
  x <- data[data$discourse == discourse, ]

  line1 <- paste0("<strong>", x$description, "</strong>")
  line2 <- paste(
    "From", x$date_min, "to", x$date_max,
    "in", stringr::str_count(x$cntrs, "\\S+"),
    ifelse(stringr::str_count(x$cntrs, "\\S+") == 1, "country:", "countries:"),
    x$cntrs
  )
  line3 <- paste(
    x$docs, "discussion threats from",
    x$srcs, "unique sources."
  )
  line4 <- paste0(
    x$comms, " overall comments, ",
    x$comms_as, " (", x$comms_as_prop, "%)", "labelled as antisemitic."
  )

  shiny::HTML(paste(line1, line2, line3, line4, sep = "<br/>"))
}


fct_code_data <- function(data_a, data_b, de_in, code_in, cntry_in, freq_in) {
  # filter lexicon code list to label factor just for values
  fct_labs <- data_a |>
    filter(.data$code_main %in% code_in)

  # filter for discourse event and code group
  data <- filter(
    data_b,
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


fct_code_freq_plot <- function(data, country, dot, freq, font) {
  if (country) {
    colors_cntry <- data |>
      dplyr::arrange(.data$country) |>
      dplyr::distinct(.data$country_clr) |>
      dplyr::pull()

    plot <- plot_ly(data,
      x = ~value, y = ~code_fct,
      color = ~country, colors = colors_cntry
    )
  } else {
    plot <- plot_ly(data, x = ~value, y = ~code_fct)
  }

  if (dot) {
    plot <- add_markers(plot, size = 5, opacity = 0.8)
  } else {
    plot <- add_bars(plot)
  }

  plot <- layout(plot,
    yaxis = list(title = "", autorange = "reversed"),
    showlegend = TRUE, font = list(family = font)
  )

  if (!country) {
    plot <- layout(plot, showlegend = FALSE)
  }

  if (freq) {
    plot <- layout(plot, xaxis = list(title = "Frequency"))
  } else {
    plot <- layout(plot, xaxis = list(title = "Percentage", ticksuffix = "%"))
  }

  return(plot)
}


fct_code_net_plot <- function(data, country, discourse, codes, color) {
  graph <- data[
    data$country %in% country & data$discourse == discourse,
    c("V1", "V2")
  ] |>
    igraph::graph_from_data_frame(
      vertices = codes["ID"],
      directed = FALSE
    )

  V(graph)$size <- igraph::degree(graph) * 10

  if (color) V(graph)$color <- codes$code_clr

  plot <- edgebundleR::edgebundle(graph,
    fontsize = 12, padding = 125, cutoff = 0, width = 800,
    tension = 0.7, nodesize = c(0, 30)
  )

  return(plot)
}



# create keyword data from prepared data and filter by input (create new pos)
fct_keyw_data <- function(data_ls, discourse, country, ref, min, emoji, max) {
  data <- data_ls[[country]][[discourse]][["keywords"]]

  # keep just target (antisemitic) keywords if input for reference is false
  if (ref == FALSE) data <- data[data$target == TRUE, ]

  # filter by minimum number of absolut observations (default = 5)
  data <- data[data$docfreq >= min, ] |>
    dplyr::mutate(pos = ifelse(
      .data$target == TRUE, row_number(), n() - row_number() + 1
    ))

  # filter emojis if input is FALSE
  if (emoji == FALSE) {
    data <- data[data$emoji == FALSE, ] |>
      dplyr::mutate(pos = ifelse(
        .data$target == TRUE, row_number(), n() - row_number() + 1
      ))
  }

  # filter by max number of keywords (via input slider)
  data <- data[data$pos <= max, ]

  return(data)
}


# create bar plot for keyword frequencies
fct_plot_keyw <- function(data, ref = FALSE, font) {
  plotdata <- data |>
    dplyr::filter(.data$target != ref) |>
    dplyr::mutate(feature = forcats::fct_reorder(.data$feature, .data$chi2))

  plot <- plotly::plot_ly(
    data = plotdata,
    y = ~feature, x = ~chi2,
    hovertemplate =
      "Occurrence over all comments:<br>%{hovertext}<extra></extra>",
    hovertext = ~text,
    color = ~target, colors = unique(plotdata$color),
    type = "bar", orientation = "h"
  ) |>
    layout(font = list(family = font))

  if (ref) {
    plot <- layout(plot, yaxis = list(title = "", side = "right"))
  } else {
    plot <- layout(plot, yaxis = list(title = ""))
  }

  return(plot)
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
    ) |>
    visInteraction(
      hideEdgesOnDrag = TRUE,
      # navigationButtons = TRUE,
      tooltipDelay = 300
    ) |>
    visNodes(
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

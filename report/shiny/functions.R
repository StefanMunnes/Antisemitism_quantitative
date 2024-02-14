fct_de_text <- function(data, discourse, col) {
  x <- data[data$discourse == discourse, ]

  if (col == "a") line0 <- paste0("<h6>Discourse Event A - ", x$title, "</h6>")
  if (col == "b") line0 <- paste0("<h6>Discourse Event B - ", x$title, "</h6>")

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
    x$comms_as, " (", x$comms_as_prop, "%)", " labelled as antisemitic."
  )

  shiny::HTML(paste(line0, paste(line1, line2, line3, line4, sep = "<br/>")))
}


fct_code_data <- function(data_code_ls, data_code, disc, code_in, cntry, freq) {
  # filter lexicon code list to label factor just for values
  fct_labs <- data_code_ls |>
    filter(.data$code_main %in% code_in)

  # filter for discourse event and code group
  data <- filter(
    data_code,
    .data$discourse == disc,
    .data$code_main %in% code_in
  ) |>
    # add factor label for lexicon codes
    dplyr::mutate(code_fct = ordered(
      .data$code,
      levels = fct_labs$code,
      labels = fct_labs$code_lab_short
    ))

  # create freq OR prop variables for overall discourse or separated by country
  if (cntry && freq) {
    data$value <- data$n_de_ctr
  }
  if (cntry && !freq) {
    data$value <- data$per_de_ctr
  }
  if (!cntry && freq) {
    data$value <- data$n_de
    data <- dplyr::distinct(
      data, .data$discourse, .data$code_fct, .data$value
    )
  }
  if (!cntry && !freq) {
    data$value <- data$per_de
    data <- dplyr::distinct(
      data, .data$discourse, .data$code_fct, .data$value
    )
  }

  return(data)
}


fct_code_freq_plot <- function(data, data2, country, axis, dot, freq, font, clr) {
  
  # add colors by country if option is choosen, otherwise default
  if (country) {
    colors_cntry <- clr[unique(data$country)]

    plot <- plot_ly(data,
      x = ~value, y = ~code_fct,
      color = ~country, colors = colors_cntry
    )
  } else {
    plot <- plot_ly(
      data, x = ~value, y = ~code_fct, marker = list(color = clr[4])
    )
  }

  plot <- layout(plot,
    yaxis = list(title = "", autorange = "reversed"),
    showlegend = TRUE, font = list(family = font)
  )

  # choose marker type from input (bar (default) or dot chart)
  if (dot) plot <- add_markers(plot, size = 5, opacity = 0.8)
  if (!dot) plot <- add_bars(plot)

  if (!country) plot <- layout(plot, showlegend = FALSE)

  if (freq) {
    plot <- layout(plot, xaxis = list(title = "Frequency"))
  } else {
    plot <- layout(plot, xaxis = list(title = "Percentage", ticksuffix = "%"))
  }

  # keep value axis constant over both plots
  if(axis) {
    value_max <- ceiling(max(c(data$value, data2$value)) / 10 ) * 10
    
    plot <- layout(plot, xaxis = list(range = list(0, value_max)))
  }

  return(plot)
}


fct_code_net_plot <- function(data, country, discourse, codes, color, clr) {
  graph <- data[
    data$country %in% country & data$discourse == discourse,
    c("V1", "V2")
  ] |>
    igraph::graph_from_data_frame(
      vertices = codes["ID"],
      directed = FALSE
    )

  V(graph)$size <- igraph::degree(graph) * 10

  if (color) V(graph)$color <- unname(clr[codes$code_main])

  plot <- edgebundleR::edgebundle(graph,
    fontsize = 12, padding = 125, cutoff = 0, width = 800,
    tension = 0.7, nodesize = c(0, 30)
  )

  return(plot)
}



# create keyword data from prepared data and filter by input (create new pos)
fct_keyw_data <- function(data_ls, discourse, country, ref, min, emoji, max, clr) {
  data <- data_ls[[country]][[discourse]][["keywords"]]

  data$color <- ifelse(data$target, clr[2], clr[1])

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
  plotdata <- data[data$target != ref, ] |>
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
fct_net_data <- function(graph, keyw_data, top) {
  nodes_df <- data.frame(
    id = V(graph)$name,
    keyword = V(graph)$name
  ) |>
    dplyr::left_join(
      keyw_data[c("feature", "docfreq", "color")],
      by = c("keyword" = "feature")
    ) |>
    dplyr::mutate(
      size = rescale(docfreq, 10, 30),
      font.size = size * 1.5 + 15
    ) |>
    dplyr::arrange(keyword) # sort alphabetically

  edges_df <- graph |>
    get.edgelist() |>
    as.data.frame() |>
    setNames(c("from", "to")) |>
    dplyr::summarize(width = n(), .by = c("from", "to")) |>
    dplyr::mutate(width = rescale(width, 2, 20)) |>
    dplyr::arrange(desc(width)) |>
    dplyr::mutate(row_per = (row_number() / n()) * 100) |>
    dplyr::mutate(row_per_max = max(row_per) |> round(0), .by = width) |>
    filter(row_per_max <= top)

  list("nodes" = nodes_df, "edges" = edges_df)
}


# network plot: nodes & edges data as input and different predefined options
fct_visNet <- function(nodes_df, edges_df, font, clr) {
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
      font = list(family = font, strokeWidth = 7),
      shadow = list(enabled = TRUE, size = 10)
    ) |>
    visEdges(
      color = list(
        color = unname(clr[3]),
        highlight = unname(clr[6]),
        hover = unname(clr[6]),
        opacity = 0.6
      ),
      hoverWidth = 2,
      smooth = list(enabled = TRUE, type = "continious")
    )
}

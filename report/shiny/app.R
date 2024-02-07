# web app (shiny and moduls)
library(shiny)
library(gridlayout)
library(bslib)

# data manipulation
library(dplyr)
library(stringr)

# visualisations (different plots)
library(plotly)
library(edgebundleR)
library(igraph)
library(visNetwork)


# ---- 1. load data ----
load("data.Rdata")
load("text.Rdata")


country_ls <- list("United Kingdome" = "UK", "Germany" = "DE", "France" = "FR")


# ---- 2. load custom functions ----
source("functions.R")



# ---- 3. UI ----
ui <- grid_page(
  # add style elements for overlaping dropdown from discourse event input
  tags$style(".bslib-card {overflow: visible;}"),
  tags$style(".bslib-card .card-body {overflow: visible;}"),
  tags$style(".selectize-dropdown-content {max-height: 400px; }"),
  layout = c(
    "discourse_events",
    "tabs_output     "
  ),
  row_sizes = c(
    "0.25fr",
    "0.75fr"
  ),
  col_sizes = c(
    "1fr"
  ),
  gap_size = "7px",
  grid_card(
    area = "discourse_events",
    card_body(
      gap = "7px",
      grid_container(
        layout = c("discourse_input discourse_text_a discourse_text_b"),
        gap_size = "7px",
        col_sizes = c(
          "0.4fr",
          "1fr",
          "1fr"
        ),
        row_sizes = c(
          "1fr"
        ),
        grid_card(
          area = "discourse_input",
          card_body(
            selectInput(
              inputId = "discourse_in_a",
              label = "Discourse Event A",
              choices = discourse_list,
              selected = "21_AIC"
            ),
            selectInput(
              inputId = "discourse_in_b",
              label = "Discourse Event B",
              choices = discourse_list,
              selected = "23_AIC"
            )
          )
        ),
        grid_card(
          area = "discourse_text_a",
          card_body(htmlOutput(outputId = "discourse_out_a"))
        ),
        grid_card(
          area = "discourse_text_b",
          card_body(htmlOutput(outputId = "discourse_out_b"))
        )
      )
    )
  ),
  grid_card(
    area = "tabs_output",
    full_screen = TRUE,
    card_body(
      tabsetPanel(
        nav_panel(
          title = "Introduction",
          grid_card(
            area = "intro_text",
            card_body(markdown(text$welcome))
          )
        ),
        nav_panel(
          title = "Label frequencies",
          grid_container(
            layout = c(
              "tabs_code_freq_settings code_freq_out_a code_freq_out_b"
            ),
            row_sizes = c(
              "1fr"
            ),
            col_sizes = c(
              "0.4fr",
              "1fr",
              "1fr"
            ),
            gap_size = "7px",
            grid_card(
              area = "tabs_code_freq_settings",
              card_body(
                tabsetPanel(
                  nav_panel(
                    title = "Settings",
                    card(
                      card_body(
                        checkboxGroupInput(
                          inputId = "code_freq_code_in",
                          label = "Label group",
                          choices = list(
                            "Classic Antisemitic Tropes" = 1,
                            "Tropes of Political or Financial Power" = 2,
                            "Secondary Antisemitism" = 3,
                            "Further Post-Holocaust Concepts" = 4,
                            "Attacks on Israel’s Legitimacy" = 5,
                            "Speech Acts" = 6
                          ),
                          selected = 1
                        ),
                        markdown("**Plot options**"),
                        checkboxInput(
                          "checkbox_cntry", "Separate by country",
                          value = TRUE
                        ),
                        checkboxInput(
                          "checkbox_freq", "Absolut frequencies",
                          value = FALSE
                        ),
                        checkboxInput(
                          "checkbox_dot", "Dotchart",
                          value = FALSE
                        ),
                      )
                    )
                  ),
                  nav_panel(
                    title = "Interpr.",
                    markdown(text$interpr_code_freq)
                  ),
                  nav_panel(
                    title = "Labels",
                    markdown(text$codes_tbl)
                  )
                )
              )
            ),
            grid_card(
              area = "code_freq_out_a",
              full_screen = TRUE,
              card_body(
                plotlyOutput(outputId = "code_freq_plot_a", height = "620px")
              )
            ),
            grid_card(
              area = "code_freq_out_b",
              full_screen = TRUE,
              card_body(
                plotlyOutput(outputId = "code_freq_plot_b", height = "620px")
              )
            )
          )
        ),
        nav_panel(
          title = "Label co-occurrences",
          grid_container(
            layout = c(
              "tabs_code_net_settings code_net_out_a code_net_out_b"
            ),
            row_sizes = c(
              "1fr"
            ),
            col_sizes = c(
              "0.4fr",
              "1fr",
              "1fr"
            ),
            gap_size = "7px",
            grid_card(
              area = "tabs_code_net_settings",
              card_body(
                tabsetPanel(
                  nav_panel(
                    title = "Settings",
                    card(
                      card_body(
                        checkboxGroupInput(
                          inputId = "code_net_cntry_in",
                          label = "Country",
                          choices = country_ls,
                          selected = "UK"
                        ),
                        markdown("**Plot options**"),
                        checkboxInput(
                          "checkbox_code_net_clr", "Color label group",
                          value = FALSE
                        )
                      )
                    )
                  ),
                  nav_panel(
                    title = "Interpr.",
                    markdown(text$interpr_code_net)
                  ),
                  nav_panel(
                    title = "Labels",
                    markdown(text$codes_tbl)
                  )
                )
              )
            ),
            grid_card(
              area = "code_net_out_a",
              full_screen = TRUE,
              card_body(
                edgebundleOutput(outputId = "code_net_plot_a", height = "620px")
              )
            ),
            grid_card(
              area = "code_net_out_b",
              full_screen = TRUE,
              card_body(
                edgebundleOutput(outputId = "code_net_plot_b", height = "620px")
              )
            )
          )
        ),
        nav_panel(
          title = "Keyword frequencies",
          grid_container(
            layout = c(
              "tabs_keyw_freq_settings keyw_freq_out_a keyw_freq_out_b"
            ),
            row_sizes = c(
              "1fr"
            ),
            col_sizes = c(
              "0.4fr",
              "1fr",
              "1fr"
            ),
            gap_size = "7px",
            grid_card(
              area = "tabs_keyw_freq_settings",
              card_body(
                tabsetPanel(
                  nav_panel(
                    title = "Settings",
                    card(
                      card_body(
                        radioButtons(
                          inputId = "keyw_cntry_in",
                          label = "Country",
                          choices = country_ls,
                          selected = "UK"
                        ),
                        sliderInput(
                          inputId = "keyw_num_slider",
                          label = "Number of keywords",
                          min = 5, max = 40, value = 30, step = 1
                        ),
                        sliderInput(
                          inputId = "keyw_min_slider",
                          label = "Min. num of comments incl. keyw.",
                          min = 1, max = 15, value = 5, step = 1
                        ),
                        checkboxInput(
                          inputId = "keyw_reference",
                          label = "Show non-antisem. keyw.",
                          value = FALSE
                        ),
                        checkboxInput(
                          inputId = "keyw_emoji",
                          label = "Include emojis",
                          value = TRUE
                        )
                      )
                    )
                  ),
                  nav_panel(
                    title = "Interpr.",
                    markdown(text$interpr_keyw_freq)
                  )
                )
              )
            ),
            grid_card(
              area = "keyw_freq_out_a",
              full_screen = TRUE,
              card_body(
                plotlyOutput(outputId = "keyw_freq_plot_a", height = "620px")
              )
            ),
            grid_card(
              area = "keyw_freq_out_b",
              full_screen = TRUE,
              card_body(
                plotlyOutput(outputId = "keyw_freq_plot_b", height = "620px")
              )
            )
          )
        ),
        nav_panel(
          title = "Keyword network",
          grid_container(
            layout = c(
              "tabs_keyw_net_settings keyw_net_out_a keyw_net_out_b"
            ),
            row_sizes = c(
              "1fr"
            ),
            col_sizes = c(
              "0.4fr",
              "1fr",
              "1fr"
            ),
            gap_size = "7px",
            grid_card(
              area = "tabs_keyw_net_settings",
              card_body(
                tabsetPanel(
                  nav_panel(
                    title = "Settings",
                    card(
                      card_body(
                        sliderInput(
                          inputId = "keyw_net_topper",
                          label = "Top % of connections",
                          min = 10, max = 100, value = 30, step = 5, post = "%"
                        ),
                        checkboxInput(
                          inputId = "keyw_net_omit",
                          label = "Omit isolated keywords",
                          value = TRUE
                        )
                      )
                    )
                  ),
                  nav_panel(
                    title = "Interpr.",
                    markdown(text$interpr_keyw_net)
                  )
                )
              )
            ),
            grid_card(
              area = "keyw_net_out_a",
              full_screen = TRUE,
              card_body(
                visNetworkOutput(outputId = "keyw_net_plot_a", height = "620px")
              )
            ),
            grid_card(
              area = "keyw_net_out_b",
              full_screen = TRUE,
              card_body(
                visNetworkOutput(outputId = "keyw_net_plot_b", height = "620px")
              )
            )
          )
        )
      )
    )
  )
)


server <- function(input, output) {
  # 1. create text output (by discourse event and multiple lines)
  output$discourse_out_a <- renderUI({
    data_de <- filter(data_de, .data$discourse == input$discourse_in_a)

    line1 <- paste0("<strong>", data_de$description, "</strong>")
    line2 <- paste(
      "From", data_de$date_min, "to", data_de$date_max,
      "in", str_count(data_de$cntrs, "\\S+"),
      ifelse(str_count(data_de$cntrs, "\\S+") == 1, "country:", "countries:"),
      data_de$cntrs
    )
    # line3 <- paste(
    #   data_de$docs, "discussion threats from",
    #   data_de$srcs, "unique sources"
    # )
    line4 <- paste0(
      data_de$comms, " overall comments, ",
      data_de$comms_as, " labelled as antisemitic (", data_de$comms_as_prop, "%)"
    )
    HTML(paste(line1, line2, line4, sep = "<br/>")) # line3,
  })

  output$discourse_out_b <- renderUI({
    data_de <- filter(data_de, .data$discourse == input$discourse_in_b)

    line1 <- paste0("<strong>", data_de$description, "</strong>")
    line2 <- paste(
      "From", data_de$date_min, "to", data_de$date_max,
      "in", str_count(data_de$cntrs, "\\S+"),
      ifelse(str_count(data_de$cntrs, "\\S+") == 1, "country:", "countries:"),
      data_de$cntrs
    )
    # line3 <- paste(
    #   data_de$docs, "discussion threats from",
    #   data_de$srcs, "unique sources"
    # )
    line4 <- paste0(
      data_de$comms, " overall comments, ",
      data_de$comms_as, " labelled as antisemitic (", data_de$comms_as_prop, "%)"
    )
    HTML(paste(line1, line2, line4, sep = "<br/>")) # line3,
  })


  # 2.1 create plot specific data (filter by discourse, code group, country)
  data_code_a <- reactive({
    fnct_data(
      codes_list, data_code, input$discourse_in_a, input$code_freq_code_in,
      input$checkbox_cntry, input$checkbox_freq
    )
  })

  data_code_b <- reactive({
    fnct_data(
      codes_list, data_code, input$discourse_in_b, input$code_freq_code_in,
      input$checkbox_cntry, input$checkbox_freq
    )
  })


  # 3. create code frequencies plots (by country and reactive to options)
  output$code_freq_plot_a <- renderPlotly({
    if (input$checkbox_cntry) {
      colors_cntry <- data_code_a() |>
        arrange(.data$country) |>
        distinct(.data$country_clr) |>
        pull()

      plot <- plot_ly(data_code_a(),
        x = ~value, y = ~code_fct,
        color = ~country, colors = colors_cntry
      )
    } else {
      plot <- plot_ly(data_code_a(), x = ~value, y = ~code_fct)
    }

    if (input$checkbox_dot) {
      plot <- add_markers(plot, size = 5, opacity = 0.8)
    } else {
      plot <- add_bars(plot)
    }

    plot <- layout(plot,
      yaxis = list(title = "", autorange = "reversed"),
      showlegend = TRUE
    )
    if (!input$checkbox_cntry) {
      plot <- layout(plot, showlegend = FALSE)
    }

    if (input$checkbox_freq) {
      plot <- layout(plot, xaxis = list(title = "Frequency"))
    } else {
      plot <- layout(plot, xaxis = list(title = "Percentage", ticksuffix = "%"))
    }

    plot
  })

  output$code_freq_plot_b <- renderPlotly({
    if (input$checkbox_cntry) {
      colors_cntry <- data_code_b() |>
        arrange(.data$country) |>
        distinct(.data$country_clr) |>
        pull()

      plot <- plot_ly(data_code_b(),
        x = ~value, y = ~code_fct,
        color = ~country, colors = colors_cntry
      )
    } else {
      plot <- plot_ly(data_code_b(), x = ~value, y = ~code_fct)
    }

    if (input$checkbox_dot) {
      plot <- add_markers(plot, size = 5, opacity = 0.8)
    } else {
      plot <- add_bars(plot)
    }

    plot <- layout(plot,
      yaxis = list(title = "", autorange = "reversed"),
      # axis = list(range = list(0, ...)),
      showlegend = TRUE
    )

    if (input$checkbox_freq) {
      plot <- layout(plot, xaxis = list(title = "Frequency"))
    } else {
      plot <- layout(plot, xaxis = list(title = "Percentage", ticksuffix = "%"))
    }
    if (!input$checkbox_cntry) {
      plot <- layout(plot, showlegend = FALSE)
    }

    plot
  })

  # 4. create code network plots
  output$code_net_plot_a <- renderEdgebundle({
    relation <- filter(
      data_relations,
      .data$country %in% input$code_net_cntry_in,
      .data$discourse == input$discourse_in_a
    ) |>
      select(V1, V2)

    graph <- graph_from_data_frame(
      relation,
      vertices = codes_list["ID"],
      directed = FALSE
    )

    V(graph)$size <- degree(graph) * 10

    if (input$checkbox_code_net_clr) V(graph)$color <- codes_list$code_clr

    plot <- edgebundle(graph,
      fontsize = 12, padding = 120, cutoff = 0, width = 800,
      tension = 0.7, nodesize = c(0, 30)
    )

    plot
  })

  output$code_net_plot_b <- renderEdgebundle({
    relation <- filter(
      data_relations,
      .data$country %in% input$code_net_cntry_in,
      .data$discourse == input$discourse_in_b
    ) |>
      select(V1, V2)

    graph <- graph_from_data_frame(
      relation,
      vertices = codes_list["ID"],
      directed = FALSE
    )

    V(graph)$size <- degree(graph) * 10

    if (input$checkbox_code_net_clr) V(graph)$color <- codes_list$code_clr

    plot <- edgebundle(graph,
      fontsize = 12, padding = 120, cutoff = 0, # width = 800,
      tension = 0.7, nodesize = c(0, 30)
    )

    plot
  })


  # prepare data for keywords (by min doc occurence, by emoji, by max number)
  data_keyw_a <- reactive({
    tryCatch(
      {
        fct_keyw_data(
          data_ls = data_dfm_keyw_ls,
          discourse = input$discourse_in_a, country = input$keyw_cntry_in,
          ref = input$keyw_reference, min = input$keyw_min_slider,
          emoji = input$keyw_emoji, max = input$keyw_num_slider
        )
      },
      error = function(e) {
        NULL
      }
    )
  })


  data_keyw_b <- reactive({
    tryCatch(
      {
        fct_keyw_data(
          data_ls = data_dfm_keyw_ls,
          discourse = input$discourse_in_b, country = input$keyw_cntry_in,
          ref = input$keyw_reference, min = input$keyw_min_slider,
          emoji = input$keyw_emoji, max = input$keyw_num_slider
        )
      },
      error = function(e) {
        NULL
      }
    )
  })

  output$keyw_freq_plot_a <- renderPlotly({
    # check for empty data -> throw empty page
    fct_validate(data_keyw_a(), text$validate)

    plotdata <- data_keyw_a()

    plot_target <- fct_plot_keyw(plotdata)

    if (input$keyw_reference) {
      plot_ref <- fct_plot_keyw(plotdata, ref = TRUE)

      plot_comb <- subplot(plot_target, plot_ref) |>
        layout(
          title = "Antisemitic vs. reference keywords",
          showlegend = FALSE
        )

      return(plot_comb)
    } else {
      return(plot_target)
    }
  })

  output$keyw_freq_plot_b <- renderPlotly({
    # check for empty data -> throw empty page
    fct_validate(data_keyw_b(), text$validate)

    plotdata <- data_keyw_b()

    plot_target <- fct_plot_keyw(plotdata)

    if (input$keyw_reference) {
      plot_ref <- fct_plot_keyw(plotdata, ref = TRUE)

      plot_comb <- subplot(plot_target, plot_ref) |>
        layout(
          title = "Antisemitic vs. reference keywords",
          showlegend = FALSE
        )

      return(plot_comb)
    } else {
      return(plot_target)
    }
  })


  output$keyw_net_plot_a <- renderVisNetwork({
    # check for empty data -> throw empty page
    fct_validate(data_keyw_a(), text$validate)

    graph_data_a <- data_dfm_keyw_ls[[input$keyw_cntry_in]][[input$discourse_in_a]][["dfm"]] |>
      quanteda::dfm_select(data_keyw_a()$feature) |>
      quanteda::fcm(context = "document") |>
      graph_from_adjacency_matrix(mode = "max", diag = FALSE) |>
      fct_graph_data(data_keyw_a(), input$keyw_net_topper)

    if (input$keyw_net_omit) {
      edges <- unique(c(graph_data_a$edges$from, graph_data_a$edges$to))

      graph_data_a$nodes <- filter(graph_data_a$nodes, id %in% edges)
    }

    fct_visNet(graph_data_a$nodes, graph_data_a$edges)
  })

  output$keyw_net_plot_b <- renderVisNetwork({
    # check for empty data -> throw empty page
    fct_validate(data_keyw_b(), text$validate)

    graph_data_b <- data_dfm_keyw_ls[[input$keyw_cntry_in]][[input$discourse_in_b]][["dfm"]] |>
      quanteda::dfm_select(data_keyw_b()$feature) |>
      quanteda::fcm(context = "document") |>
      graph_from_adjacency_matrix(mode = "max", diag = FALSE) |>
      fct_graph_data(data_keyw_b(), input$keyw_net_topper)

    if (input$keyw_net_omit) {
      edges <- unique(c(graph_data_b$edges$from, graph_data_b$edges$to))

      graph_data_b$nodes <- filter(graph_data_b$nodes, id %in% edges)
    }

    fct_visNet(graph_data_b$nodes, graph_data_b$edges)
  })

  bs_theme(font_scale = 0.5) # , `enable-gradients` = TRUE, `enable-shadows` = TRUE
}

shinyApp(ui, server)

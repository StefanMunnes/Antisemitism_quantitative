library(dplyr)
library(stringr)
library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)


# ---- 1. load data ----

# 1.1 discourse event information
discourse_list <- read.csv("data/discourse_list.csv")
data_de <- read.csv("data/data_de.csv")

# 1.2 lexicon labels
lexicon_fct_labels <- read.csv("data/lexicon_fct_labels.csv")

# 1.3 code data (proportion, code_group, colors)
data_code <- read.csv("data/data_code.csv")



# ---- 2. create custom functions ----
fnct_data <- function(de_in, code_in, cntry_in, freq_in) {
  # filter lexicon code list to label factor just for values
  fct_labs <- lexicon_fct_labels |>
    filter(.data$code_lex_main %in% code_in)

  # filter for discourse event and code group
  data <- filter(
    data_code,
    .data$discourse == de_in,
    .data$code_lex_main %in% code_in
  ) |>
    # add factor label for lexicon codes
    mutate(code_lex_fct = ordered(
      .data$code_lex,
      levels = fct_labs$code_lex,
      labels = fct_labs$code_lex_name
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
      data, .data$discourse, .data$code_lex_fct, .data$value
    )
  }
  if (!cntry_in && !freq_in) {
    data$value <- data$per_de
    data <- distinct(
      data, .data$discourse, .data$code_lex_fct, .data$value
    )
  }

  data
}


# ---- 3. UI ----

ui <- grid_page(
  layout = c(
    "header header",
    "left_panel main_in_out"
  ),
  row_sizes = c(
    "40px",
    "1fr"
  ),
  col_sizes = c(
    "330px",
    "1.4fr"
  ),
  gap_size = "8px",
  grid_card_text(
    area = "header",
    content = "Discourse event comparison",
    alignment = "start",
    is_title = TRUE
  ),
  grid_card(
    area = "left_panel",
    card_body(
      gap = "8px",
      markdown("**Plot content**"),
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
      ),
      checkboxGroupInput(
        inputId = "code_in",
        label = "Code group",
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
      checkboxInput("checkbox_cntry", "Separate by country", value = TRUE),
      checkboxInput("checkbox_freq", "Absolut frequencies", value = FALSE),
      checkboxInput("checkbox_dot", "Dotchart", value = FALSE),
      markdown(
        "**Interpretation example**

        Of the classic antisemitic tropes, Evil/The Devil is the most
        common one in social media comments on the Arab-Israeli conflict.
        This attribution is found in 30% to 40% of antisemitic comments in 2021.
        Most common in Germany, least common in France.
        Compared to the discourse event from 2023, for which data is only
        available for the United Kingdom,
        the percentage has decreased to just over 10%"
      )
    )
  ),
  grid_card(
    area = "main_in_out",
    card_body(
      grid_container(
        layout = c(
          "out_text_a out_text_b",
          "out_plot_a out_plot_b"
        ),
        row_sizes = c(
          "0.54fr",
          "1.84fr"
        ),
        col_sizes = c(
          "1fr",
          "1fr"
        ),
        gap_size = "8px",
        grid_card(
          area = "out_text_a",
          card_body(htmlOutput(outputId = "textOutput_a"))
        ),
        grid_card(
          area = "out_text_b",
          card_body(htmlOutput(outputId = "textOutput_b"))
        ),
        grid_card(
          area = "out_plot_a",
          card_body(plotlyOutput(outputId = "plot_a")),
          full_screen = TRUE
        ),
        grid_card(
          area = "out_plot_b",
          card_body(plotlyOutput(outputId = "plot_b")),
          full_screen = TRUE
        )
      )
    )
  )
)


server <- function(input, output) {
  # 1. create plot specific data (filter by discourse, code group, country)
  data_a <- reactive({
    fnct_data(
      input$discourse_in_a, input$code_in,
      input$checkbox_cntry, input$checkbox_freq
    )
  })

  data_b <- reactive({
    fnct_data(
      input$discourse_in_b, input$code_in,
      input$checkbox_cntry, input$checkbox_freq
    )
  })


  # 2. create text output (by discourse event and multiple lines)
  output$textOutput_a <- renderUI({
    data_de <- filter(data_de, .data$discourse == input$discourse_in_a)

    line1 <- paste0("<strong>", data_de$title, "</strong>")
    line2 <- paste(
      "From", data_de$date_min, "to", data_de$date_max,
      "in", str_count(data_de$cntrs, "\\S+"),
      ifelse(str_count(data_de$cntrs, "\\S+") == 1, "country:", "countries:"),
      data_de$cntrs
    )
    line3 <- paste(
      data_de$docs, "discussion threats from",
      data_de$srcs, "unique sources"
    )
    line4 <- paste0(
      data_de$comms, " overall comments, ",
      data_de$comms_as, " coded as antisemitic (", data_de$comms_as_prop, "%)"
    )
    HTML(paste(line1, line2, line3, line4, sep = "<br/>"))
  })

  output$textOutput_b <- renderUI({
    data_de <- filter(data_de, .data$discourse == input$discourse_in_b)

    line1 <- paste0("<strong>", data_de$title, "</strong>")
    line2 <- paste(
      "From", data_de$date_min, "to", data_de$date_max,
      "in", str_count(data_de$cntrs, "\\S+"),
      ifelse(str_count(data_de$cntrs, "\\S+") == 1, "country:", "countries:"),
      data_de$cntrs
    )
    line3 <- paste(
      data_de$docs, "discussion threats from",
      data_de$srcs, "unique sources"
    )
    line4 <- paste0(
      data_de$comms, " overall comments, ",
      data_de$comms_as, " coded as antisemitic (", data_de$comms_as_prop, "%)"
    )
    HTML(paste(line1, line2, line3, line4, sep = "<br/>"))
  })


  # 3. create plots (by country and reactive to options)
  output$plot_a <- renderPlotly({
    if (input$checkbox_cntry) {
      colors_cntry <- data_a() |>
        arrange(.data$country) |>
        distinct(.data$color) |>
        pull()

      plot <- plot_ly(data_a(),
        x = ~value, y = ~code_lex_fct,
        color = ~country, colors = colors_cntry
      )
    } else {
      plot <- plot_ly(data_a(), x = ~value, y = ~code_lex_fct)
    }

    if (input$checkbox_dot) {
      plot <- add_markers(plot, opacity = 0.8)
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

  output$plot_b <- renderPlotly({
    if (input$checkbox_cntry) {
      colors_cntry <- data_b() |>
        arrange(.data$country) |>
        distinct(.data$color) |>
        pull()

      plot <- plot_ly(data_b(),
        x = ~value, y = ~code_lex_fct,
        color = ~country, colors = colors_cntry
      )
    } else {
      plot <- plot_ly(data_b(), x = ~value, y = ~code_lex_fct)
    }

    if (input$checkbox_dot) {
      plot <- add_markers(plot, opacity = 0.8)
    } else {
      plot <- add_bars(plot)
    }

    plot <- layout(plot,
      yaxis = list(title = "", autorange = "reversed"),
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
}

shinyApp(ui, server)

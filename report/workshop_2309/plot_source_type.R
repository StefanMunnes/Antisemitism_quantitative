# number of comments for each discourse event per country

data_source_type <- data_source |>
  count(country, source_type) |>
  mutate(country_n = sum(n), .by = country) |>
  mutate(
    country_n = paste(country, country_n, sep = "\n"),
    source_type_n = paste(source_type, n, sep = "\n")
  )


png(paste0(output, "plot_treemap_source_type.png"), width = 800, height = 350)

treemap(data_source_type,
  index = c("country_n", "source_type_n"),
  vSize = "n",
  palette = "Set2",
  fontsize.labels = c(25, 15),
  fontcolor.labels = "black",
  force.print.labels = TRUE,
  align.labels = list(c("left", "top"), c("center", "center")),
  bg.labels = 0,
  title = "",
  aspRatio = 2.5
)

dev.off()



# ---- 2.1 different ideations for each discourse events per country ----
data_ideation_source_type <- data_source |>
  count(country, source_type, comment_ideation) |>
  filter(!is.na(comment_ideation), comment_ideation != "I2") |>
  mutate(
    sum = sum(n),
    prop = n / sum,
    .by = c(country, source_type)
  )

data_antisem_country <- data_source |>
  filter(!is.na(comment_ideation), comment_ideation != "I2") |>
  mutate(antisem = comment_ideation %in% c("I1", "I1ASC", "I1a")) |>
  summarize(prop_antisem = mean(antisem), .by = country)


plot_ideation_data_source <- ggplot() +
  geom_bar(
    data = data_ideation_source_type,
    aes(x = source_type, y = prop, fill = comment_ideation),
    stat = "identity"
  ) +
  geom_hline(
    data = data_antisem_country,
    aes(yintercept = prop_antisem)
  ) +
  geom_text(
    data = data_antisem_country,
    aes(y = prop_antisem + 0.02, label = round(prop_antisem, 2)),
    x = 1, size = 3.5
  ) +
  facet_grid(~country, scales = "free") +
  scale_fill_manual(values = col_ideation, name = "Ideation") +
  labs(x = "", y = "Proportion") +
  theme_classic() +
  theme(
    strip.text = element_text(size = 12),
    panel.grid.major.y = element_line(color = "grey")
  )

ggsave(plot_ideation_data_source,
  filename = paste0(output, "plot_ideation_source_type.png"),
  scale = 1.2, width = 8, height = 4.5
)

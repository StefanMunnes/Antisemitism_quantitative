# number of comments for each discourse event per country

data_source_type <- data_source |>
  count(country, source_type) |>
  mutate(country_n = sum(n), .by = country) |>
  mutate(
    country_n = paste0(country, " (N: ", country_n, ")"),
    source_type_n = paste(source_type, n, sep = "\n")
  )


png(paste0(output, "plot_comments_source_type.png"), width = 800, height = 330)

treemap(data_source_type,
  index = c("country_n", "source_type_n"),
  vSize = "n",
  palette = "Set2",
  fontsize.labels = c(20, 15),
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
  filter(comment_ideation != "Unclear Ideation") |>
  mutate(
    sum = sum(n),
    prop = n / sum,
    .by = c(country, source_type)
  )

data_antisem_country <- data_source |>
  filter(comment_ideation != "Unclear Ideation") |>
  summarize(prop_antisem = mean(comment_antisem), .by = country)


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
    aes(y = prop_antisem + 0.03, label = round(prop_antisem, 2)),
    x = 1, size = 3.5
  ) +
  facet_grid(~country, scales = "free") +
  scale_fill_manual(values = col_ideation, name = "Ideation") +
  labs(x = "", y = "proportion") +
  theme_classic() +
  theme(
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 8),
    panel.grid.major.y = element_line(color = "grey")
  )

ggsave(plot_ideation_data_source,
  filename = paste0(output, "plot_ideation_source_type.png"),
  scale = 0.9, width = 10, height = 4.5
)

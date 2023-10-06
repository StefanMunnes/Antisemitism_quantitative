data_source_title <- data_source |>
  mutate(
    source_title =
      ifelse(
        is.na(source_title),
        str_extract(document, "^.+_(.+$)", 1),
        source_title
      )
  ) |>
  count(source_title, country, comment_ideation) |>
  filter(!is.na(comment_ideation), comment_ideation != "I2") |>
  mutate(
    sum = sum(n),
    prop = n / sum,
    .by = source_title
  ) |>
  filter(!str_detect(comment_ideation, "I0")) |>
  mutate(prop_sum = sum(prop), .by = source_title) |>
  group_by(country) |>
  arrange(desc(prop_sum), source_title, .by_group = TRUE) |>
  mutate(rank = rleid(source_title)) |>
  filter(rank <= 15) |>
  mutate(comment_ideation = factor(comment_ideation,
    levels = c("I1", "I1ASC", "I1a")
  ))

plot_source_title <- data_source_title |>
  ggplot(aes(x = reorder(rank, desc(rank)))) +
  geom_bar(
    aes(y = prop, fill = comment_ideation),
    stat = "identity"
  ) +
  geom_text(aes(label = source_title), y = 0, hjust = 0, size = 3) +
  coord_flip() +
  facet_grid(~country) +
  scale_fill_manual(values = col_ideation[3:5], name = "Ideation") +
  labs(y = "proportion", x = "") +
  theme_classic() +
  theme(
    strip.text = element_text(size = 12),
    panel.grid.major.x = element_line(color = "#bebebec7"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


ggsave(plot_source_title,
  filename = paste0(output, "plot_source_title.png"),
  scale = 1.2, width = 10, height = 4.5
)

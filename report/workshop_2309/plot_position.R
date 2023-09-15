# ---- 1. show appearence of ideations over time comments ----
data_position <- data_source |>
  mutate(
    pos_rel = comment_id / max(comment_id, na.rm = TRUE),
    .by = document
  ) |>
  filter(!is.na(comment_ideation))

# 1.1 over all
plot_position <- data_position |>
  ggplot(aes(pos_rel, fill = comment_ideation)) +
  geom_density(alpha = 0.8, adjust = 0.4) +
  # stat_bin(boundary = 1, bins = 10) +
  facet_grid(comment_ideation ~ .) +
  scale_fill_manual(values = col_ideation, name = "Ideation") +
  labs(x = "relative position") +
  theme_classic() +
  theme(
    strip.text = element_text(size = 12),
    panel.grid.major.y = element_line(color = "grey"),
    legend.position = "none"
  )

ggsave(plot_position,
  filename = paste0(output, "plot_position.png"),
  scale = 1.2, width = 8, height = 4.5
)

# 1.2 over source type (platform)
plot_position_source_type <- data_position |>
  ggplot(aes(pos_rel, fill = comment_ideation)) +
  geom_density(alpha = 0.8, adjust = 0.6, linewidth = 0.2) +
  #  stat_bin(boundary = 1, bins = 10) +
  facet_grid(comment_ideation ~ source_type, scales = "free") +
  scale_fill_manual(values = col_ideation, name = "Ideation") +
  labs(x = "relative position") +
  theme_classic() +
  theme(
    strip.text.x = element_text(size = 6),
    strip.text.y = element_text(size = 9),
    panel.grid.major.y = element_line(color = "grey"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )

ggsave(plot_position_source_type,
  filename = paste0(output, "plot_position_source_type.png"),
  scale = 1.1, width = 9, height = 4.5
)

# 1.3 over discourse
plot_position_discourse <- data_position |>
  ggplot(aes(pos_rel, fill = comment_ideation)) +
  geom_density(alpha = 0.8, adjust = 0.6, linewidth = 0.2) +
  #  stat_bin(boundary = 1, bins = 10) +
  facet_grid(comment_ideation ~ discourse, scales = "free") +
  scale_fill_manual(values = col_ideation, name = "Ideation") +
  labs(x = "relative position") +
  theme_classic() +
  theme(
    strip.text.x = element_text(size = 6),
    strip.text.y = element_text(size = 9),
    panel.grid.major.y = element_line(color = "grey"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )

ggsave(plot_position_discourse,
  filename = paste0(output, "plot_position_discourse.png"),
  scale = 1.1, width = 9, height = 4.5
)

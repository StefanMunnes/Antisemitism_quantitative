# ---- 1. where appear antisemitic comments appear more often ----
data_reply <- data_source |>
  filter(!is.na(comment_level)) |>
  mutate(
    reply = comment_level > 0,
    reply_mean = mean(reply)
  ) |>
  summarize(
    n = n(), reply_mean = first(reply_mean),
    .by = c(reply, comment_ideation)
  ) |>
  filter(comment_ideation != "Unclear Ideation") |>
  mutate(
    n_sum = sum(n),
    prop = n / n_sum,
    prop_pos = prop / 2,
    # comment_ideation = paste0(comment_ideation, " (N: ", n_sum, ")"),
    .by = comment_ideation
  ) |>
  filter(reply)

plot_reply <- data_reply |>
  ggplot(aes(x = comment_ideation, y = prop, fill = comment_ideation)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(y = prop_pos, label = round(prop, 2))) +
  geom_hline(
    aes(yintercept = reply_mean),
    color = "#202020"
  ) +
  facet_grid(. ~ comment_ideation, scales = "free_x") +
  labs(x = "", y = "proportion") +
  scale_fill_manual(values = col_ideation, ) +
  scale_y_continuous(n.breaks = 8) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 12),
    panel.grid.major.y = element_line(color = "grey")
  )

ggsave(plot_reply,
  filename = paste0(output, "plot_reply.png"),
  scale = 0.9, width = 9, height = 5
)


# ---- 2. number of replies for (non) antisemitic comments ----
data_reply_n <- data_source |>
  mutate(,
    threat_id = ifelse(comment_level == 0, 1, 0) |> cumsum(),
    .by = document
  ) |>
  mutate(
    com_antisem_first = first(comment_antisem),
    threat_replies = n() - 1,
    .by = c(document, threat_id)
  ) |>
  filter(comment_level == 0, !is.na(com_antisem_first)) |>
  mutate(
    replies_mean_all = mean(threat_replies),
    threat_replies = ifelse(threat_replies >= 5, 5, threat_replies),
    .by = com_antisem_first
  ) |>
  summarise(
    n = n(), replies_mean_all = first(replies_mean_all),
    .by = c(threat_replies, com_antisem_first)
  ) |>
  mutate(
    n_sum = sum(n),
    prop = n / n_sum,
    prop_pos = prop / 2,
    .by = com_antisem_first
  ) |>
  mutate(
    com_antisem_first = ordered(com_antisem_first,
      levels = c(FALSE, TRUE),
      labels = c(
        paste0("non-antisemitic comment (N: ", max(n_sum), ")"),
        paste0("antisemitic comment (N: ", min(n_sum), ")")
      )
    )
  )

plot_reply_n <- data_reply_n |>
  ggplot(aes(x = threat_replies, y = prop, fill = com_antisem_first)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = prop_pos, label = round(prop, 2))) +
  geom_vline(aes(xintercept = replies_mean_all, color = com_antisem_first)) +
  geom_text(
    aes(
      x = replies_mean_all + 0.35, y = 0.75,
      label = round(replies_mean_all, 2)
    )
  ) +
  facet_grid(. ~ com_antisem_first) +
  labs(x = "# of replies", y = "proportion") +
  scale_x_continuous(
    breaks = seq(0, 5),
    labels = c("0", "1", "2", "3", "4", "5+")
  ) +
  scale_y_continuous(n.breaks = 7) +
  scale_fill_manual(values = c(col_ideation[1], col_ideation[4])) +
  scale_color_manual(
    values = c(col_ideation[1], col_ideation[4]), guide = "none"
  ) +
  theme_classic() +
  theme(
    strip.text = element_text(size = 12),
    panel.grid.major.y = element_line(color = "#bebebec7"),
    legend.position = "none"
  )

ggsave(plot_reply_n,
  filename = paste0(output, "plot_reply_n.png"),
  scale = 0.9, width = 9, height = 5
)


# ---- 3. share of antisemitic replies to (non) antisemitic comments ----
data_reply_antisem <- data_source |>
  mutate(,
    threat_id = ifelse(comment_level == 0, 1, 0) |> cumsum(),
    .by = document
  ) |>
  mutate(
    com_antisem_first = ifelse(row_number() == 1, first(comment_antisem), NA),
    com_antisem_reply = ifelse(row_number() > 1, comment_antisem, NA),
    reply_antisem_mean = mean(com_antisem_reply, na.rm = TRUE),
    .by = c(document, threat_id)
  ) |>
  # keep just first comment and comments with replies
  filter(!is.na(com_antisem_first), !is.na(reply_antisem_mean)) |>
  mutate(
    com_antisem_n = n(),
    com_antisem_mean = mean(reply_antisem_mean),
    .by = comment_antisem
  ) |>
  mutate(
    comment_antisem = ordered(comment_antisem,
      levels = c(FALSE, TRUE),
      labels = c(
        paste0("non-antisemitic (N: ", max(com_antisem_n), ")"),
        paste0("antisemitic (N: ", min(com_antisem_n), ")")
      )
    )
  )

plot_reply_antisem <- data_reply_antisem |>
  ggplot(aes(
    x = reply_antisem_mean,
    group = comment_antisem,
    fill = comment_antisem
  )) +
  geom_density(alpha = 0.6, adjust = 0.2) +
  geom_vline(aes(xintercept = com_antisem_mean, color = comment_antisem)) +
  geom_text(
    aes(x = com_antisem_mean + 0.03, y = 50, label = round(com_antisem_mean, 2))
  ) +
  scale_fill_manual(
    values = c(col_ideation[2], col_ideation[4]), name = "Initial comment"
  ) +
  scale_color_manual(
    values = c(col_ideation[2], col_ideation[4]), guide = "none"
  ) +
  labs(x = "proportion of antisemitic replies", y = "density") +
  theme_classic() +
  theme(legend.position = c(.8, .8))

ggsave(plot_reply_antisem,
  filename = paste0(output, "plot_reply_antisem.png"),
  scale = 0.8, width = 9, height = 5
)

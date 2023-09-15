# ---- 1. where appear antisemitic comments appear more often ----
data_reply <- data_source |>
  filter(!is.na(comment_level)) |>
  mutate(
    reply = ifelse(comment_level > 0, 1, 0),
    reply_mean = mean(reply)
  ) |>
  summarize(
    n = n(), reply_mean = first(reply_mean),
    .by = c(reply, comment_ideation)
  ) |>
  filter(!is.na(comment_ideation), comment_ideation != "I2") |>
  mutate(
    n_sum = sum(n),
    prop = n / n_sum,
    prop_pos = prop / 2,
    comment_ideation = paste0(comment_ideation, " (N: ", n_sum, ")"),
    reply = factor(reply, labels = c("No reply", "Reply")),
    .by = comment_ideation
  )

plot_reply <- data_reply |>
  ggplot(aes(x = reply, y = prop, fill = reply)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_text(aes(y = prop_pos, label = round(prop, 2))) +
  geom_hline(aes(yintercept = 1 - reply_mean), color = "#f8766dde") +
  geom_hline(aes(yintercept = reply_mean), color = "#00bfc4de") +
  facet_grid(~comment_ideation) +
  labs(x = "", y = "proportion") +
  scale_y_continuous(n.breaks = 8) +
  theme_classic() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12)
  )

ggsave(plot_reply,
  filename = paste0(output, "plot_reply.png"),
  scale = 1.1, width = 9, height = 5
)


# ---- 2. number of replies for (non) antisemitic comments ----
data_reply_n <- data_source |>
  mutate(,
    threat_id = ifelse(comment_level == 0, 1, 0) |>
      cumsum(),
    com_antisem = case_when(
      str_detect(comment_ideation, "I0") ~ "non-antisemitic",
      str_detect(comment_ideation, "I1") ~ "antisemitic",
      .default = NA
    ),
    .after = comment_level,
    .by = document
  ) |>
  mutate(
    com_antisem_first = ifelse(row_number() == 1, first(com_antisem), NA),
    threat_replies = n() - 1,
    .by = c(document, threat_id)
  ) |>
  filter(!is.na(com_antisem_first), comment_level == 0) |>
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
    com_antisem_first = paste0(com_antisem_first, " (N: ", n_sum, ")"),
    .by = com_antisem_first
  )

plot_reply_n <- data_reply_n |>
  ggplot(aes(x = threat_replies, y = prop, fill = com_antisem_first)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = prop_pos, label = round(prop, 2))) +
  geom_vline(aes(xintercept = replies_mean_all, color = com_antisem_first)) +
  geom_text(
    aes(
      x = replies_mean_all + 0.3, y = 0.75,
      label = round(replies_mean_all, 2)
    )
  ) +
  facet_grid(~com_antisem_first) +
  labs(x = "# of replies", y = "proportion") +
  scale_x_continuous(
    breaks = seq(0, 5),
    labels = c("0", "1", "2", "3", "4", "5+")
  ) +
  scale_y_continuous(n.breaks = 7) +
  scale_fill_manual(values = c(col_ideation[4], col_ideation[1])) +
  scale_color_manual(
    values = c(col_ideation[4], col_ideation[2]), guide = "none"
  ) +
  theme_classic() +
  theme(
    strip.text = element_text(size = 12),
    panel.grid.major.y = element_line(color = "#bebebec7"),
    legend.position = "none"
  )

ggsave(plot_reply_n,
  filename = paste0(output, "plot_reply_n.png"),
  scale = 1.1, width = 8, height = 5
)


# ---- 3. share of antisemitic replies to (non) antisemitic comments ----
data_reply_antisem <- data_source |>
  mutate(,
    threat_id = ifelse(comment_level == 0, 1, 0) |> cumsum(),
    com_antisem = case_when(
      str_detect(comment_ideation, "I0") ~ 0,
      str_detect(comment_ideation, "I1") ~ 1,
      .default = NA
    ),
    .after = comment_level,
    .by = document
  ) |>
  mutate(
    com_antisem_first = ifelse(row_number() == 1, first(com_antisem), NA),
    com_antisem_reply = ifelse(row_number() > 1, com_antisem, NA),
    reply_antisem_mean = mean(com_antisem_reply, na.rm = TRUE),
    .by = c(document, threat_id)
  ) |>
  # keep just first comment and comments with replies
  filter(!is.na(com_antisem_first), !is.na(reply_antisem_mean)) |>
  mutate(
    com_antisem_n = n(),
    com_antisem =
      ifelse(com_antisem == 0,
        paste0("non-antisemitic (N:", com_antisem_n, ")"),
        paste0("antisemitic (N:", com_antisem_n, ")")
      ),
    .by = com_antisem
  )

data_reply_antisem_mean <- summarize(
  data_reply_antisem,
  mean = mean(reply_antisem_mean), .by = com_antisem
)

plot_reply_antisem <- data_reply_antisem |>
  ggplot(aes(
    x = reply_antisem_mean,
    group = com_antisem,
    fill = com_antisem
  )) +
  geom_density(alpha = 0.6, adjust = 0.5) +
  geom_vline(
    data = data_reply_antisem_mean,
    aes(xintercept = mean, color = com_antisem)
  ) +
  geom_text(
    data = data_reply_antisem_mean,
    aes(x = mean + 0.025, y = 25, label = round(mean, 2))
  ) +
  scale_fill_manual(
    values = c(col_ideation[4], col_ideation[2]), name = "Initial comment"
  ) +
  scale_color_manual(
    values = c(col_ideation[4], col_ideation[2]), guide = "none"
  ) +
  labs(x = "proportion of antisemitic replies", y = "density") +
  theme_classic() +
  theme(legend.position = c(.8, .8))

ggsave(plot_reply_antisem,
  filename = paste0(output, "plot_reply_antisem.png"),
  scale = 1.1, width = 8, height = 5
)

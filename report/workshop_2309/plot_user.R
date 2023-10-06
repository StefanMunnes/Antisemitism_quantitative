test <- data_source |>
  filter(!is.na(comment_antisem), !is.na(comment_user)) |>
  count(country, source_type, document, comment_user, comment_antisem) |>
  mutate(
    different_idea_n = n_distinct(comment_antisem, na.rm = TRUE) - 1,
    antisem_min_once = max(comment_antisem) == 1,
    user_coms_n = sum(n),
    user_coms_gr_n = ifelse(user_coms_n >= 6, 6, user_coms_n),
    .by = c(document, comment_user)
  )


test2 <- test |>
  filter(row_number() == 1, .by = c(document, comment_user)) |>
  mutate(coms_mean_all = mean(user_coms_n), .by = antisem_min_once) |>
  summarize(
    n = n(), coms_mean_all = first(coms_mean_all),
    .by = c(antisem_min_once, user_coms_gr_n)
  ) |>
  mutate(
    n_sum = sum(n),
    prop = n / n_sum,
    prop_pos = prop / 2,
    .by = antisem_min_once,
  ) |>
  mutate(
    antisem_min_once = ordered(antisem_min_once,
      levels = c(FALSE, TRUE),
      labels = c(
        paste0("no antisemitic comments (N: ", max(n_sum), ")"),
        paste0("min. one antisemetic comment (N: ", min(n_sum), ")")
      )
    )
  )

plot_user_coms_n <- test2 |>
  ggplot(aes(x = user_coms_gr_n, y = prop, fill = antisem_min_once)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = prop_pos, label = round(prop, 2))) +
  geom_vline(aes(xintercept = coms_mean_all, color = antisem_min_once)) +
  geom_text(
    aes(
      x = coms_mean_all, y = 0.75,
      label = round(coms_mean_all, 2)
    ),
    hjust = -0.2
  ) +
  facet_grid(. ~ antisem_min_once) +
  labs(x = "# of comments", y = "proportion") +
  scale_x_continuous(
    breaks = seq(0, 6),
    labels = c("0", "1", "2", "3", "4", "5", "6+")
  ) +
  scale_y_continuous(n.breaks = 7) +
  scale_fill_manual(values = c(col_ideation[1], col_ideation[4])) +
  scale_color_manual(
    values = c(col_ideation[2], col_ideation[4]), guide = "none"
  ) +
  theme_classic() +
  theme(
    strip.text = element_text(size = 12),
    panel.grid.major.y = element_line(color = "#bebebec7"),
    legend.position = "none"
  )

ggsave(plot_user_coms_n,
  filename = paste0(output, "plot_user_coms_n.png"),
  scale = 0.8, width = 10, height = 5
)


test3 <- test |>
  filter(antisem_min_once, user_coms_n > 1, comment_antisem == 1) |>
  mutate(
    prop_antisem = n / user_coms_n,
    # user_coms_gr_n = factor(user_coms_gr_n,
    #   labels = c("2", "3", "4", "5", "6+")
    # )
  ) |>
  mutate(
    mean_all = mean(prop_antisem),
    n_gr = n(),
    user_coms_gr_n = ifelse(user_coms_gr_n == 6, "6+", user_coms_gr_n) |>
      paste0(" (N: ", n_gr, ")"),
    .by = user_coms_gr_n
  )

plot_user_coms_prop <- test3 |>
  ggplot(aes(x = prop_antisem, fill = user_coms_gr_n)) +
  geom_density(alpha = 0.6, adjust = 0.5) +
  geom_vline(
    aes(xintercept = mean_all, color = user_coms_gr_n)
  ) +
  geom_text(
    aes(
      x = mean_all + 0.15, y = 11,
      label = round(mean_all, 2), color = user_coms_gr_n
    )
  ) +
  facet_grid(~user_coms_gr_n) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  labs(x = "proportion of antisemitic comments", y = "density") +
  theme_classic() +
  theme(
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 6),
    panel.grid.major.y = element_line(color = "#bebebec7"),
    legend.position = "none"
  )

ggsave(plot_user_coms_prop,
  filename = paste0(output, "plot_user_coms_prop.png"),
  scale = 0.8, width = 10, height = 5
)




install.packages("gender")
library(gender)


gender(a$comment_user[1:100])

?str_split

mutate(test3[1:100, ], forename = str_split_i(comment_user, " ", 1)) |>
  pull(forename) |>
  gender()

b <- filter(data_source, str_detect(comment_user, "Bundestagswahl"))

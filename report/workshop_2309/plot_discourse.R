data_comments <- read_csv("data/data_level_comments.csv") |>
  mutate(comment_ideation = factor(comment_ideation,
    levels = c("I0", "I0c", "I1", "I1ASC", "I1a")
  ))


# number of comments for each discourse event per country

data_discourse <- data_comments |>
  summarize(comments = n(), .by = c("country", "discourse")) |>
  mutate(country_n = sum(comments), .by = country) |>
  mutate(
    country_n = paste(country, country_n, sep = "\n"),
    discourse_n = paste(discourse, comments, sep = "\n")
  )


png(paste0(output, "plot_treemap_discourse.png"), width = 800, height = 350)

treemap(data_discourse,
  index = c("country_n", "discourse_n"),
  vSize = "comments",
  palette = "Set2",
  fontsize.labels = c(18, 15),
  fontcolor.labels = "black",
  force.print.labels = TRUE,
  align.labels = list(c("left", "top"), c("center", "center")),
  bg.labels = 0,
  title = "",
  aspRatio = 2.3
)

dev.off()



# ---- 2.1 different ideations for each discourse events per country ----
data_ideation_discourse <- data_comments |>
  summarize(n = n(), .by = c(country, discourse, comment_ideation)) |>
  filter(!is.na(comment_ideation), comment_ideation != "I2") |>
  mutate(
    sum = sum(n),
    prop = n / sum,
    .by = c(country, discourse)
  ) |>
  mutate(discourse = str_replace(discourse, "_", "\n"))

data_antisem_country <- data_source |>
  filter(!is.na(comment_ideation), comment_ideation != "I2") |>
  mutate(antisem = comment_ideation %in% c("I1", "I1ASC", "I1a")) |>
  summarize(prop_antisem = mean(antisem), .by = country)


plot_ideation_discourse <- ggplot() +
  geom_bar(
    data = data_ideation_discourse,
    aes(x = discourse, y = prop, fill = comment_ideation),
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
    axis.text.x = element_text(size = 10),
    strip.text = element_text(size = 12),
    panel.grid.major.y = element_line(color = "grey")
  )

ggsave(plot_ideation_discourse,
  filename = paste0(output, "plot_ideation_discourse.png"),
  scale = 1.2, width = 11.5, height = 4.5
)


# ---- 2.2 compare just 4 discourse events same in all 3 countries ----
discourse_same <- count(data_discourse, discourse) |>
  filter(n == 3) |>
  pull(discourse)

data_ideation_discourse_same <- data_comments |>
  mutate(discourse = str_replace(discourse, "21_AICN", "21_AIC")) |>
  filter(discourse %in% discourse_same) |>
  mutate(discourse = str_replace(discourse, "21_AIC", "21_AIC(N)")) |>
  summarize(n = n(), .by = c(country, discourse, comment_ideation)) |>
  filter(!is.na(comment_ideation), comment_ideation != "I2") |>
  group_by(country, discourse) |>
  arrange(country, discourse, desc(comment_ideation)) |>
  mutate(
    prop = n / sum(n),
    prop_pos = cumsum(prop) - (prop / 2)
  )

plot_ideation_discourse_same <- data_ideation_discourse_same |>
  ggplot(aes(x = country, y = prop, fill = comment_ideation)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(y = prop_pos, x = country, label = na_if(round(prop, 2), 0)),
    size = 3.5, color = "#2e2d2d"
  ) +
  facet_grid(~discourse, scales = "free") +
  scale_fill_manual(values = col_ideation, name = "Ideation") +
  labs(x = "", y = "Proportion", ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 12),
    strip.text = element_text(size = 12),
    panel.grid.major.y = element_line(color = "grey")
  )


ggsave(plot_ideation_discourse_same,
  filename = paste0(output, "plot_ideation_discourse_same.png"),
  scale = 1.2, width = 10, height = 4.5
)

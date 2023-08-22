nominations_topcis <- readRDS("../data/nominees_rec.RDS") |>
  filter(row_number() == 1, .by = c(authors, title, ynom)) |>
  select(ynom, starts_with("topic_")) |>
  pivot_longer(cols = !ynom, names_to = "topic", names_prefix = "topic_") |>
  summarize(
    prop = sum(value) / n(),
    N = n(),
    n = sum(value),
    .by = c(ynom, topic)
  )


plot_topics <- ggplot(
  nominations_topcis,
  aes(x = ynom, y = prop, color = topic)
) +
  geom_point(aes(size = n)) +
  geom_smooth(linewidth = 1.5) +
  geom_text(y = -0.03, aes(label = N), size = 5) +
  labs(x = "Year of nomination", y = "Share of topics") +
  theme_bw()


ggsave(
  file = "../output/graphs/plot_topics.png",
  plot = plot_topics, dpi = 600, scale = 0.9, height = 10, width = 12
)

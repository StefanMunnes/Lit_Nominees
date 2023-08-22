colors_5 <- c("#b37cfd", "#71bec3", "#c08402", "#8cac1e", "#d8746b")

# ---- 1. coefplot with Average Marginal Effects ----

data_log <- lapply(margins_log[1:5], summary) |>
  bind_rows(.id = "model") |>
  mutate(term = factor) |>
  rename(
    estimate = AME,
    std.error = SE
  ) |>
  filter(str_detect(factor, "senti_")) |>
  relabel_predictors(coef_labs) |>
  mutate(term = as.character(term))


plot_log <- data_log |>
  dwplot(
    vline = geom_vline(
      xintercept = 0,
      colour = "grey60",
      linetype = 2
    ),
    dodge_size = .9,
    style = c("dotwhisker"),
    whisker_args = list(size = 1),
    dot_args = list(size = 10, aes(shape = model)),
    model_order = names(margins_log[1:5])
  ) +
  ggtitle("Predicting Winners by Quality") +
  xlab("Average Marginal Effects") + ylab("") +
  scale_x_continuous(breaks = seq(-0.1, 0.25, 0.05)) +
  scale_color_manual(
    values = colors_5,
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_shape_manual(values = c(18, 17, 16, 15, 25)) +
  guides(
    shape = guide_legend("Model", reverse = TRUE),
    colour = guide_legend("Model", reverse = TRUE)
  ) +
  theme_bw(base_size = 25) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.margin = margin(1, 1, 1, 1),
    axis.text.x = element_text(size = 12),
    panel.grid.minor = element_blank()
  )
plot_log

ggsave(
  file = "../output/graphs/coefplot_ame_quali.png",
  plot = plot_log, dpi = 600, scale = 1.2, height = 11, width = 15
)

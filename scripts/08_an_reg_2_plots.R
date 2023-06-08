# ---- 1. coefplot with Average Marginal Effects ----

plot_groups <- list(
  c(
    "Quality", "No review available (ref. clearly low)",
    "Clearly high quality"
  ),
  c("Topics/Zeitgeist", "History", "Culture"),
  c(
    "Prominence", "# previous books (ref. median <= 5.0)",
    "# reviews (ref. median <= 4.0)"
  ),
  c("Demogr.", "Female", "Non-German native speaker")
)


data_log <- lapply(margins_log, summary) |>
  bind_rows(.id = "model") |>
  mutate(term = factor) |>
  rename(
    estimate = AME,
    std.error = SE
  ) |>
  filter(
    !str_detect(term, "After|jury"),
    term != "debut",
    term != "nonfiction"
  ) |>
  # remove coeficients for other models -> just one coefficient (except quality)
  # filter(row_number() == 1 | str_detect(term, "senti_qual"), .by = term) |>
  relabel_predictors(coef_labs) |>
  mutate(
    term = as.character(term),
    # confidence intervalls to zero
    upper = estimate,
    lower = estimate,
    std.error = 0
  )


plot_log <- data_log |>
  dwplot(
    vline = geom_vline(
      xintercept = 0,
      colour = "grey60",
      linetype = 2
    ),
    dodge_size = 0.8,
    style = c("dotwhisker"),
    dot_args = list(size = 3.8, aes(shape = model)),
    model_order = c("Quality", "+ Zeitgeist", "+ Prominence", "+ Demographics")
  ) +
  ggtitle("Predicting Winners") +
  xlab("Average Marginal Effects") + ylab("") +
  scale_x_continuous(breaks = seq(-0.04, 0.16, 0.02)) +
  scale_color_discrete(guide = guide_legend(reverse = TRUE)) +
  scale_shape_manual(values = c(18, 17, 16, 15)) +
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

plot_log <- plot_log |>
  add_brackets(plot_groups, fontSize = 1.4)

# plot_log

ggsave(
  file = "../output/graphs/coefplot_log_models_qual.png",
  plot = plot_log, dpi = 600, scale = 1.2, height = 9, width = 15
)



# ---- 2. diff for quality coefficients ----
data_log_diff <- data_log |>
  filter(str_detect(factor, "senti_qual")) |>
  select(model, factor, term, estimate) |>
  # wide format to have models as variables for easy calculation
  pivot_wider(names_from = model, values_from = "estimate") |>
  # distract model coefficient from just quality coeffs to get diff
  mutate(across(starts_with("+"), ~ . - Quality, .names = "diff_{col}")) |>
  # bring back to long format for plotting
  pivot_longer(starts_with("diff"), names_to = "model", values_to = "diff") |>
  mutate(model = str_remove(model, "diff_"))


plot_log_diff <- data_log_diff |>
  ggplot(aes(y = term, x = diff, fill = model)) +
  geom_bar(
    stat = "identity",
    position = position_dodge2()
  ) +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("Difference in AME's to base model") +
  xlab("Percentage Points") +
  ylab("Quality") +
  guides(fill = guide_legend("Model", reverse = TRUE)) +
  scale_x_continuous(breaks = seq(-0.04, 0.02, 0.01)) +
  theme_bw(base_size = 25) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.margin = margin(1, 1, 1, 1),
    panel.grid.minor = element_blank()
  )

# plot_log_diff

ggsave(
  file = "../output/graphs/diffplot_log_qual.png",
  plot = plot_log_diff, dpi = 600, scale = 1.2, height = 9, width = 15
)

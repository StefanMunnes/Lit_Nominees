# ---- 1. coefplot with Average Marginal Effects ----

plot_groups <- list(
  c(
    "Quality", "# Reviews (ref. median <= 4.0)",
    "Clearly high Quality (ref. clearly low)"
  ),
  c(
    "Prominence", "# Previous Books (ref. median <= 5.0)",
    "Wikipedia Views (ref. median <= 8.6)"
  ),
  c("Demogr.", "Female", "German background"),
  c("Topics", "History", "Dummy: Nonfiction")
)


plot_log <- summary(margins_log) |>
  rename(
    term = factor,
    estimate = AME,
    std.error = SE
  ) |>
  filter(!str_detect(term, "After|jury"), term != "debut") |>
  dwplot(
    vline = geom_vline(
      xintercept = 0,
      colour = "grey60",
      linetype = 2
    ),
    dodge_size = 0.4,
    style = c("dotwhisker"),
    dot_args = list(size = 5),
    whisker_args = list(size = 1),
    line_args = list(alpha = 0.75, size = 2)
  ) |>
  relabel_predictors(coef_labs) +
  theme_bw(base_size = 20) +
  xlab("Average Marginal Effects") + ylab("") +
  ggtitle("Predicting Winners") +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )


plot_log <- plot_log |>
  add_brackets(plot_groups, fontSize = 1.3)
  
plot_log

ggsave(
  file = "../output/graphs/coefplot_log_short.png",
  plot = plot_log, dpi = 500, scale = 1.15, height = 8, width = 15
)


# ---- 2. interaction effects ----

predicts_2_df <- function(vars,
                          mod = models_log$"Full Model",
                          vcov_mat = cl_vcov_mat) {
  at_list <- list(
    unique(nominees_an[vars[1]]) |> dplyr::pull(),
    unique(nominees_an[vars[2]]) |> dplyr::pull()
  )

  names(at_list) <- c(vars[1], vars[2])

  df <- margins::prediction(
    model = mod,
    calculate_se = TRUE,
    vcov = vcov_mat,
    at = at_list
  ) |>
    summary() |>
    as.data.frame()

  names(df) <- c(vars[1], vars[2], names(df[3:8]))

  return(df)
}


#
plot_opts <- list(
  geom_pointrange(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(.2)
  ),
  geom_hline(yintercept = 0, alpha = 0.5),
  scale_y_continuous(breaks = seq(-0.0, 0.5, 0.1), limits = c(-0.05, 0.55)),
  xlab(""),
  theme_bw(base_size = 20),
  theme(
    legend.title = element_blank(),
    legend.justification = c(1.2, 1.3),
    legend.position = c(1, 1),
    legend.background = element_blank(),
    plot.title = element_text(size = 20),
    panel.grid.major.x = element_blank()
  )
)


#
plot1 <- predicts_2_df(c("female", "metoo")) |>
  ggplot(aes(
    x = metoo, y = Prediction,
    color = factor(female, labels = c("Male", "Female"))
  )) +
  plot_opts +
  scale_color_brewer(type = "qual", palette = 2) +
  ggtitle("Female x #metoo")


#
plot2 <- predicts_2_df(c("language_german", "syria")) |>
  ggplot(aes(
    x = syria, y = Prediction,
    color = factor(language_german, labels = c("Non-German", "German"))
  )) +
  plot_opts +
  scale_color_manual(values = c("#7570b3", "#e7298a")) +
  ggtitle("German background x Migration") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank()
  )

#
plot3 <- predicts_2_df(c("female", "jury_group")) |>
  mutate(jury_group = forcats::fct_relevel(
    jury_group, "more male", "even", "more female"
  )) |>
  ggplot(aes(
    x = jury_group, y = Prediction,
    color = factor(female, labels = c("Male", "Female"))
  )) +
  plot_opts +
  scale_color_brewer(type = "qual", palette = 2) +
  ggtitle("Female x Jury Composition") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank()
  )


p_load("ggpubr")

plots_interactions <- ggarrange(plot1, plot2, plot3,
  nrow = 1
)


ggsave(
  file = "../output/graphs/interactions_log_predictions.png",
  plot = plots_interactions, dpi = 500, scale = 1.3, height = 8, width = 15
)
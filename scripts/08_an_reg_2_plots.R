pacman::p_load("dotwhisker", "ggpubr")


# ---- 1. coefplot with Average Marginal Effects ----

plot_groups <- list(
  c("Demogr.", "Female", "Non-German background"),
  c(
    "Prominence", "# previous books (ref. median <= 5.0)",
    "Wikipedia views (ref. median <= 8.6)"
  ),
  c("Zeitgeist", "History", "Culture"),
  c(
    "Quality", "# reviews (ref. median <= 4.0)",
    "Clearly high quality"
  )
)


data_log <- lapply(margins_log, summary) |>
  bind_rows(.id = "model") |>
  rename(
    term = factor,
    estimate = AME,
    std.error = SE
  ) |>
  filter(
    !str_detect(term, "After|jury"),
    term != "debut",
    term != "nonfiction"
  ) |>
  relabel_predictors(coef_labs) |>
  mutate(
    term = as.character(term),
    model = case_when(
      model == "M1" ~ "Demographics",
      model == "M2" ~ "+ Prominence",
      model == "M3" ~ "+ Zeitgeist",
      model == "M4" ~ "+ Quality"
    ),
    # confidence intervalls to zero
    upper = estimate,
    lower = estimate,
    std.error = 0,
    # shape = as.numeric(factor(Models))
  )


plot_log <- data_log |>
  dwplot(
    vline = geom_vline(
      xintercept = 0,
      colour = "grey60",
      linetype = 2
    ),
    dodge_size = 0.9,
    # model_name = "Models",
    style = c("dotwhisker"),
    dot_args = list(size = 3.5),
    whisker_args = list(size = 0.75),
    line_args = list(alpha = 0.75, size = 12)
  ) +
  theme_bw(base_size = 20) +
  # guides(colour = guide_legend("Models")) +
  xlab("Average Marginal Effects") + ylab("") +
  ggtitle("Predicting Winners") +
  theme(
    plot.title = element_text(face = "bold"),
    plot.margin = margin(1, 1, 1, 1)
  )

plot_log <- plot_log |>
  add_brackets(plot_groups, fontSize = 1.3)

# plot_log

ggsave(
  file = "../output/graphs/coefplot_log_short_multimodel.png",
  plot = plot_log, dpi = 600, scale = 1.15, height = 9, width = 15
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
  scale_y_continuous(breaks = seq(-0.1, 0.5, 0.1), limits = c(-0.1, 0.55)),
  xlab(""),
  theme_bw(base_size = 20),
  theme(
    legend.title = element_blank(),
    legend.justification = c(1.1, 1.25),
    legend.position = c(1, 1),
    legend.background = element_blank(),
    plot.title = element_text(size = 20),
    panel.grid.major.x = element_blank()
    # legend.background = element_rect(fill='transparent'),
    # legend.key = element_blank(),
    # panel.background = element_rect(fill = "transparent", colour = NA),
    # legend.box.background = element_rect(fill='transparent'),
    # plot.background = element_rect(fill = "transparent", colour = NA)
  )
)


# Plot1: Female x #metoo
plot1 <- predicts_2_df(c("female", "metoo")) |>
  ggplot(aes(
    x = metoo, y = Prediction,
    color = factor(female, labels = c("Male", "Female"))
  )) +
  plot_opts +
  scale_color_brewer(type = "qual", palette = 2) +
  ggtitle("Female x #metoo")


# Plot2: German background x Migration
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

# Plot3: Female x Jury Composition
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


plots_interactions <- ggarrange(plot1, plot2, plot3, nrow = 1)

ggsave(
  file = "../output/graphs/interactions_log_predictions.png",
  plot = plots_interactions, dpi = 500, scale = 1.3, height = 8, width = 15
)

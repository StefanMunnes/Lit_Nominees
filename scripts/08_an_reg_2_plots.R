plot_groups <- list(
  c(
    "Quality", "# Reviews (ref. median <= 4.0)",
    "Clearly high Quality (ref. clearly low)"
  ),
  c(
    "Prominence", "# Previous Books (ref. median <= 5.0)",
    "Wikipedia Views (ref. median <= 8.6)"
  ),
  c("Topics", "History", "Dummy: Nonfiction"),
  c("Zeitgeist/Jury", "Female", "Jury male dominated (ref. even)")
)


plot_log <- summary(margins_log) |>
  rename(
    term = factor,
    estimate = AME,
    std.error = SE
  ) |>
  filter(!str_detect(term, ":|After"), term != "debut") |>
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
  theme_bw(base_size = 20) + xlab("Average Marginal Effects") + ylab("") +
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



predicts_2_df <- function(vars,
                          mod = models_log[[6]], vcov_mat = cl_vcov_mat) {
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



predicts_2_df(c("female", "jury_group")) |>
  ggplot(aes(x = jury_group, y = Prediction, color = as.factor(female))) +
  geom_pointrange(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(.2)
  )

# female * metoo
# language_german * syria
# homophily * metoo


################################################################################

model_log <- glm(model_formulars[[6]], data = nominees_an, family = "binomial")

margins <- margins_summary(model_log)

ggplot(margins[margins$factor != "female", ], aes(y = factor, x = AME)) +
  geom_pointrange(
    aes(xmin = lower, xmax = upper)
  )

cl_vcov_mat_log <- vcovCL(model_log, cluster = ~prize)


predicts_2_df(
  c("jury_preference", "metoo"),
  mod = model_log, vcov_mat = cl_vcov_mat_log
) |>
  ggplot(aes(x = metoo, y = Prediction, color = as.factor(jury_preference))) +
  geom_pointrange(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(.2)
  )




################################################################################

models_log <- lapply(model_formulars, function(m) {
  glm(m, data = nominees, family = "binomial")
})


ggemmeans(models[[6]], c("female", "metoo"))

ggemmeans(models_log[[6]], c("female", "metoo", "homophily"))
ggemmeans(models[[6]], c("female", "metoo"))




models <- lapply(model_formulars, function(m) {
  glm(m, data = nominees, family = binomial())
})


a <- ggpredict(
  models[[1]],
  c("revs_n_cat") # , "senti_mean_cat", "senti_vari_cat", "debut"
)

b <- ggemmeans(
  models[[1]],
  c("revs_n_cat")
)

b <-
  a <- model_parameters(models[[6]])
b <- compare_parameters(models)

margins(models[[6]], at = list(revs_n_cat = levels(nominees$revs_n_cat)))




a <- glm(model_formulars[[1]], data = nominees, family = "binomial")
# coeftest(vcov = vcovCL, cluster = ~prize)
b <- margins(a, variables = "debut", at = list(revs_n = c(1, 5)))


a <- glm(model_formulars[[6]], data = nominees, family = "binomial")
b <- margins(a, at = list(fsenti_mean = seq(1, 7, 0.5)), over = "female")

b <- predict(
  lm(winner ~ revs_n + debut, data = nominees),
  newdata = data.frame(
    revs_n = c(1, 1, 3, 3, 6, 6, 9, 9),
    debut = c(FALSE, TRUE)
  ),
  interval = "confidence"
)

summary(glm(winner ~ debut, data = nominees, family = "binomial"))
summary(margins(glm(winner ~ debut, data = nominees, family = "binomial")))


a <- glm(model_formulars[[1]], data = nominees, family = "binomial")
d <- predict(a, interval = "confidence")

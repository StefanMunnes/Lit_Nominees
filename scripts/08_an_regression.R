pacman::p_load("lmtest", "sandwich", "margins", "broom", "dotwhisker")

pacman::p_load("ggeffects", "parameters")


nominees_an <- readRDS("../data/nominees_rec.RDS") |>
  mutate(across(where(is.logical), as.numeric))


model_formulars <- c(
  "Quality" = winner ~ revs_n_cat + senti_qual_cat + debut,
  "Prominence" = winner ~ books_dnb_prev_cat + wikiprizes_pre_cat +
    nom_prize_prev + pub_reputation_mean_cat + wv_mean_cat + debut,
  "Topics" = winner ~ topic_history + topic_culture + topic_identity +
    topic_politics + topic_relations + debut,
  "Zeitgeist" = winner ~ female * metoo + language_german * syria + debut,
  "Jury" = winner ~ metoo * homophily + debut,
  "Full Model" = winner ~ revs_n_cat + senti_qual_cat +
    books_dnb_prev_cat + wikiprizes_pre_cat + nom_prize_prev +
    pub_reputation_mean_cat + wv_mean_cat +
    topic_history + topic_culture + topic_identity + topic_politics +
    topic_relations + female * metoo + language_german * syria +
    metoo * homophily + debut + nonfiction
)

models_lm <- lapply(model_formulars, function(m) {
  lm(m, data = nominees_an)
})


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
  c("Zeitgeist/Jury", "Female", "Jury Homophily")
)

plot_lm <- models_lm[[6]] |>
  coeftest(vcov = vcovCL, cluster = ~prize) |>
  broom::tidy() |>
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
  relabel_predictors(
    c(
      "revs_n_cat> median (4.0)" =
        "# Reviews (ref. median <= 4.0)",
      "senti_qual_catnone" =
        "No review available (ref. clearly low)",
      "senti_qual_catdisputed low" =
        "Disputed low Quality (ref. clearly low)",
      "senti_qual_catdisputed high" =
        "Disputed high Quality (ref. clearly low)",
      "senti_qual_catclearly high" =
        "Clearly high Quality (ref. clearly low)",
      "books_dnb_prev_cat> median (5.0)" =
        "# Previous Books (ref. median <= 5.0)",
      "wikiprizes_pre_cat> median (4.0)" =
        "# Previous Prizes (ref. median <= 4.0)",
      "nom_prize_prev" = "Previously nominated (ref. not)",
      "pub_reputation_mean_cat> median (4.4)" =
        "High Publisher Reputation (ref. median <= 4.4)",
      "wv_mean_cat> median (8.6)" = "Wikipedia Views (ref. median <= 8.6)",
      topic_history = "History",
      topic_politics = "Politics",
      topic_relations = "Relations",
      topic_identity = "Identity",
      topic_culture = "Culture",
      nonfiction = "Dummy: Nonfiction",
      female = "Female",
      language_german = "German Background",
      homophily = "Jury Homophily"
    )
  ) +
  theme_bw(base_size = 20) + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("Predicting Winners") +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )


plot_lm <- plot_lm |>
  add_brackets(plot_groups, fontSize = 1.3)

plot_lm

ggsave(
  file = "../output/graphs/coefplot_lm_short.png",
  plot = plot_lm, dpi = 500, scale = 1.15, height = 8, width = 15
)


stargazer(models_lm,
  title = "Linear Probability Model",
  column.labels = names(models_lm),
  dep.var.labels = c("Winner"),
  omit = c("debut"),
  covariate.labels = c(
    "# Reviews (ref. mean < 4.1)",
    "Clearly low Quality (ref. none)",
    "Disputed low Quality (ref. none)",
    "Disputed high Quality (ref. none)",
    "Clearly high Quality (ref. none)",
    "High Publisher Reputation (ref. low)",
    "# Previous Books (ref. mean < 9.0)",
    "# Previous Prizes (ref. mean < 5.7)",
    "Previously nominated (ref. not)",
    "Wikipedia Views (ref. mean < 20.2)",
    "Topic: History", "Topic: Politics", "Topic: Relations",
    "Topic: Identity", "Topic: Culture",
    "Female", "After Metoo", "German Background", "After 2015",
    "Jury Homophily", "Nonfiction", "Female x After #metoo",
    "German background x After 2015", "Jury Homophily x After #metoo"
  ),
  align = TRUE,
  omit.stat = "all",
  notes = "Standard errors in parentheses. All models include control for debut prize",
  type = "html",
  out = "../output/tables/regression.htm"
)




cl_vcov_mat <- vcovCL(models_lm[[6]], cluster = ~prize)


predicts_2_df <- function(vars,
                          mod = models_lm[[6]], vcov_mat = cl_vcov_mat) {
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



predicts_2_df(c("language_german", "syria")) |>
  ggplot(aes(x = syria, y = Prediction, color = as.factor(language_german))) +
  geom_pointrange(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(.2)
  )

# female * metoo
# language_german * syria
# homophily * metoo


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

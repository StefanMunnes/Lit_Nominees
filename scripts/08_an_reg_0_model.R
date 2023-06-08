nominees_an <- readRDS("../data/nominees_rec.RDS") |>
  mutate(across(where(is.logical), as.numeric))


coef_labs <- c(
  "senti_qual_catnone" =
    "No review available (ref. clearly low)",
  "senti_qual_catdisputed low" =
    "Disputed low quality",
  "senti_qual_catdisputed high" =
    "Disputed high quality ",
  "senti_qual_catclearly high" =
    "Clearly high quality",
  "topic_history" = "History",
  "topic_politics" = "Politics",
  "topic_relations" = "Relations",
  "topic_identity" = "Identity",
  "topic_culture" = "Culture",
  "books_dnb_prev_cat> median (5.0)" =
    "# previous books (ref. median <= 5.0)",
  "wikiprizes_pre_cat> median (4.0)" =
    "# previous prizes (ref. median <= 4.0)",
  "nom_prize_prev" = "Previously unawarded nominated (ref. not)",
  "pub_reputation_mean_cat> median (4.4)" =
    "High publisher reputation (ref. median <= 4.4)",
  "wv_mean_cat> median (8.6)" = "Wikipedia views (ref. median <= 8.6)",
  "revs_n_cat> median (4.0)" =
    "# reviews (ref. median <= 4.0)",
  "female" = "Female",
  "age_nom_cat> median (43.0)" = "Higher age (ref. median <= 43.0)",
  "language_nongerman" = "Non-German native speaker"
)


model_formulars <- c(
  "Quality" = winner ~ senti_qual_cat + debut,
  "+ Zeitgeist" = winner ~ senti_qual_cat + debut +
    topic_history + topic_culture +
    topic_identity + topic_politics + topic_relations,
  "+ Prominence" = winner ~ senti_qual_cat + debut +
    topic_history + topic_culture +
    topic_identity + topic_politics + topic_relations +
    books_dnb_prev_cat + wikiprizes_pre_cat +
    nom_prize_prev + pub_reputation_mean_cat + wv_mean_cat + revs_n_cat,
  "+ Demographics" = winner ~ senti_qual_cat + debut +
    topic_history + topic_culture +
    topic_identity + topic_politics + topic_relations +
    books_dnb_prev_cat + wikiprizes_pre_cat +
    nom_prize_prev + pub_reputation_mean_cat + wv_mean_cat + revs_n_cat +
    female + age_nom_cat + language_nongerman
)


models_log <- lapply(model_formulars, function(m) {
  glm(m, data = nominees_an, family = "binomial")
})

# old -> needed in interaction plots
# cl_vcov_mat <- vcovCL(models_log$"M4",
#   cluster = ~prize
# )

margins_log <- lapply(models_log, function(model) {
  margins(model, vcov = vcovCL(model, cluster = ~prize))
})

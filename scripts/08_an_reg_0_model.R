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
  "revs_n_cat> median (4.0)" =
    "# reviews (ref. median <= 4.0)",
  "revs_n" = "# reviews",
  "books_dnb_prev_cat> median (5.0)" = "# previous books (ref. median <= 5.0)",
  "books_dnb_prev" = "# previous books",
  "wikiprizes_pre_cat> median (4.0)" =
    "# previous prizes (ref. median <= 4.0)",
  "wikiprizes_pre" = "# previous prizes",
  "nom_prize_prev" = "Previously nominated (ref. not)",
  "pub_reputation_mean_cat> median (4.4)" =
    "High publisher reputation (ref. median <= 4.4)",
  "pub_reputation_mean" = "Publisher reputation",
  "wv_mean_cat> median (8.5)" = "Wikipedia views (ref. median <= 8.5)",
  "wv_mean" = "Wikipedia views",
  "topic_history" = "History",
  "topic_politics" = "Politics",
  "topic_relations" = "Relations",
  "topic_identity" = "Identity",
  "topic_culture" = "Culture",
  "female" = "Female",
  "age_nom_cat> median (43.0)" = "Higher age (ref. median <= 43.0)",
  "age_nom" = "Age",
  "language_nongerman" = "Non-native German speaker"
)


model_formulars <- c(
  "Quality" = winner ~ debut + senti_qual_cat,
  "+ # Reviews" = winner ~ debut + senti_qual_cat +
    revs_n_cat,
  "+ Prominence" = winner ~ debut + senti_qual_cat +
    revs_n_cat +
    books_dnb_prev_cat + wikiprizes_pre_cat +
    nom_prize_prev + pub_reputation_mean_cat + wv_mean_cat,
  "+ Zeitgeist" = winner ~ debut + senti_qual_cat +
    revs_n_cat +
    books_dnb_prev_cat + wikiprizes_pre_cat +
    nom_prize_prev + pub_reputation_mean_cat + wv_mean_cat +
    topic_history + topic_culture +
    topic_identity + topic_politics + topic_relations,
  "+ Demographics" = winner ~ debut + senti_qual_cat +
    revs_n_cat +
    topic_history + topic_culture +
    topic_identity + topic_politics + topic_relations +
    books_dnb_prev_cat + wikiprizes_pre_cat +
    nom_prize_prev + pub_reputation_mean_cat + wv_mean_cat +
    female + age_nom_cat + language_nongerman,
  "+ Prize FE" = winner ~ debut + senti_qual_cat +
    revs_n_cat +
    topic_history + topic_culture +
    topic_identity + topic_politics + topic_relations +
    books_dnb_prev_cat + wikiprizes_pre_cat +
    nom_prize_prev + pub_reputation_mean_cat + wv_mean_cat +
    female + age_nom_cat + language_nongerman +
    prize_deutscher + prize_leipziger +
    prize_oesterreich + prize_oesterreich_debuet + prize_schweiz,
  "Continuous IVs" = winner ~ debut + senti_qual_cat +
    revs_n +
    topic_history + topic_culture +
    topic_identity + topic_politics + topic_relations +
    books_dnb_prev + wikiprizes_pre +
    nom_prize_prev + pub_reputation_mean + wv_mean +
    female * jury_group + female * metoo + age_nom +
    language_nongerman * syria
)



models_log <- lapply(model_formulars, function(m) {
  glm(m, data = nominees_an, family = "binomial")
})


margins_log <- lapply(models_log, function(model) {
  margins(model, vcov = vcovCL(model, cluster = ~prize))
})

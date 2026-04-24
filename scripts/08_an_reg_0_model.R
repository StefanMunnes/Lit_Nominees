nominees_an <- readRDS("../data/nominees_rec.RDS") |>
  mutate(across(where(is.logical), as.numeric))


coef_labs <- c(
  "senti_qual_catnone" = "none (ref. negative)",
  "senti_qual_catdisputed low" = "disputed negative",
  "senti_qual_catdisputed high" = "disputed positive ",
  "senti_qual_catclearly high" = "clearly positive",
  "revs_n_cat> median (4.0)" = "# reviews (> 4)",
  "revs_n" = "# reviews",
  "books_dnb_prev_cat> median (5.0)" = "# previous books (> 5)",
  "books_dnb_prev" = "# previous books",
  "wikiprizes_pre_cat> median (4.0)" = "# previous prizes (> 4)",
  "wikiprizes_pre" = "# previous prizes",
  "nom_prize_prev" = "previously nomination",
  "pub_reputation_mean_cat> median (4.4)" = "publisher status (> 4.4)",
  "pub_reputation_mean" = "publisher status",
  "wv_mean_cat> median (8.5)" = "Wikipedia views (> 8.6)",
  "wv_mean" = "Wikipedia views",
  "topic_history" = "history",
  "topic_politics" = "politics",
  "topic_relations" = "relations",
  "topic_identity" = "identity",
  "topic_culture" = "culture",
  "female" = "female",
  "age_nom_cat> median (43.0)" = "age (> 43)",
  "age_nom" = "age",
  "language_nongerman" = "non-native German speaker",
  "debut" = "debut"
)


model_formulars <- c(
  "Quality" = winner ~ debut + senti_qual_cat,
  "+ # Reviews" = winner ~ debut + senti_qual_cat + revs_n_cat,
  "+ Prominence" = winner ~ debut +
    senti_qual_cat +
    revs_n_cat +
    books_dnb_prev_cat +
    wikiprizes_pre_cat +
    nom_prize_prev +
    pub_reputation_mean_cat +
    wv_mean_cat,
  "+ Zeitgeist" = winner ~ debut +
    senti_qual_cat +
    revs_n_cat +
    books_dnb_prev_cat +
    wikiprizes_pre_cat +
    nom_prize_prev +
    pub_reputation_mean_cat +
    wv_mean_cat +
    topic_history +
    topic_culture +
    topic_identity +
    topic_politics +
    topic_relations,
  "+ Demographics" = winner ~ debut +
    senti_qual_cat +
    revs_n_cat +
    topic_history +
    topic_culture +
    topic_identity +
    topic_politics +
    topic_relations +
    books_dnb_prev_cat +
    wikiprizes_pre_cat +
    nom_prize_prev +
    pub_reputation_mean_cat +
    wv_mean_cat +
    female +
    age_nom_cat +
    language_nongerman,
  "+ Prize FE" = winner ~ debut +
    senti_qual_cat +
    revs_n_cat +
    topic_history +
    topic_culture +
    topic_identity +
    topic_politics +
    topic_relations +
    books_dnb_prev_cat +
    wikiprizes_pre_cat +
    nom_prize_prev +
    pub_reputation_mean_cat +
    wv_mean_cat +
    female +
    age_nom_cat +
    language_nongerman +
    prize_deutscher +
    prize_leipziger +
    prize_oesterreich +
    prize_oesterreich_debuet +
    prize_schweiz,
  "Continuous IVs" = winner ~ debut +
    senti_qual_cat +
    revs_n +
    topic_history +
    topic_culture +
    topic_identity +
    topic_politics +
    topic_relations +
    books_dnb_prev +
    wikiprizes_pre +
    nom_prize_prev +
    pub_reputation_mean +
    wv_mean +
    female * jury_group +
    female * metoo +
    age_nom +
    language_nongerman * syria
)


models_log <- lapply(model_formulars, function(m) {
  glm(m, data = nominees_an, family = "binomial")
})


margins_log <- lapply(models_log, function(model) {
  margins(model, vcov = vcovCL(model, cluster = ~prize))
})

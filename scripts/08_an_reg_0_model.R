pacman::p_load("sandwich", "margins")


nominees_an <- readRDS("../data/nominees_rec.RDS") |>
  mutate(across(where(is.logical), as.numeric))


coef_labs <- c(
  "female" = "Female",
  "age_nom_cat> median (43.0)" = "Higher age (ref. median <= 43.0)",
  "language_german" = "German background",
  "books_dnb_prev_cat> median (5.0)" =
    "# previous books (ref. median <= 5.0)",
  "wikiprizes_pre_cat> median (4.0)" =
    "# previous prizes (ref. median <= 4.0)",
  "nom_prize_prev" = "Previously nominated (ref. not)",
  "pub_reputation_mean_cat> median (4.4)" =
    "High publisher reputation (ref. median <= 4.4)",
  "wv_mean_cat> median (8.6)" = "Wikipedia views (ref. median <= 8.6)",
  "topic_history" = "History",
  "topic_politics" = "Politics",
  "topic_relations" = "Relations",
  "topic_identity" = "Identity",
  "topic_culture" = "Culture",
  # "female:metooAfter #metoo" = "Female x after #metoo",
  # "language_german:syriaAfter 2015" = "German background x after 2015",
  # "syriaAfter 2015" = "After 2015",
  # "metooAfter #metoo" = "After #metoo",
  # "jury_groupmore female" = "Jury female dominated (ref. even)",
  # "jury_groupmore male" = "Jury male dominated (ref. even)",
  # "female:jury_groupmore female" = "Female x jury female dominated (ref. even)",
  # "female:jury_groupmore male" = "Female x jury male dominated (ref. even)",
  # "nonfiction" = "Nonfiction",
  "revs_n_cat> median (4.0)" =
    "# reviews (ref. median <= 4.0)",
  "senti_qual_catnone" =
    "No review available (ref. clearly low)",
  "senti_qual_catdisputed low" =
    "Disputed low quality",
  "senti_qual_catdisputed high" =
    "Disputed high quality ",
  "senti_qual_catclearly high" =
    "Clearly high quality"
)


model_formulars <- c(
  "M1" = winner ~ female + age_nom_cat + language_german + debut,
  "M2" = winner ~ female + age_nom_cat + language_german +
    books_dnb_prev_cat + wikiprizes_pre_cat +
    nom_prize_prev + pub_reputation_mean_cat + wv_mean_cat + debut,
  "M3" = winner ~ female + age_nom_cat + language_german +
    books_dnb_prev_cat + wikiprizes_pre_cat +
    nom_prize_prev + pub_reputation_mean_cat + wv_mean_cat +
    topic_history + topic_culture +
    topic_identity + topic_politics + topic_relations + nonfiction +
    female * metoo + language_german * syria +
    female * jury_group + debut,
  "M4" = winner ~ female + age_nom_cat + language_german +
    books_dnb_prev_cat + wikiprizes_pre_cat +
    nom_prize_prev + pub_reputation_mean_cat + wv_mean_cat +
    topic_history + topic_culture +
    topic_identity + topic_politics + topic_relations + nonfiction +
    female * metoo + language_german * syria +
    female * jury_group +
    revs_n_cat + senti_qual_cat + debut
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

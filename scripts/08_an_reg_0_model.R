pacman::p_load("lmtest", "sandwich", "margins", "broom", "dotwhisker")

pacman::p_load("ggeffects", "parameters")
pacman::p_load("modelsummary", "flextable", "officer")


nominees_an <- readRDS("../data/nominees_rec.RDS") |>
  mutate(across(where(is.logical), as.numeric))


coef_labs <- c(
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
  "topic_history" = "History",
  "topic_politics" = "Politics",
  "topic_relations" = "Relations",
  "topic_identity" = "Identity",
  "topic_culture" = "Culture",
  "nonfiction" = "Dummy: Nonfiction",
  "female" = "Female",
  "female:metooAfter #metoo" = "Female x After #metoo",
  "jury_groupmore female" = "Jury female dominated (ref. even)",
  "jury_groupmore male" = "Jury male dominated (ref. even)",
  "female:jury_groupmore female" = "Female x Jury female dominated (ref. even)",
  "female:jury_groupmore male" = "Female x Jury male dominated (ref. even)",
  "language_german" = "German background",
  "syriaAfter 2015" = "After 2015",
  "language_german:syriaAfter 2015" = "German background x After 2015"
)


model_formulars <- c(
  "Quality" = winner ~ revs_n_cat + senti_qual_cat + debut,
  "Prominence" = winner ~ books_dnb_prev_cat + wikiprizes_pre_cat +
    nom_prize_prev + pub_reputation_mean_cat + wv_mean_cat + debut,
  "Topics" = winner ~ topic_history + topic_culture + topic_identity +
    topic_politics + topic_relations + debut,
  "Zeitgeist" = winner ~ female * metoo + language_german * syria + debut,
  "Jury" = winner ~ female * jury_group + debut,
  "Full Model" = winner ~ revs_n_cat + senti_qual_cat +
    books_dnb_prev_cat + wikiprizes_pre_cat + nom_prize_prev +
    pub_reputation_mean_cat + wv_mean_cat +
    topic_history + topic_culture + topic_identity + topic_politics +
    topic_relations + female * metoo + language_german * syria +
    female * jury_group + debut + nonfiction
)


models_log <- lapply(model_formulars, function(m) {
  glm(m, data = nominees_an, family = "binomial")
})


cl_vcov_mat <- vcovCL(models_log[[6]], cluster = ~prize)

margins_log <- margins(models_log[[6]], vcov = cl_vcov_mat)

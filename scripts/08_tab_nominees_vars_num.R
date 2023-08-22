# load data and category of by-varialbe to all observations

noms_all <- readRDS("../data/nominees_rec.RDS") |>
  mutate(winner_all = "All Nominations")


# prep varlist and table options
varlist <- winner_all ~ winner +
  senti_mean + senti_vari +
  revs_n +
  books_dnb_prev + wikiprizes_pre + nom_prize_prev +
  pub_reputation_mean + wv_mean +
  topic_history + topic_politics + topic_relations + topic_identity +
  topic_culture +
  female + age_nom + language_nongerman +
  debut


tableby(varlist,
  data = noms_all, total = FALSE, cat.simplify = TRUE,
  numeric.stats = c("Nmiss", "meansd", "range")
) |>
  set_labels(
    list(
      winner = "Winner",
      senti_mean = "Mean sentiment",
      senti_vari = "Variance sentiment",
      revs_n = "# reviews",
      books_dnb_prev = "# previous books",
      wikiprizes_pre = "# previous prizes",
      nom_prize_prev = "Previously nominated",
      pub_reputation_mean = "Publisher reputation",
      wv_mean = "Wikipedia views",
      topic_history = "History",
      topic_politics = "Politics",
      topic_relations = "Relations",
      topic_identity = "Identity",
      topic_culture = "Culture",
      female = "Gender: Female",
      age_nom = "Age",
      language_nongerman = "Non-native German speaker",
      debut = "Debut Prize "
    )
  ) |>
  write2word(
    file = "tab_nominees_vars_num.doc",
    keep.md = FALSE
  )

file.copy("tab_nominees_vars_num.doc", "../output/tables/", overwrite = TRUE)
file.remove("tab_nominees_vars_num.doc")

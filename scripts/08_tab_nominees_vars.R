p_load("arsenal")


# load data and category of by-varialbe to all observations
noms_all <- readRDS("../data/nominees_rec.RDS") |>
  mutate(winner_all = "All Nominations")


# prep varlist and table options
varlist <- winner_all ~ winner + revs_n_cat + senti_qual_cat +
  books_dnb_prev_cat + wikiprizes_pre_cat + nom_prize_prev +
  pub_reputation_mean_cat + wv_mean_cat +
  topic_history + topic_politics + topic_relations + topic_identity +
  topic_culture + nonfiction +
  female + age_nom_cat + language_german + jury_group + debut


tableby(varlist, data = noms_all, total = FALSE, cat.simplify = TRUE) |>
  set_labels(
    list(
      winner = "Winner",
      revs_n_cat = sprintf("# Reviews %s", levels(noms_all$revs_n_cat)[2]),
      senti_qual_cat = "Review Quality",
      senti_mean_cat = "Review sentiment",
      senti_vari_cat = "Variance sentiment",
      books_dnb_prev_cat = sprintf(
        "# previous books %s", levels(noms_all$books_dnb_prev_cat)[2]
      ),
      wikiprizes_pre_cat = sprintf(
        "# previous prizes %s", levels(noms_all$wikiprizes_pre_cat)[2]
      ),
      nom_prize_prev = "Previously nominated",
      pub_reputation_mean_cat = sprintf(
        "High Publisher Reputation %s",
        levels(noms_all$pub_reputation_mean_cat)[2]
      ),
      wv_mean_cat = sprintf(
        "Wikipedia views %s", levels(noms_all$wv_mean_cat)[2]
      ),
      topic_history = "Topic: History",
      topic_politics = "Topic: Politics",
      topic_relations = "Topic: Relations",
      topic_identity = "Topic: Identity",
      topic_culture = "Topic: Culture",
      nonfiction = "Nonfiction",
      female = "Female",
      age_nom_cat = sprintf("Higher age %s", levels(noms_all$age_nom_cat)[2]),
      language_german = "German Background",
      jury_group = "Jury Composition",
      debut = "Debut"
    )
  ) |>
  write2word(
    file = "tab_nominees_vars.doc",
    keep.md = FALSE
  )


file.copy("tab_nominees_vars.doc", "../output/tables/", overwrite = TRUE)
file.remove("tab_nominees_vars.doc")

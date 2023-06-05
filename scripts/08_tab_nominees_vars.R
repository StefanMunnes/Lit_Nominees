p_load("arsenal")


# load data and category of by-varialbe to all observations
noms_all <- readRDS("../data/nominees_rec.RDS") |>
  mutate(winner_all = "All Nominations")


# prep varlist and table options
varlist <- winner_all ~ winner +
  female + age_nom_cat + language_nongerman +
  books_dnb_prev_cat + wikiprizes_pre_cat + nom_prize_prev +
  pub_reputation_mean_cat + wv_mean_cat +
  topic_history + topic_politics + topic_relations + topic_identity +
  topic_culture +
  revs_n_cat + senti_qual_cat +
  debut + nonfiction + jury_group

# Change labels
# ? Change in recode_vars or keep originals?
levels(noms_all$jury_group) <- c("Even gender distribution", "More women in jury", "More men in jury")
levels(noms_all$revs_n_cat) <- c("(<= median, 4.0)", "(> median, 4.0)")
levels(noms_all$wikiprizes_pre_cat) <- c("(<= median, 4.0)", "(> median, 4.0)")
levels(noms_all$books_dnb_prev_cat) <- c("(<= median, 5.0)", "(> median, 5.0)")
levels(noms_all$pub_reputation_mean_cat) <- c("(<= median, 4.4)", "(> median, 4.4)")
levels(noms_all$wv_mean_cat) <- c("(<= median, 8.6)", "(> median, 8.6)")
levels(noms_all$age_nom_cat) <- c("(<= median, 43)", "(> median, 43)")


tableby(varlist, data = noms_all, total = FALSE, cat.simplify = TRUE) |>
  set_labels(
    list(
      winner = "Winner",
      female = "Gender: Female",
      age_nom_cat = sprintf("Higher age %s", levels(noms_all$age_nom_cat)[2]),
      language_nongerman = "Non-German native speaker",
      books_dnb_prev_cat = sprintf(
        "# books prior to nomination %s", levels(noms_all$books_dnb_prev_cat)[2]
      ),
      wikiprizes_pre_cat = sprintf(
        "# prizes prior to nomination %s", levels(noms_all$wikiprizes_pre_cat)[2]
      ),
      nom_prize_prev = "Previously unawarded nominated",
      pub_reputation_mean_cat = sprintf(
        "Publisher reputation %s",
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
      revs_n_cat = sprintf("# Reviews %s", levels(noms_all$revs_n_cat)[2]),
      senti_qual_cat = "Review Quality",
      senti_mean_cat = "Review sentiment",
      senti_vari_cat = "Variance sentiment",
      debut = "Debut Prize ",
      nonfiction = "Nonfiction book",
      jury_group = "Jury Composition"
    )
  ) |>
  write2word(
    file = "tab_nominees_vars.doc",
    keep.md = FALSE
  )

file.copy("tab_nominees_vars.doc", "../output/tables/", overwrite = TRUE)
file.remove("tab_nominees_vars.doc")

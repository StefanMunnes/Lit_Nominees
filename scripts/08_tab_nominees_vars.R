p_load("arsenal")


# load data and category of by-varialbe to all observations
noms_all <- readRDS("../data/nominees_rec.RDS") |>
  mutate(winner = "All Nominations")

# add cut-offs to label
# levels(noms_all$prevbooks) <- c("none", "medium (1-10)", "many (>10)")
# levels(noms_all$prevprize) <- c("none", "medium (1-7)", "many (>7)")


# prep varlist and table options
varlist <- winner ~ revs_n_cat + senti_qual_cat + wv_mean_cat +
  # senti_mean_cat + senti_vari_cat +
  books_dnb_prev_cat + wikiprizes_pre_cat + # prevbooks + prevprize +
  pub_reputation + nom_prize_prev +
  topic_history + topic_politics + topic_relations + topic_identity +
  topic_culture +
  female + homophily + language_german + debut + nonfiction


tableby(varlist, data = noms_all, total = FALSE, cat.simplify = TRUE) |>
  set_labels(list(
    revs_n_cat = sprintf("# Reviews %s", levels(noms_all$revs_n_cat)[2]),
    senti_qual_cat = "Review Quality",
    senti_mean_cat = "Review sentiment",
    senti_vari_cat = "Variance sentiment",
    wv_mean_cat = sprintf(
      "Wikipedia views %s", levels(noms_all$wv_mean_cat)[2]
    ),
    pub_reputation = "High Publisher Reputation",
    prevbooks = "# previous books",
    books_dnb_prev_cat = sprintf(
      "# previous books %s", levels(noms_all$books_dnb_prev_cat)[2]
    ),
    prevprize = "# previous prizes",
    wikiprizes_pre_cat = sprintf(
      "# previous prizes %s", levels(noms_all$wikiprizes_pre_cat)[2]
    ),
    nom_prize_prev = "Previously nominated",
    topic_history = "Topic: History",
    topic_politics = "Topic: Politics",
    topic_relations = "Topic: Relations",
    topic_identity = "Topic: Identity",
    topic_culture = "Topic: Culture",
    female = "Female",
    homophily = "Jury Homophily",
    language_german = "German Background",
    debut = "Debut",
    nonfiction = "Nonfiction"
  )) |>
  write2word(
    file = "tab_nominees_vars.doc",
    keep.md = FALSE
  )

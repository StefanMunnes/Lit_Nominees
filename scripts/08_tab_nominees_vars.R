p_load("arsenal")


# load data
nominees <- readRDS("../data/nominees_rec.RDS")

# prep by-variable and labels for categories
noms_all <- nominees |>
  mutate(
    winner = "All Nominations",
    senti_mean_cat = factor(senti_mean_cat,
      levels = c("none", levels(nominees$senti_mean_cat)[1:2])
    ),
    senti_vari_cat = factor(senti_vari_cat,
      levels = c("none", levels(nominees$senti_vari_cat)[1:2])
    )
  )
levels(noms_all$prevbooks) <- c("none", "medium (1-10)", "many (>10)")
levels(noms_all$prevprize) <- c("none", "medium (1-7)", "many (>7)")


# prep varlist and table options
varlist <- winner ~ revs_n_cat + senti_mean_cat + senti_vari_cat + wv_mean_cat +
  prevbooks + prevprize + pub_reputation + nom_prize_prev +
  topic_history + topic_politics + topic_relations + topic_identity +
  topic_culture +
  female + homophily + language_german + debut + nonfiction


tableby(varlist, data = noms_all, total = FALSE, cat.simplify = TRUE) |>
  set_labels(list(
    revs_n_cat = sprintf("# Reviews %s", levels(noms_all$revs_n_cat)[2]),
    senti_mean_cat = "Review sentiment",
    senti_vari_cat = "Variance sentiment",
    wv_mean_cat = sprintf(
      "Wikipedia views %s",
      levels(noms_all$wv_mean_cat)[2]
    ),
    pub_reputation = "High Publisher Reputation",
    prevbooks = "# previous books",
    prevprize = "# previous prizes",
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

p_load("arsenal")


# load data
nominees <- readRDS("../data/nominees_rec.RDS")

noms_all <- mutate(nominees,
  winner = "All Nominees"
)

# prep different data.frames for different tables
# nominees_all <- nominees |>
#   mutate(winner = FALSE) |>
#   bind_rows(nominees[nominees$winner, ]) |>
#   mutate(winner = ifelse(winner, "Winner", "All Nominees"))


# prep varlist and table options
varlist <- winner ~ revs_n_cat + senti_mean_cat + senti_vari_cat + wv_mean_cat +
  pub_reputation + prevbooks + prevprize + nom_prize_prev +
  topic_history + topic_politics + topic_relations + topic_identity +
  topic_culture +
  female + homophily + language_german + debut + nonfiction


tableby(varlist, data = noms_all, total = FALSE, cat.simplify = TRUE) |>
  set_labels(list(
    revs_n_cat = "# Reviews",
    senti_mean_cat = "Review sentiment",
    senti_vari_cat = "Variance sentiment",
    wv_mean_cat = "Wikipedia Views",
    pub_reputation = "Publisher Reputation",
    prevbooks = "# previous books",
    prevprize = "# previous prizes",
    nom_prize_prev = "# previous nominations",
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


c <- tableby(varlist, data = noms_all, control = controls, cat.simplify = TRUE)

summary(c, text = TRUE)

p_load("arsenal")


# load data
nominees <- readRDS("../data/nominees_rec.RDS")

nom_test <- mutate(nominees,
  winner = "All Nominees"
)

# prep different data.frames for different tables
nominees_all <- nominees |>
  mutate(winner = FALSE) |>
  bind_rows(nominees[nominees$winner, ]) |>
  mutate(winner = ifelse(winner, "Winner", "All Nominees"))

noms_cat <- nom_test |>
  mutate(across(
    c(revs_n, senti_mean, senti_vari, wv_mean, homophily),
    ~ case_when(
      is.na(.x) ~ "none",
      .x < mean(.x, na.rm = TRUE)
      ~ sprintf("< mean (%.1f)", mean(.x, na.rm = TRUE)),
      .x > mean(.x, na.rm = TRUE)
      ~ sprintf("> mean (%.1f)", mean(.x, na.rm = TRUE)),
    ) |>
      as.ordered()
  ))


# prep varlist and table options
varlist <- winner ~ revs_n + senti_mean + senti_vari + homophily + wv_mean +
  pub_reputation + prevbooks + prevprize + nom_prize_prev +
  topic_history + topic_politics + topic_relations + topic_identity +
  topic_culture +
  female + language_german + debut + nonfiction


tableby(varlist, data = noms_cat, total = FALSE, cat.simplify = TRUE) |>
  set_labels(list(
    revs_n = "# Reviews", senti_mean = "Review sentiment",
    senti_vari = "Variance sentiment", wv_mean = "Wikipedia Views",
    pub_reputation = "Publisher Reputation", prevbooks = "# previous books",
    prevprize = "# previous prizes", nom_prize_prev = "# previous nominations",
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


c <- tableby(varlist, data = nominees_cat, control = controls, cat.simplify = TRUE)

summary(c, text = TRUE)

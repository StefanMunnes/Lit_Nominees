
path_shiny <- "../data/shiny_handcoding/data/"


# ---- 1. load and merge shiny raw data ----

shiny_raw <- lapply(c("01", "02", "03"), function(dir) {
  files <- list.files(
    path = paste0(path_shiny, dir),
    pattern = ".csv", full.names = TRUE
  )

  lapply(files, function(file) {
    read.csv(file = file, encoding = "UTF-8", na.strings = "") |>
      cbind(file) # add file name to get rater
  })
}) |>
  bind_rows() |>
  rename(
    title = Buch,
    text = Review
  )



# ---- 2. clean raw shiny data ----

# load review data to get book url by review text
revs_book_url <- readRDS("../data/pt_reviews.RDS") |>
  select(text, url_book)

# dictionary for reduction of open codes to more generalized
codes <- read_excel(path = "../data/themen_all.xlsx", sheet = 1) |>
  select(original, final) |>
  na.omit() |>
  filter(final != "E") |>
  mutate(original = str_remove_all(original, "[ ()-]"))


# join review data.frame by text to get unique book url for possible duplicates by title
sent_topics <- left_join(shiny_raw, revs_book_url, by = "text") |>
  # get rater number from file name & prepare sentiment and topic
  mutate(
    rater = str_sub(file, -7, -7),
    Sentiment = na_if(Sentiment, 8),
    across(starts_with("Thema"), na_if, "Bitte hier eintragen")
  ) |>
  # create combined topic variable with
  unite(col = "topics_orig", starts_with("Thema"), na.rm = TRUE, sep = ";") |>
  mutate(topics_orig = na_if(topics_orig, "")) |>
  # SM: a <- group_by(sent_topics, title) |> filter(n_distinct(url_book) > 1)
  # Die Erziehung des Mannes: enthält eine Rezension zu zwei Büchern: Sentiment für Doppelrezension raus
  # Brüder und Schwestern: 2 Bücher von selbem Autor: gleiches Buch, gleicher Inhalt?: sentiment für zweites Buch raus
  # Außer sich: zwei verschiedene Bücher: trennen, erstes keine Topics

  # sentiment to NA if multiple books (after check: keep topics)
  mutate(
    Sentiment =
      case_when(
        str_detect(text, "(Doppel|Mehrfach|Sammel)(besprechung|rezension)") ~ NA_integer_,
        str_detect(text, "(bespricht|besprechen)\\s(zwei|drei)\\s") ~ NA_integer_,
        TRUE ~ Sentiment
      )
  ) |>
  # manually set topic from two separate books with same title (diff in URL) and remove wrong book
  mutate(topics_orig = ifelse(Nr. == 680, "DDR;Familie;Kommunismus", topics_orig)) |>
  filter(
    url_book != "/buch/birk-meinhardt/brueder-und-schwestern-die-jahre-1989-2001-roman-2017.html" &
      url_book != "/buch/thomas-glavinic/der-jonas-komplex.html"
  ) |>
  # calculate mean sentiment
  group_by(url_book) |>
  mutate(senti_mean = round(mean(Sentiment, na.rm = T), 2)) |>
  # keep just the one observation per rater with topic included
  # can't just filter over NA; sometimes topics missing, keep at least one row
  group_by(url_book, rater) |>
  mutate(
    revs_n_st = n(),
    revs_n_nomis_st = sum(!is.na(Sentiment))
  ) |>
  arrange(topics_orig) |>
  filter(row_number() == 1) |>
  # calculate number of raters
  group_by(url_book) |>
  mutate(rater_n_st = n()) |>
  ungroup() |>
  # categorize topics
  mutate(
    topics_tmp = str_remove_all(topics_orig, "[ ()-]"),
    topics_tmp = stri_replace_all(topics_tmp,
      regex = paste0("\\b", codes$original, "\\b"),
      replacement = codes$final,
      vectorize_all = FALSE
    ),
    topic_history = str_detect(topics_tmp, "\\bA\\b"),
    topic_politics = str_detect(topics_tmp, "\\bB\\b"),
    topic_relations = str_detect(topics_tmp, "\\bC\\b"),
    topic_identity = str_detect(topics_tmp, "\\bD\\b"),
    topic_culture = str_detect(topics_tmp, "\\bF\\b")
  ) |>
  # if multiple raters, keep three topics with highest agreement *larger zero*
  # JV: 4 books lose topics
  #     Apostoloff                      C;A;D     2
  #     Annette, ein Heldinnenepos      Epos;A;B  1
  #     Ambra                           C;A;A     1
  #     Am Ende schmeißen wir mit Gold  D;F       0
  group_by(url_book) |>
  # mutate(across(starts_with("topic_"), ~ mean(.x) >= .5)) |>
  mutate(
    across(
      starts_with("topic_"),
      ~ mean(.x) %in% subset(.x, !. %in% c(0, NA)) |>
        sort(decreasing = TRUE) |>
        unique() |>
        head(3)
    )
  ) |>
  mutate(topics_orig = paste(topics_orig, collapse = "|")) |>
  filter(row_number() == 1) |>
  ungroup() |>
  # mutate(topics_n = select(., starts_with("topic_")) |> rowSums()) |>
  select(
    title, url_book, revs_n_st, revs_n_nomis_st, rater_n_st,
    senti_mean, starts_with("topic_"), topics_orig
  )


saveRDS(sent_topics, "../data/sentiment_topics.RDS")



# test data.frame to see co-occurrence from uncategorized topics
#
# test <- sent_topics |>
#   select(title, senti_mean, rater_n, starts_with("Thema"), starts_with("topic")) |>
#   distinct() |>
#   mutate(thema = paste(Thema1, Thema2, Thema3, sep = ";")) |>
#   separate_rows(thema, sep = ";") |>
#   filter(str_length(thema) > 2) |>
#   select(thema, starts_with("topic")) |>
#   group_by_all() |>
#   summarize(n = n()) |>
#   arrange(thema, desc(n))

# # delete in final code
# sent_topics |>
#   filter(topics_n < 3, topics_tmp |> str_detect("[A:F]")) |>
#   View()

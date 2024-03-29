# ---- 1. load and merge shiny raw data ----

shiny_raw <- lapply(c("01", "02", "03"), function(dir) {
  files <- list.files(
    path = paste0("../data/shiny_handcoding/data/", dir),
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



# ---- 2. load further data to keep just reviews before prize ----

nominees_pt <- readRDS("../data/nominees_pt.RDS") |>
  dplyr::select(url_book, prize, ynom, match_id) |>
  na.omit() # remove books with no book url -> no pt -> no sentiment

# date of nomination, prize receipt and ceremony
dates <- readxl::read_xlsx("../data/preise_daten.xlsx") |>
  select(prize, ynom, winner_announced)

# load review data to get book url by review text and add prize and date
revs_book_url <- readRDS("../data/pt_reviews.RDS") |>
  select(text, url_book, date) |>
  inner_join(nominees_pt, by = "url_book") |>
  inner_join(dates, by = c("prize", "ynom"))


# dictionary for reduction of open codes to more generalized
codes <- read.csv("../data/topics_hc.csv", encoding = "UTF-8") |>
  select(original, final) |>
  mutate(original = str_remove_all(original, "[ ()-]"))



# ---- 3. clean raw shiny data ----

# join review data by text to get unique book url (possible duplicates by title)
# inner_join: remove books/sentiments with no prize (13/38) & 8x no hand coding
# many-to-many: multiple raters same review to same review multiple prizes
sent_topics <- shiny_raw |>
  inner_join(revs_book_url, by = "text", relationship = "many-to-many") |>
  # get rater number from file name & prepare sentiment and topic
  mutate(
    rater = str_sub(file, -7, -7),
    Sentiment = na_if(Sentiment, 8),
    across(starts_with("Thema"), na_if, "Bitte hier eintragen")
  ) |>
  # create combined topic variable with
  unite(col = "topics_orig", starts_with("Thema"), na.rm = TRUE, sep = ";") |>
  mutate(topics_orig = na_if(topics_orig, "")) |>
  # sentiment to NA if multiple books (after check: keep topics)
  mutate(
    Sentiment =
      case_when(
        str_detect(
          text,
          "(Doppel|Mehrfach|Sammel)(besprechung|rezension)"
        ) ~ NA_integer_,
        str_detect(
          text,
          "(bespricht|besprechen)\\s(zwei|drei)\\s"
        ) ~ NA_integer_,
        TRUE ~ Sentiment
      )
  ) |>
  # manually set topic from two separate books with same title (diff in URL)
  mutate(topics_orig = ifelse(
    title == "Brüder und Schwestern",
    "DDR;Familie;Kommunismus",
    topics_orig
  )) |>
  # mark if review was later than price announcement (435 ~ 15%)
  mutate(
    rev_later = date > winner_announced,
    Sentiment = ifelse(rev_later, NA, Sentiment) # Sent to NA if rev was later
  ) |>
  # calculate mean sentiment: group also by prize & year
  group_by(url_book, prize, ynom) |>
  mutate(
    senti_mean = round(mean(Sentiment, na.rm = TRUE), 2),
    senti_vari = round(var(Sentiment, na.rm = TRUE), 2)
  ) |>
  # keep just the one observation per rater with topic included
  # can't just filter over NA; sometimes topics missing, keep at least one row
  group_by(url_book, rater, prize, ynom) |>
  mutate(
    revs_n_st = n(),
    revs_n_nomis_st = sum(!is.na(Sentiment))
  ) |>
  arrange(topics_orig) |>
  filter(row_number() == 1) |>
  # calculate number of raters
  group_by(url_book, prize, ynom) |>
  mutate(rater_n_st = n()) |>
  ungroup() |>
  # categorize topics
  mutate(
    topics_tmp = str_remove_all(topics_orig, "[ ()-]"),
    topics_tmp = stri_replace_all(
      topics_tmp,
      regex = paste0("\\b", codes$original, "\\b"),
      replacement = codes$final,
      vectorize_all = FALSE
    ),
    topic_history = str_detect(topics_tmp, "\\bA\\b"),
    topic_politics = str_detect(topics_tmp, "\\bB\\b"),
    topic_relations = str_detect(topics_tmp, "\\bC\\b"),
    topic_identity = str_detect(topics_tmp, "\\bD\\b"),
    topic_culture = str_detect(topics_tmp, "\\bE\\b")
  ) |>
  # if multiple raters, keep three topics with highest agreement *larger zero*
  group_by(url_book, prize, ynom) |>
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
  select(
    title, , prize, ynom, url_book, revs_n_st, revs_n_nomis_st, rater_n_st,
    senti_mean, senti_vari, starts_with("topic_"), topics_orig
  )


sent_topics <- bind_rows(
  sent_topics,
  readRDS("../data/tmp_sentiment_aspekte_new.RDS")
) |>
  # add one last missing book by hand
  bind_rows(
    data.frame(
      title = "In seiner frühen Kindheit ein Garten",
      url_book = "/buch/christoph-hein/in-seiner-fruehen-kindheit-ein-garten.html",
      revs_n_st = 5, revs_n_nomis_st = 5, rater_n_st = 1,
      senti_mean = 1.8, senti_vari = 1.7,
      topic_history = TRUE, topic_politics = TRUE, topic_relations = TRUE,
      topic_identity = FALSE, topic_culture = FALSE,
      prize = "leipziger", ynom = 2005
    )
  ) |>
  # filter duplicates: keep just last (newest with more tester)
  filter(row_number() == n(), .by = c("url_book", "prize", "ynom"))


saveRDS(sent_topics, "../data/sentiment_topics.RDS")


# ! we have different values for sentiment for same books if one for later prize
# (keeps more reviews valid from date)

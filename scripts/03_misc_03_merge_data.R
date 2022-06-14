
# load prepared nominee data from perlentaucher (prizes + books + reviews)
nominees_pt <- readRDS("../data/nominees_pt.RDS")

# lod sentiment and topics
sent_topics <- readRDS("../data/sentiment_topics.RDS")

# load Publisher Information
publisher <- read.csv("../data/publisherstatus/publisherstatus_new.csv", encoding = "UTF-8")

# load Jury Information
jury <- read_xlsx("../data/prizes_xlsx/Jury_allprices.xlsx") %>%
  group_by(ynom, prize) %>%
  mutate(jury_fem = round(sum(Jury_F) / sum(Jury_F, Jury_M), 2)) %>%
  select(ynom, prize, jury_fem)

# load dnb books (for books before nomination)
dnb_books <- readRDS("../data/dnb_books_prize.RDS") |>
  select(match_id, title_dnb, year_dnb, keyword_dnb) |> 
  mutate(url_name = str_replace(match_id, "(.*?)_.+", "\\1")) |> 
  select(!match_id)


nominees <- nominees_pt |> 
  
  # 1. add sentiment and topics
  full_join(sent_topics, by = "url_book", suffix = c("_pt", "_st")) |>
  mutate(no_prize = is.na(prize),
         no_topic = is.na(senti_mean)) |>

  # 12x no prize with coded sent/topics
  # 43x no sent/topics (2x with pt-revs, 41 without pt-revs)

  filter(!no_prize & !no_topic) |> 

  
  # 2. add gender share of jury
  left_join(jury, by = c("ynom", "prize")) |>

  
  # 3. add publisher reputation
  mutate(publisher = case_when(publisher == "Hanser Berlin" ~ "Carl Hanser Verlag",
                               publisher == "Rowohlt Berlin Verlag" ~ "Rowohlt Verlag",
                               publisher == "Matthes und Seitz Berlin" ~ "Matthes und Seitz",
                               publisher == "Galiani Verlag" ~ "Galiani Verlag Berlin",
                               TRUE ~ publisher)) |>
  left_join(publisher, by = "publisher") |>
  rename(pub_reputation = reputation,
         pub_reputation_mean = mean_reputation) |>

  
  # 4. number of books before nomination (by list of DNB books)
  left_join(dnb_books, by = "url_name") |>
  group_by(url_name, prize, ynom) |>
  mutate(books_dnb_n = n(),
         books_dnb_prev = sum(year_dnb < ynom),
         no_dnb = is.na(title_dnb)) |>
  filter(row_number() == 1) |>

  # 5. times of nomination and previous nominations for each author and prize
  group_by(url_name, prize) |>
  arrange(ynom) |>
  mutate(nom_prize_n = n(),
         nom_prize_prev = row_number() > 1) |>
  ungroup() |>

  select(authors, title_pt, subti, prize, ynom, shortlist, winner, debut, 
         jury_fem, nom_prize_n, nom_prize_prev,
         ybirth, ydeath, age_nom, female, language, academic, institute,
         revs_n, revs_fem, revs_n_nomis_st, senti_mean, books_dnb_n, books_dnb_prev,
         ypub, publisher, pub_place, starts_with("pub_rep"), # pages, price, type, isbn,
         tags, tpcs, keyword_dnb, starts_with("topic_"), topics_orig,
         url_name, url_book, match_id)


saveRDS(nominees, file = "../data/nominees.RDS")


# SM: add dummy debüt + interaction (?)


# SM: scrape Wikipedia:
# https://de.wikipedia.org/wiki/Liste_deutscher_Literaturpreise
# https://de.wikipedia.org/wiki/Liste_der_%C3%B6sterreichischen_Literaturpreise
# https://de.wikipedia.org/wiki/Liste_Schweizer_Literaturpreise

# SM: topic modelling reviews


# SM: lastname classification for migration background
authors_surnames <- select(prizes_df, name, language) |>
  unique()

# https://forebears.io/surnames/geltinger
# https://gender-api.com/en/api-docs/v2/query-by-full-name


# ---- 1. load necessary data ----

# prepared nominee data from perlentaucher (prizes + books + reviews)
nominees_pt <- readRDS("../data/nominees_pt.RDS")

# sentiment and topics
sent_topics <- readRDS("../data/sentiment_topics.RDS") |>
  distinct(across(url_book:topics_orig))

# Publisher Information
publisher <- read.csv("../data/publisherstatus/publisherstatus_new.csv",
  encoding = "UTF-8"
)

# Jury Information
jury <- read_xlsx("../data/prizes_xlsx/Jury_allprices.xlsx") %>%
  group_by(ynom, prize) %>%
  mutate(jury_fem = round(sum(Jury_F) / sum(Jury_F, Jury_M), 2)) %>%
  select(ynom, prize, jury_fem)

# dnb books (for books before nomination)
dnb_books <- readRDS("../data/dnb_books_prize.RDS") |>
  select(match_id, title_dnb, year_dnb, keyword_dnb) |>
  mutate(url_name = str_replace(match_id, "(.*?)_.+", "\\1")) |>
  select(!match_id)

# wikipedia data
wikiviews_pre <- readRDS("../data/wikiviews_pre.RDS")

# all prizes/nominations from wikipedia (before nomination)
wikiprizes_authors <- readRDS("../data/wp_prizes_authors.RDS") |>
  # prepare missings in url with url_name
  mutate(
    wiki_url =
      str_remove_all(wiki_url, "/w/index.php\\?title=|&action=edit&redlink=1"),
    wikiprizes_year = as.numeric(year)
  ) |>
  select(wiki_url, wikiprizes_year)


nominees <- nominees_pt |>
  # 1. add sentiment and topics
  full_join(sent_topics, by = "url_book", suffix = c("_pt", "_st")) |>
  mutate(
    no_prize = is.na(prize),
    no_senttop = is.na(senti_mean)
  ) |>
  # 12x no prize with coded sent/topics
  # 43x no sent/topics (2x with pt-revs, 41 without pt-revs)

  filter(!no_prize) |>
  # 2. add gender share of jury
  left_join(jury, by = c("ynom", "prize")) |>
  # 3. add publisher reputation
  mutate(publisher = case_when(
    publisher == "Hanser Berlin" ~ "Carl Hanser Verlag",
    publisher == "Rowohlt Berlin Verlag" ~ "Rowohlt Verlag",
    publisher == "Matthes und Seitz Berlin" ~ "Matthes und Seitz",
    publisher == "Galiani Verlag" ~ "Galiani Verlag Berlin",
    publisher == "Blumenbar" ~ "Blumenbar Verlag",
    publisher == "Jung und Jung" ~ "Jung und Jung Verlag",
    publisher == "Picus" ~ "Picus Verlag",
    publisher == "Piper" ~ "Piper Verlag",
    publisher == "Rowohlt" ~ "Rowohlt Verlag",
    publisher == "Rowohlt Berlin" ~ "Rowohlt Verlag",
    publisher == "S. Fischer" ~ "S. Fischer Verlag",
    publisher == "Schöffling" ~ "Schöffling und Co. Verlag",
    publisher == "Suhrkamp" ~ "Suhrkamp Verlag",
    publisher == "Zsolnay" ~ "Zsolnay Verlag",
    publisher == "DVA" ~ "Deutsche Verlags-Anstalt (DVA)",
    TRUE ~ publisher
  )) |>
  left_join(publisher, by = "publisher") |>
  rename(
    pub_reputation = reputation,
    pub_reputation_mean = mean_reputation
  ) |>
  # 4. number of books before nomination (by list of DNB books)
  full_join(dnb_books, by = "url_name") |>
  group_by(url_name, prize, ynom) |>
  mutate(
    books_dnb_n = n(),
    books_dnb_prev = sum(year_dnb < ynom, na.rm = TRUE),
    no_dnb = is.na(title_dnb)
  ) |>
  filter(row_number() == 1) |>
  # 5. times of nomination and previous nominations for each author and prize
  group_by(url_name, prize) |> # MK: Why group by prize?
  arrange(ynom) |>
  mutate(
    nom_prize_n = n(),
    nom_prize_prev = row_number() > 1
  ) |>
  ungroup() |>
  # 6. Add Wikipedia Views
  full_join(wikiviews_pre, by = c("url_name", "prize", "ynom")) |>
  # 7. number of awards (on wikipedia) before nomination
  # prepare wiki_url to match with missing urls -> just name with _
  mutate(
    wiki_url = na_if(wiki_url, "NA"),
    wiki_url =
      ifelse(
        is.na(wiki_url),
        str_replace_all(authors, " ", "_"),
        wiki_url
      )
  ) |>
  full_join(wikiprizes_authors, by = "wiki_url") |>
  group_by(url_name, prize, ynom) |>
  mutate(wikiprizes_pre = sum(wikiprizes_year < ynom, na.rm = TRUE)) |>
  filter(row_number() == 1) |>
  ungroup() |>
  # keep just necessary variales
  dplyr::select(
    authors, title, subti, prize, ynom, shortlist, winner, debut,
    jury_fem, nom_prize_n, nom_prize_prev,
    ybirth, ydeath, age_nom, female, language, academic, institute,
    revs_n, revs_fem, revs_n_nomis_st, senti_mean, senti_vari,
    books_dnb_n, books_dnb_prev, books_pt_n, starts_with("wv_"), wikiprizes_pre,
    ypub, publisher, pub_place, starts_with("pub_rep"),
    starts_with("topic_"), topics_orig, tags, tpcs, keyword_dnb,
    shortlist_date, longlist_date, wv_interval,
    url_name, url_book, match_id, no_pt, no_senttop
  )


saveRDS(nominees, file = "../data/nominees.RDS")


# SM: add dummy debüt + interaction (?)

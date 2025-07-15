# ---- 1. load necessary data ----

# prepared nominee data from perlentaucher (prizes + books + reviews)
nominees_pt <- readRDS("../data/nominees_pt.RDS")

# sentiment and topics
sent_topics <- readRDS("../data/sentiment_topics.RDS") |>
  select(!title)

# missing topics
codes_last_miss <- readxl::read_xlsx("../data/topics_last_miss.xlsx") |>
  pivot_wider(names_from = "value")

codes_mi <- read.csv("../data/topics_hc_mi.csv", encoding = "UTF-8") |>
  select(match_id, value) |>
  pivot_wider(names_from = "value") |>
  bind_rows(codes_last_miss) |>
  # remove one wrong book with already 3 topics
  dplyr::filter(match_id != "christoph-hein_in-seiner-_37")

# Publisher Information (mean rating of reputation)
publisher <- read.csv(
  "../data/publisherstatus_coded.csv",
  encoding = "UTF-8"
) |>
  filter(metric == "reputation") |>
  rename(pub_reputation_mean = mean) |>
  select(publisher, pub_reputation_mean)

# Jury Information
jury <- readxl::read_xlsx("../data/prizes_xlsx/Jury_allprices.xlsx") %>%
  group_by(ynom, prize) %>%
  mutate(jury_fem = round(sum(Jury_F) / sum(Jury_F, Jury_M), 2)) %>%
  select(ynom, prize, jury_fem)

# dnb books (for books before nomination)
dnb_books <- readRDS("../data/dnb_books_prize.RDS") |>
  select(match_id, title_dnb, year_dnb, keyword_dnb) |>
  mutate(url_name = str_replace(match_id, "(.*?)_.+", "\\1")) |>
  select(!match_id)

# wikipedia data
wikiviews_pre <- readRDS("../data/wikiviews_pre.RDS") |>
  filter(!is.na(prize)) # remove one wrong observations not in nominees list

# all prizes/nominations from wikipedia (before nomination)
wikiprizes_authors <- readRDS("../data/wp_prizes_authors.RDS") |>
  # prepare missings in url with url_name
  mutate(
    wiki_url = str_remove_all(
      wiki_url,
      "/w/index.php\\?title=|&action=edit&redlink=1"
    ),
    wikiprizes_year = as.numeric(year)
  ) |>
  select(wiki_url, wikiprizes_year)


nominees <- nominees_pt |>
  # 1. add sentiment and topics
  full_join(
    sent_topics,
    by = c("url_book", "prize", "ynom"),
    suffix = c("_pt", "_st")
  ) |>
  mutate(
    no_prize = is.na(prize),
    no_senttop = is.na(senti_mean)
  ) |>
  # 12x no prize with coded sent/topics
  # 43x no sent/topics (2x with pt-revs, 41 without pt-revs)

  filter(!no_prize) |>
  # add topics from keywords/blurb for books (match_id) with not enough topics
  full_join(codes_mi, by = "match_id") |>
  mutate(
    topic_history = ifelse(!is.na(A), TRUE, topic_history),
    topic_politics = ifelse(!is.na(B), TRUE, topic_politics),
    topic_relations = ifelse(!is.na(C), TRUE, topic_relations),
    topic_identity = ifelse(!is.na(D), TRUE, topic_identity),
    topic_culture = ifelse(!is.na(E), TRUE, topic_culture),
    across(starts_with("topic_"), ~ ifelse(is.na(.x), FALSE, .x))
  ) |>
  # 2. add gender share of jury
  left_join(jury, by = c("ynom", "prize")) |>
  # 3. add publisher reputation
  mutate(
    publisher = case_when(
      publisher == "Hanser Berlin" ~ "Carl Hanser Verlag",
      publisher == "Matthes und Seitz Berlin" ~ "Matthes und Seitz",
      publisher == "Galiani Verlag" ~ "Galiani Verlag Berlin",
      publisher == "Blumenbar" ~ "Blumenbar Verlag",
      publisher == "Jung und Jung" ~ "Jung und Jung Verlag",
      publisher == "Picus" ~ "Picus Verlag",
      publisher == "Piper" ~ "Piper Verlag",
      publisher == "Rowohlt" ~ "Rowohlt Verlag",
      publisher == "Rowohlt Berlin" ~ "Rowohlt Verlag",
      publisher == "Rowohlt Berlin Verlag" ~ "Rowohlt Verlag",
      publisher == "S. Fischer" ~ "S. Fischer Verlag",
      publisher == "Schöffling" ~ "Schöffling und Co. Verlag",
      publisher == "Suhrkamp" ~ "Suhrkamp Verlag",
      publisher == "Zsolnay" ~ "Zsolnay Verlag",
      publisher == "DVA" ~ "Deutsche Verlags-Anstalt (DVA)",
      publisher == "Kremayr & Scheriau" ~ "Kremayr und Scheriau Verlag",
      match_id == "konstantin-kuespert_rechtes-de_46" ~ "Suhrkamp Verlag",
      .default = publisher
    )
  ) |>
  left_join(publisher, by = "publisher") |>
  # 4. number of books before nomination (by list of DNB books)
  # many-to-many: multiple books per author; auhtor multiple times in nominees
  full_join(dnb_books, by = "url_name", relationship = "many-to-many") |>
  # remove additional url_name observsations not from nominees list
  filter(!is.na(prize)) |>
  group_by(url_name, prize, ynom) |>
  mutate(
    books_dnb_n = n(),
    books_dnb_prev = sum(year_dnb < ynom, na.rm = TRUE),
    no_dnb = is.na(title_dnb)
  ) |>
  filter(row_number() == 1) |>
  # 5. times of nomination and previous nominations for each author and prize
  group_by(url_name, prize) |>
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
    wiki_url = ifelse(
      is.na(wiki_url),
      str_replace_all(authors, " ", "_"),
      wiki_url
    )
  ) |>
  # 8. Add list of Prizes (just year) for each author: count sum before ynom
  # many-to-many: multiple prizes per author; auhtor multiple times in nominees
  full_join(
    wikiprizes_authors,
    by = "wiki_url",
    relationship = "many-to-many"
  ) |>
  group_by(url_name, prize, ynom) |>
  mutate(wikiprizes_pre = sum(wikiprizes_year < ynom, na.rm = TRUE)) |>
  filter(row_number() == 1) |>
  ungroup() |>
  # remove additional observsation not from nominees list
  filter(!is.na(prize)) |>
  # keep just necessary variales
  dplyr::select(
    authors,
    title,
    subti,
    prize,
    ynom,
    shortlist,
    winner,
    debut,
    jury_fem,
    nom_prize_n,
    nom_prize_prev,
    ybirth,
    ydeath,
    age_nom,
    female,
    language,
    academic,
    institute,
    revs_n,
    revs_fem,
    revs_n_nomis_st,
    senti_mean,
    senti_vari,
    books_dnb_n,
    books_dnb_prev,
    books_pt_n,
    starts_with("wv_"),
    wikiprizes_pre,
    ypub,
    publisher,
    pub_place,
    pub_reputation_mean,
    starts_with("topic_"),
    topics_orig,
    tags,
    tpcs,
    keyword_dnb,
    shortlist_date,
    longlist_date,
    wv_interval,
    url_name,
    url_book,
    match_id,
    no_pt,
    no_senttop,
    poetry
  )

saveRDS(nominees, file = "../data/nominees.RDS")

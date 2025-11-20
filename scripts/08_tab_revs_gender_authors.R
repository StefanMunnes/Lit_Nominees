# ---- 0. load and prepare review data with sentiment and gender----

prizes_df <- readRDS("../data/pt_prizes.RDS")
books_df <- readRDS("../data/pt_books.RDS")
reviews_gndr_df <- readRDS("../data/reviews_gndr.RDS")
sent_topics <- readRDS("../data/sentiment_topics.RDS") |>
  select(!title)

df_revs_gndr <- full_join(books_df, reviews_gndr_df, by = "url_book") |>
  separate_rows(url_name, sep = ";") |>
  mutate(
    url_name = case_when(
      str_detect(url_name, "stefanie-de-velasco") ~ "stefanie-de-velasco",
      str_detect(url_name, "thomas-meyer") ~ "thomas-meyer",
      str_detect(url_name, "antje-ravi") ~ "antje-ravic-strubel",
      str_detect(url_name, "mariam-kuehsel") ~ "mariam-kuehsel-hussaini",
      TRUE ~ url_name
    ),
    title = str_squish(title),
    match_id = cr_match_id(url_name, title)
  ) |>
  full_join(prizes_df, by = "match_id", suffix = c(".pt", ".xlsx")) |>
  full_join(
    sent_topics,
    by = c("url_book", "prize", "ynom"),
    suffix = c("_pt", "_st")
  ) |>
  filter(!is.na(prize)) |>
  select(url_name.pt, female, rev_gndr, senti_mean) |>
  filter(!is.na(rev_gndr)) |>
  mutate(aut_gndr = ifelse(female, "F", "M"))


# --- 1. review sentiment by gender (and by gender of author) ----

table_summary <- df_revs_gndr %>%
  group_by(aut_gndr, rev_gndr) %>%
  summarise(
    n = n(),
    mean_rating = mean(senti_mean, na.rm = TRUE),
    sd_rating = sd(senti_mean, na.rm = TRUE),
    .groups = "drop"
  )

flextable(table_summary) |>
  set_header_labels(
    aut_gndr = "Author",
    rev_gndr = "Reviewer",
    n = "N",
    mean_rating = "Mean",
    sd_rating = "SD"
  ) |>
  add_header_row(
    values = c("Gender", "", "Sentiment"),
    colwidths = c(2, 1, 2),
    top = TRUE
  ) |>
  colformat_double(digits = 2) |>
  autofit() |>
  save_as_docx(path = "../output/tables/tab_sent_by_gender.docx")


# --- 2. ANOVA review sentiment by gender (and by gender of author) ----

mod <- aov(senti_mean ~ aut_gndr * rev_gndr, data = df_revs_gndr)

broom::tidy(mod) |>
  flextable() |>
  set_header_labels(
    term = "Term",
    df = "Df",
    sumsq = "Sum Sq.",
    meansq = "Mean Sq.",
    statistic = "F-value",
    p.value = "p-value"
  ) |>
  labelizor(
    j = 1,
    label = c(
      "aut_gndr" = "Author",
      "rev_gndr" = "Reviewer",
      "aut_gndr:rev_gndr" = "Author x Reviewer"
    )
  ) |>
  colformat_double(digits = 2) |>
  colformat_double(j = 1, digits = 0) |>
  colformat_double(j = 6, digits = 3) |>
  autofit() |>
  save_as_docx(path = "../output/tables/tab_anova_sent_by_gender.docx")

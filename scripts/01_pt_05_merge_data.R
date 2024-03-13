# ---- combine book, review, and prize information ----

prizes_df <- readRDS("../data/pt_prizes.RDS")
books_df <- readRDS("../data/pt_books.RDS")
reviews_gndr_df <- readRDS("../data/reviews_gndr.RDS")


nominees_pt <- full_join(books_df, reviews_gndr_df, by = "url_book") |>
  # summarize review information on book level:
  group_by(url_book) |>
  mutate(revs_fem = round(sum(rev_gndr == "F") / revs_n, 2)) |>
  filter(row_number() == 1) |>
  # calculate number of books in total and for prize on PT
  group_by(url_name) |>
  mutate(books_pt_n = n()) |> # ,books_pt_prev = sum(year_ypub < ynom) -> TODO: for each prize

  # create match id for join with prices (separate books with multiple authors)
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
  # check for books with multiple publications in later years (after check: keep first publication)
  group_by(match_id) |>
  mutate(match_id_multiple = n() > 1) |>
  filter(ypub == min(ypub)) |>
  ungroup() |>
  # add prize information (also multiple prizes for a single books)
  full_join(prizes_df, by = "match_id", suffix = c(".pt", ".xlsx")) |>
  # keep just nominees (drop all books by authors who have not been nominated)
  filter(!is.na(prize)) |>
  # fill and keep just one variant of variables
  mutate(
    no_pt = is.na(url_book),
    authors = ifelse(is.na(authors), name, authors),
    title = ifelse(is.na(title.pt), title.xlsx, title.pt),
    subti = ifelse(is.na(subti.pt), subti.xlsx, subti.pt),
    publisher = ifelse(is.na(publisher.pt), publisher.xlsx, publisher.pt),
    url_name = ifelse(is.na(url_name.pt), url_name.xlsx, url_name.pt),
    age_nom = ynom - ybirth, # age in year of nomination
    debut = prize %in% c("oesterreich_debuet", "aspekte")
  ) |>
  select(
    authors, title, subti, prize, ynom, shortlist, winner, debut,
    ypub, publisher, pub_place, authors_n,
    tags, tpcs, tags_n, tpcs_n, revs_n, revs_fem,
    female, ybirth, ydeath, age_nom, academic, institute, language,
    url_name, url_book, match_id, no_pt, books_pt_n
  )


saveRDS(nominees_pt, "../data/nominees_pt.RDS")

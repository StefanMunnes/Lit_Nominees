
# ---- combine book, review, and prize information ----

prizes_df <- readRDS("../data/pt_prizes.RDS")
books_df <- readRDS("../data/pt_books.RDS")
reviews_gndr_df <- readRDS("../data/reviews_gndr.RDS")


nominees_pt <- full_join(books_df, reviews_gndr_df, by = "url_book") |>

  # summarize review information on book level:
  group_by(url_book) |>
  mutate(revs_fem = round(sum(rev_gndr == "F") / revs_n, 2)) |>
  filter(row_number() == 1) |>
  ungroup() |>

  # create match id for join with prices (separate books with multiple authors)
  separate_rows(url_name, sep = ";") |>

  mutate(url_name = case_when(str_detect(url_name, "stefanie-de-velasco") ~ "stefanie-de-velasco",
                              str_detect(url_name, "thomas-meyer") ~ "thomas-meyer",
                              str_detect(url_name, "antje-ravi") ~ "antje-ravic-strubel",
                              TRUE ~ url_name),

         title = str_squish(title),

         match_id = cr_match_id(url_name, title)) |>

  # check for books with multiple publications in later years (after check: keep first publication)
  group_by(match_id) |>
  mutate(match_id_multiple = n() > 1) |>
  filter(ypub == min(ypub)) |>
  ungroup() |>

  # add prize information (also multiple prizes for a single books)
  full_join(prizes_df, by = "match_id", suffix = c(".pt", ".xlsx")) |>

  mutate(no_pt = is.na(url_book),
         title = ifelse(is.na(title.pt), title.xlsx, title.pt),
         subti = ifelse(is.na(subti.pt), subti.xlsx, subti.pt),
         publisher = ifelse(is.na(publisher.pt), publisher.xlsx, publisher.pt),
         age_nom = ynom - ybirth, # age in year of nomination
         url_name = url_name.pt,
         debut = prize %in% c("oesterreich_debuet", "aspekte")) |> 

  # keep just nominees (drop all books by authors who have not been nominated)
  filter(!is.na(prize)) #|> 
  
test <- filter(nominees, match_id.x != match_id.y) |> select(starts_with("match_id"))

# check why different match_ids!???!?!

  select(authors, title, subti, prize, ynom, shortlist, winner, debut,
         ypub, publisher, pub_place, authors_n,
         tags, tpcs, tags_n, tpcs_n, revs_n, revs_fem,
         female, ybirth, ydeath, age_nom, academic, institute, language,
         url_name, url_book, match_id, no_pt)


saveRDS(nominees_pt, "../data/nominees_pt.RDS")

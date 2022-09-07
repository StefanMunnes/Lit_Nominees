
# ---- 1. load and combine  data ----

# nominee data from perlentaucher
authors_prizes <- readRDS("../data/nominees_pt.RDS") |>
  select(url_name, prize, ynom)

# date of nomination, prize receipt and ceremony
dates <- read_xlsx("../data/preise_daten.xlsx") |>
  select(!contains("Source"))

# author names with wikipedia urls
dnb_authors_wiki <- readRDS("../data/dnb_books_prize.RDS") |>
  distinct(name, wikipedia) |>
  transmute(
    wiki_url = url_decode_utf(wikipedia),
    url_name = clean(name)
  )


# combine data -> year prize authors with date of long/short-list
authors_wiki_url <- left_join(authors_prizes, dates, by = c("prize", "ynom")) |>
  full_join(dnb_authors_wiki, by = "url_name") |>
  filter(wiki_url != "NA") # remove authors without wikipedia url



# ---- 2. get number of wikiviews for time before announcement ----

# 2.1 loop over list of wikipedia title pages
wikiviews_pre_ls <- lapply(seq_len(nrow(authors_wiki_url)), function(row) {
  url <- authors_wiki_url$wiki_url[row]
  page <- str_remove(url, "^.*/wiki/")

  # get end date (depents on prize) & start date (12 month before)
  if (authors_wiki_url$prize[row] %in% c("deutscher", "oesterreich")) {
    end <- authors_wiki_url$longlist_date[row]
  } else {
    end <- authors_wiki_url$shortlist_date[row]
  }
  start <- end %m-% months(12)

  message(page, " from ", start, " to ", end)

  Sys.sleep(1)

  # create base data.frame with prize infos for matching
  df <- data.frame(
    wiki_url = url,
    prize = authors_wiki_url$prize[row],
    ynom = authors_wiki_url$ynom[row]
  )

  # get wiki views for authors with valid wikipedia page
  tryCatch(
    {
      df_views <- wp_trend(page = page, from = start, to = end, lang = "de")
      df <- cbind(df, df_views[c("date", "views")])
    },
    warning = function(warn) {
    },
    error = function(err) {
    }
  )

  return(df)
})


# 2.2 combine dfs, summarize new variables & merge with nominee names
wikiviews_pre <- wikiviews_pre_ls |>
  bind_rows() |>
  group_by(wiki_url, prize, ynom) |>
  summarise(
    wv_days = sum(!is.na(date)),
    wv_interval = paste(first(date), " - ", last(date)),
    wv_sum = sum(views, na.rm = TRUE),
    wv_sum_log = log(wv_sum),
    wv_mean = mean(views, na.rm = TRUE),
    wv_mean_log = log(wv_mean)
  ) |>
  mutate(wv_interval = case_when(
    wv_days == 0 ~ NA_character_,
    TRUE ~ wv_interval
  )) |>
  ungroup() |>
  full_join(authors_wiki_url, by = c("wiki_url", "prize", "ynom")) |>
  select(
    url_name, prize, ynom, longlist_date, shortlist_date, winner_announced,
    ceremony_date, wv_interval, wv_days,
    wv_sum, wv_sum_log, wv_mean, wv_mean_log,
    wiki_url
  )


saveRDS(wikiviews_pre, file = "../data/wikiviews_pre.RDS")

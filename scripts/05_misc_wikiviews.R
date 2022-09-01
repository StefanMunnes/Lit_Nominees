
# ---- 1. load and combine  data ----

# nominee data from perlentaucher
authors_prizes <- readRDS("../data/nominees_pt.RDS") |>
  mutate(url_name = clean(authors)) |>
  select(url_name, prize, ynom)

# date of nomination, prize receipt and ceremony
dates <- read_xlsx("../data/preise_daten.xlsx") |>
  select(!contains("Source"))

# author names with wikipedia urls
dnb_authors_wiki <- readRDS("../data/dnb_books_prize.RDS") |>
  distinct(name, wikipedia) |>
  mutate(url_name = clean(name) |> str_replace("-ravic-", "-ravik-"))


# combine data -> year prize authors with date of long/short-list
authors_wiki_url <- left_join(authors_prizes, dates, by = c("prize", "ynom")) |>
  full_join(dnb_authors_wiki, by = "url_name")



# ---- 2. get number of wikiviews for time before announcement ----

# 2.1 loop over list of wikipedia title pages
wikiviews_pre_ls <- lapply(seq_len(nrow(authors_wiki_url)), function(row) {
  url <- authors_wiki_url$wikipedia[row]
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
    wikipedia = url,
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
  group_by(wikipedia, prize, ynom) |>
  summarize(
    days = ifelse(sum(!is.na(date)) == 0, NA, n()),
    interval = ifelse(!is.na(days), paste(first(date), " - ", last(date)), NA),
    wv_sum = sum(views, na.rm = TRUE),
    wv_sum_log = log(views),
    wv_mean = mean(views, na.rm = TRUE),
    wv_mean_log = log(wv_mean)
  ) |>
  filter(row_number() == 1) |>
  full_join(authors_wiki_url, by = c("wikipedia", "prize", "ynom")) |>
  select(
    url_name, prize, ynom, longlist_date, shortlist_date, winner_announced,
    ceremony_date, interval, days,
    wv_sum, wv_sum_log, wv_mean, wv_mean_log,
    wikipedia
  )


saveRDS(wikiviews_pre, file = "../data/wikiviews_pre.RDS")

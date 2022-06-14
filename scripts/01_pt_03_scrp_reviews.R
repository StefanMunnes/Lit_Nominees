# scrape review text (etc. ) from scraped list of books for each author

books_df <- readRDS("../data/pt_books.RDS")


# create function to simplify code: scrapted html to text via specific node
html2text <- function(html, node) {
  html %>% html_nodes(node) %>% html_text()
}


# ---- 1. scrape header and text of reviews from Perlentaucher by url_book ----
reviews_htmls <- lapply(unique(books_df$url_book), function(url) {

  print(url)

  Sys.sleep(1)

  tryCatch({ paste0("https://www.perlentaucher.de", url) %>% read_html()},
            error = function(e) NULL)
})

# name list of reviews with url_book
names(reviews_htmls) <- paste0(unique(books_df$url_book))


# get missing books b/c timeout (?) and retry scraping for them
missing_books <- names(keep(reviews_htmls, is.null))

if (length(missing_books) > 0) {

  reviews_htmls_2 <- lapply(missing_books, function(url) {

    print(url)

    Sys.sleep(1)

    paste0("https://www.perlentaucher.de", url) %>% read_html()
  })

  # name list of reviews with url_book
  names(reviews_htmls_2) <- missing_books


  # combine both list and replace empty with new scraped values
  reviews_htmls <- modifyList(reviews_htmls, reviews_htmls_2)
}

# added 28.11.2021 - additional elements in the list without name -> remove
#reviews_pt_htmls_noname <- reviews_pt_htmls[names(reviews_pt_htmls) == "<unknown>"]
#reviews_pt_htmls <- reviews_pt_htmls[!is.na(names(reviews_pt_htmls))]



# ---- 2. extract Infos about book and reviews from scraped html-list ----
reviews_ls <- lapply(reviews_htmls, function(html) {

  title <- html2text(html, ".booktitle")
  subti <- html2text(html, ".bookdata .smaller")
  news  <- html2text(html, "h3.newspaper")
  text  <- html2text(html, "#col_middle .paragraph")
  tags  <- html2text(html, ".kw .kw")   %>% stri_paste(collapse = "; ")
  tpcs  <- html2text(html, "h4+ ul li") %>% stri_paste(collapse = "; ")

  if (length(news) == 0) {
    news <- NA
    text <- NA
  }

  data.frame(title = title, subti = subti,
             news = news, text = text,
             tags = tags, tpcs = tpcs)
})


# ---- 3. create dataframe of list of scraped dataframes with review ----
reviews_df <- bind_rows(reviews_ls, .id = "url_book") %>%

  separate(news, c("news", "date"), sep = ", ") %>%

  group_by(url_book) %>%
  mutate(title = trimws(title),
         news = trimws(str_replace_all(news, "Rezensionsnotiz zu", "")),
         date = as.Date(date, "%d.%m.%Y"),
         text = trimws(text),
         rev_len = str_length(text),

         # get rid off pt reviews, but keep at least one obser for tags/topics
         news   = ifelse(str_detect(news, "Perlentaucher"), NA, news),
         text   = ifelse(str_detect(news, "Perlentaucher"), NA, text),
         napt   = sum(is.na(news)),
         revs_n = n() - napt,

         tags   = ifelse(str_length(tags) > 0, tags, NA),
         tpcs   = ifelse(str_length(tpcs) > 0, tpcs, NA),
         tags_n = ifelse(is.na(tags), 0, str_count(tags, ";") + 1),
         tpcs_n = ifelse(is.na(tpcs), 0, str_count(tpcs, ";") + 1)) %>%

  filter(!is.na(news) | (is.na(news) & revs_n == 0)) %>%
  ungroup() %>%

  # remove duplicates (same book-url, news, date and text)
  mutate(rev_intro = str_sub(text, 1, 15)) %>%
  distinct(url_book, news, date, rev_intro, .keep_all = T) %>%
  select(!c(napt, rev_intro)) %>%

reviews_df <- reviews_df |> 
  # correct book (sub-)titles manually to fit with DNB and published book titles
  mutate(
    title = case_when(
      str_detect(title, "Die Erfindung der Rote") ~ "Die Erfindung der Roten Armee Fraktion durch einen manisch depressiven Teenager im Sommer 1969",
      str_detect(title, "Der Träumer") ~ "Die Träumer",
      str_detect(title, "Das Fest der Steine") ~ "Das Fest der Steine",
      TRUE ~ title)) |>

  # add review ID
  mutate(rev_id = row_number())


saveRDS(reviews_df, file = "../data/pt_reviews.RDS")

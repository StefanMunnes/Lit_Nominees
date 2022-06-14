# scrape book information from perlentaucher from list of nomineed books

prizes_df <- readRDS("../data/prizes_xlsx.RDS")

authors_url <- unique(prizes_df$url_name)

# ---- 0. add special URLs manualy (2 pages, or multiple Names) ----
authors_url <- c(authors_url, "stefanie-de-velasco-8iv")

authors_url[authors_url == "thomas-meyer"] <- "thomas-meyer-6sz"
authors_url[authors_url == "antje-ravic-strubel"] <- "antje-ravik-strubel"

authors_url <- sort(authors_url)


# ---- 1. scrape author and book informations from perlentaucher ----
books_htmls <- lapply(authors_url, function(name) {

  print(name)

  Sys.sleep(1)

  tryCatch( {
    paste0("https://www.perlentaucher.de/autor/", name, ".html") %>% read_html()
  }, error = function(e) NULL)
})


# name list of author pages from perlentaucher
names(books_htmls) <- authors_url


books_ls <- lapply(books_htmls, function(html) {
  data.frame(header   = html_nodes(html, "#col_middle h3") %>% html_text(),
             subline  = html_nodes(html, ".book .subline") %>% html_text(),
             url_book = html_nodes(html, "div > h3 > a")   %>% html_attr("href")
  )
})


# ---- 2. get names of authors with no page at perlentaucher ----
missing <- keep(books_ls, plyr::empty)
(missing_names <- names(missing))


# ---- 3. create and recode dataframe from list of scraped dataframes ----
books_df2 <- bind_rows(books_ls, .id = "url_name") %>%

  separate(header, c("authors", "work"),
           sep = ":", extra = "merge", fill = "right") %>%

  # deal with extra commas to avoid erros with later separate by comma
  mutate(subline = sub("Basel, Weil", "Basel/Weil", subline),
         subline = sub("Klöpfer, Narr", "Klöpfer/Narr", subline)) %>%

  separate(subline, c("publisher", "multi", "type", "pages", "price"),
           sep = ",", extra = "merge", fill = "right") %>%

  mutate(authors_n = str_count(authors, "/") + 1,
         pub_place = trimws(str_extract(multi, "(^[[:alpha:] -./]+)")),
         ypub  = str_extract(multi, "[0-9]{4,4}") %>% as.numeric,
         isbn  = str_extract(multi, "[0-9 ]{10,14}"),
         price = ifelse(str_detect(pages, "EUR"), pages, price) %>%
         	          str_remove(" EUR") %>% as.numeric,
         pages = ifelse(str_detect(type, "Seiten"), type, pages),
         pages = ifelse(str_detect(pages, "Seiten"),
                        str_remove(pages, "\\sSeiten") %>% as.numeric, NA),
         type  = ifelse(str_detect(type, "Seiten"),
                        NA, trimws(tolower(type)))) %>%

  filter(!str_detect(type, "^cd|mp3|hörbuch")) %>%

  select(url_name, work, authors, authors_n, publisher, pub_place,
         ypub, pages, price, type, isbn, url_book) |>

  # compress books with multiple authors to unique with names in one cell
  group_by(url_book) |>
  mutate(url_name = paste(url_name, collapse = ";")) |>
  distinct() |>
  ungroup()


saveRDS(books_df, file = "../data/pt_books.RDS")

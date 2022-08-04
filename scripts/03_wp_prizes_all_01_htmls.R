
# ---- scrape all wikipedia htmls from german-speaking book prizes ----

# 1 get urls from 3 main pages (germany, austira, switzerland)
url_wiki <- "https://de.wikipedia.org/"

urls_nodes <- list(
  url = c(
    "Liste_deutscher_Literaturpreise",
    "Liste_der_%C3%B6sterreichischen_Literaturpreise",
    "Liste_von_Literaturpreisen#Schweiz"
  ),
  node = c("h2+ ul li > a:nth-child(1)", "p+ ul a", "ul:nth-child(56) a")
)

#
urls_prizes_all <- sapply(1:3, function(x) {
  read_html(paste0(url_wiki, "wiki/", urls_nodes$url[x])) |>
    html_nodes(urls_nodes$node[x]) |>
    html_attr("href")
}) |>
  unlist()

# remove wrong urls from list (mostly just to city, no prize information)
urls_prizes_all <- urls_prizes_all[!(urls_prizes_all %in% c(
  "/wiki/Toblach",
  "/wiki/Grein",
  "/wiki/FM4",
  "/wiki/Alberndorf_in_der_Riedmark",
  "/wiki/Karl_Otten"
))]

# ? aussortieren
# Verlagspreis: https://de.wikipedia.org/wiki/Kurt_Wolff_Stiftung

# get vectors of (un)valid prize urls
prizes_no_page <- urls_prizes_all[grepl("action=edit", urls_prizes_all, TRUE)]

urls_prizes <- setdiff(urls_prizes_all, prizes_no_page)


# 2. scrape raw htmls from vector with valid urls and store for extraction
htmls_ls <- lapply(urls_prizes, function(url) {
  message(url)

  Sys.sleep(1)

  GET(paste0(url_wiki, url)) |> content(as = "text")
})

names(htmls_ls) <- urls_prizes


saveRDS(htmls_ls, file = "../data/wp_prizes_htmls.RDS")

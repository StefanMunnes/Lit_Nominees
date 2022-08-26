
url_wiki <- "https://de.wikipedia.org"

# ---- scrape all wikipedia htmls from german-speaking book prizes ----


# list of start urls with subcategories
urls_prizes_subcat1 <- c(
  "Kategorie:Literaturpreis_(Deutschland)",
  "Kategorie:Literaturpreis_(Bayern)",
  "Kategorie:Literaturpreis_(Freie_Hansestadt_Bremen)"
)

# scrape urls in subcategories of main-pages
urls_prizes_subcat2 <- scrp_urls(urls_prizes_subcat1, "#mw-subcategories a") |>
  str_remove("/wiki/") |>
  unname()


# list of urls containing direct links to prizes
urls_prizes_main <- c(
  "Kategorie:Literaturpreis_(Deutschland)",
  "Kategorie:Literaturpreis_(Österreich)",
  "Kategorie:Literaturpreis_(Schweiz)",
  urls_prizes_subcat2,
  "Liste_deutscher_Literaturpreise",
  "Liste_der_österreichischen_Literaturpreise",
  "Liste_von_Literaturpreisen#Schweiz", # just switzerland
  "Liste_von_Literaturpreisen", # just international
  "Kategorie:Lyrikpreis",
  "Kategorie:Kinder-_und_Jugendliteraturpreis",
  "Liste_von_Kinder-_und_Jugendliteraturpreisen",
  "Liste_der_Sachbuchpreise_im_deutschsprachigen_Raum"
)

urls_prizes_main_sub <- scrp_urls(urls_prizes_main, "auto")
names(urls_prizes_main_sub) <- NULL


# Kinder-_und_Jugendliteratur#Preise -> subheading with german speaking prizes

html_kjl <- read_html(paste0(url_wiki, "/wiki/Kinder-_und_Jugendliteratur"))

urls_kinder_jugendliteratur <-
  sapply(c(199, 201, 203, 211, 213, 215, 217), function(nth) {
    html_nodes(html_kjl, paste0("ul:nth-child(", nth, ") a:nth-child(1)")) |>
      html_attr("href")
  }) |>
  unlist()


# Tabelle: "Liste_von_Lyrikpreisen"
table <- read_html("https://de.wikipedia.org/wiki/Liste_von_Lyrikpreisen") |>
  html_elements("table")

ns <- xml_ns(table)
rows <- xml_find_all(table, ".//tr", ns = ns)
cells <- lapply(rows, xml_find_all, ".//td|.//th", ns = ns)

urls_lyrikpreise <- data.frame(
  prize_url = sapply(2:length(cells), function(row) {
    get_name_link(cells[[row]][1], "href")
  }),
  language = sapply(2:length(cells), function(row) {
    html_clean_text(cells[[row]][2])
  })
) |>
  filter(!is.na(prize_url) & str_detect(language, "Deutsch")) |>
  pull(prize_url)


# combine all sources and manuall urls and clear list of prize urls
urls_prizes_all <- c(
  urls_prizes_main_sub,
  urls_kinder_jugendliteratur,
  urls_lyrikpreise
) |>
  # transform url to readable format and lowercase
  url_decode_utf() |>
  c( # add wiki urls for nominations for bigger prices
    "/wiki/Preis_der_Leipziger_Buchmesse/Belletristik",
    "/wiki/Liste_der_Preisträger_und_Nominierten_des_Schweizer_Buchpreises",
    paste0("/wiki/Ingeborg-Bachmann-Preis_", seq(1977, 2022)),

    # add "Stadtschreiber" prices manually
    "/wiki/Stadtschreiber_von_Bergen",
    "/wiki/Mainzer_Stadtschreiber",
    "/wiki/Dresdner_Stadtschreiber",
    "/wiki/Inselschreiber",
    "/wiki/Stadtschreiber_von_Graz",
    "/wiki/H._C._Artmann-Stipendium",
    "/wiki/Kitzbüheler_Stadtschreiber",

    # add mising price manually
    "/wiki/Friedrich-Glauser-Preis"
  ) |>
  # remove dubplicates
  unique() |>
  # remove wrong prizes by pattern
  str_subset("Liste_von_|Liste_deutscher", TRUE) |> # main-list-urls
  # remove international prizes
  str_subset("[pP]ri(x|ze)|Award|Medal|Child|Dagg|Adult|Poet_|_Griff", TRUE) |>
  str_subset("Thijssenprijs|Schwed|Tomas|Tchicaya|_de_|Szymborska", TRUE) |>
  str_subset("japanisch|Tsukahara|Saisei|Kiriyama|Takami|Ogawa", TRUE) |>
  str_subset("((Foto|[hH]ör|Bilder)(spiel|buch))|Cartoon|Illustra", TRUE) |>
  str_subset("Verl[ae]g[es]", TRUE) |> # publisher prizes
  str_subset("Übersetz|FIT", TRUE) %>% # translations

  # remove more prizes manually
  subset(. %notin% c(
    "/wiki/Theaterpreis", # list of theater prizes
    "/wiki/Kurt_Wolff_Stiftung", # Verlagspreis
    "/wiki/Dulzinea_(Zeitschrift)",
    "/wiki/Diagram-Preis", # english book title
    "/wiki/Juhan-Liiv-Lyrikpreis",
    "/wiki/Pfälzischer_Mundartdichterwettstreit", # not important; error tables
    "/wiki/Hansjörg-Martin-Preis", # part of other price; produces error
    "/wiki/Samfundet_De_Nios_Astrid_Lindgren-Preis", # swedish
    "/wiki/Premio_Andersen", # italian
    "/wiki/P.C.-Hooft-Preis", # netherland
    "/wiki/Woutertje_Pieterse_Prijs", # netherland
    "/wiki/Rómulo-Gallegos-Preis", # spanish
    "/wiki/Prémio_Camões", # portuges
    "/wiki/Premio_Internazionale_di_Poesia_Nosside", # no german authors
    "/wiki/Internationale_Bodenseekonferenz", # no prize
    "http://www.literaturport.de/index.php?id=33" # wrong url
  )) |>
  # remove unvalid links (page not available, just create from edit)
  str_subset("action=edit", TRUE)



# 2. Prizes: scrape raw htmls from vector with valid urls and store
htmls_prizes_ls <- scrp_htmls(urls_prizes_all)

saveRDS(htmls_prizes_ls, file = "../data/wp_prizes_htmls.RDS")



# 3. Authors: scrape raw htmls from vector with valid urls and store
urls_authors <- readRDS("../data/dnb_books_prize.RDS") |>
  distinct(name, wikipedia) |>
  pull(wikipedia) |>
  na.omit()


htmls_authors_ls <- scrp_htmls(urls_authors)

saveRDS(htmls_authors_ls, file = "../data/wp_authors_htmls.RDS")


prizes_raw <- readRDS("../data/wp_prizes_raw.RDS")


prizes_all <- prizes_raw |>
  mutate(
    name = str_remove_all(name, "\\(.*?\\)") |> str_squish(),
    multi_year = str_detect(year, ";"), # check if keep just latest year
    year = case_when(
      title == "Walter-Kempowski-Literaturpreis" ~
        str_extract(h3, "[0-9]{4,4}"),
      title == "Andreas-Gryphius-Preis" & multi_year == TRUE ~ "1970",
      title == "Tukan-Preis" ~ str_extract(text, "^[0-9]{4,4}"),
      str_detect(url_prize, "Bachmann-Preis_") ~
        str_extract(url_prize, "[0-9]{4,4}$"),
      str_detect(text, "^[0-9]{4,4}.{1,3}(?![0-9])") ~
        str_extract(year, "^[0-9]{4,4}"),
      TRUE ~ year
    ),
    year = sapply(year, multi_years),
    keep = case_when(
      note == "empty" ~ FALSE,
      title == "Mannheimer Literaturpreis" ~ FALSE, # check later
      str_detect(h2, "Mehrfachgewinner") ~ FALSE,
      str_detect(h2, "Kritikerjury") ~ FALSE,
      str_detect(h2, "Jugendjury") ~ FALSE,
      str_detect(h2, "Jahrestagungen") ~ FALSE,
      str_detect(h2, "Schriftenreihe der ILG") ~ FALSE,
      str_detect(h2, "Gastautoren") ~ FALSE,
      str_detect(h2, "Veranstaltungsreihen") ~ FALSE,
      title == "Ingeborg-Bachmann-Preis" ~ FALSE, # part of sub-pages with year
      # title == "Ingeborg-Bachmann-Preis" & str_detect(h2, "Preise") ~ FALSE,
      title == "Theodor-Storm-Gesellschaft" ~ FALSE,
      str_detect(h2, "Liste der Juroren") ~ FALSE,
      str_detect(h2, "Themenstellungen") ~ FALSE,
      str_detect(h2, "Sonderpreisjury") ~ FALSE,
      str_detect(name, "Frankfurter Buchmesse") ~ FALSE,
      str_detect(h2, "Kategorien") ~ FALSE,
      str_detect(h2, "Quelle") ~ FALSE,
      str_detect(h2, "Anthologien") ~ FALSE,
      str_detect(h2, "Anthologien") ~ FALSE,
      str_detect(h2, "Ausstellungskataloge") ~ FALSE,
      str_detect(h2, "Filmografie") ~ FALSE,
      str_detect(h3, "Bearbeitungen (in Auswahl)") ~ FALSE,
      str_detect(h3, "Übersetzungen (in Auswahl)") ~ FALSE,
      tabhead == "Lateinamerikanische Preisträger" ~ FALSE,
      is.na(year) | year == "" ~ FALSE, # no year
      TRUE ~ TRUE
    ),
    wiki_url = url_decode_utf(link) # new name for decoded url to match
  ) |>
  # remove unvalid observations
  filter(keep) |>
  # split if multiple authors per row (= year)
  separate_rows(name, wiki_url, sep = ";") |>
  # get year from long list of names with years in parantheses
  mutate(
    name = str_squish(name),
    text = ifelse(
      url_prize == "/wiki/Schweizerische_Schillerstiftung",
      str_extract(text, paste(name, ".*?\\(.*?\\)")),
      text
    ),
    year = ifelse(
      url_prize == "/wiki/Schweizerische_Schillerstiftung",
      get_years(text, "[0-9]{4,4}"),
      year
    )
  ) |>
  # remove observations from table with country in front of name
  filter(!str_detect(name, "^(Deutschland|Österreich|Schweiz|Georgien)$")) |>
  # split if multiple years of prize per author (= row)
  separate_rows(year, sep = ";") |>
  # manually correct wiki urls
  mutate(
    wiki_url = case_when(
      wiki_url == "/wiki/Volker_H._Altwasser" ~ "/wiki/Volker_Altwasser",
      TRUE ~ wiki_url
    ) |> str_squish()
  ) |>
  # filter duplicates per price, chapter, name & year (no Sub-Chapters h3, h4)
  distinct(url_prize, year, name, wiki_url, .keep_all = TRUE)


# load list of authors wiki urls to match with long list of all prizes
authors_wiki_url <- readRDS("../data/dnb_books_prize.RDS") |>
  distinct(name, wikipedia) |>
  transmute(wiki_url = wikipedia |>
    str_remove("https://de.wikipedia.org") |>
    url_decode_utf()) |>
  filter(wiki_url != "NA")

# additionaly, get all prizes from authors without valid wiki url
prizes_no_wiki <- prizes_all |>
  filter(name %in% c(
    "Carmen Buttjer", "Cihan Acar", "Gunther Neumann", "Sophie Albers",
    "Ramona Raabe ", "Dimitrij Wall", "Martin Kordic"
  ))

# filter list of all prizes by authors wiki url & add prizes authors w/out url
prizes_authors <- left_join(authors_wiki_url, prizes_all, by = "wiki_url") |>
  rbind(prizes_no_wiki) |>
  mutate(keep = case_when(
    # exclude some translations
    str_detect(text, "Übersetzung") & title == "Hamburger Literaturpreise"
    ~ FALSE,
    str_detect(text, "Übersetzung") & name == "Olaf Kühl" ~ FALSE,
    str_detect(text, "Übersetzung") & name == "Karl Rühmann" ~ FALSE,

    # exclude win as publisher
    str_detect(text, "Herausgeber") & name == "Susan Kreller" ~ FALSE,

    # exclude non-win
    title == "Heidelberger Stückemarkt" & name == "Verena Güntner" ~ FALSE,
    TRUE ~ TRUE
  )) |>
  filter(keep)

prizes_authors_miss <- filter(prizes_authors, is.na(title))


saveRDS(prizes_authors, file = "../data/wp_prizes_authors.RDS")


# ! Jury in Sublist
# /wiki/Liste_der_Preisträger_und_Nominierten_des_Deutschen_Buchpreises

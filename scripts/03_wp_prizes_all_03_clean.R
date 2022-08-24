
# check for multiple years
# 1. keep latest, or 2. see if all years in front, or 3. expected value in row

multi_years <- function(var = year) {
  year_list <- stringr::str_split(var, pattern = ";") |>
    unlist() |>
    as.numeric()

  if (length(year_list) < 2) {
    return(var)
  }
  if (length(year_list) > 2) {
    return(var) # return("multi year")
  }

  # if year_list == 2
  if (abs(diff(year_list, 1)) >= 2) {
    return(var) # return("big diff")
  }

  # if diff(year_list) == 2
  return(max(year_list))
}


url_decode_utf <- function(x) {
  y <- URLdecode(x)
  Encoding(y) <- "UTF-8"
  y
}


prizes_raw <- readRDS("../data/wp_prizes_raw.RDS")


prizes_all <- prizes_raw |>
  mutate(
    multi_year = str_detect(year, ";"), # check if keep just latest year
    year = case_when(
      title == "Walter-Kempowski-Literaturpreis" ~
        str_extract(h3, "[0-9]{4,4}"),
      str_detect(text, "^[0-9]{4,4}.{1,3}(?![0-9])") ~
        str_extract(year, "^[0-9]{4,4}"),
      title == "Andreas-Gryphius-Preis" & multi_year == TRUE ~
        "1970",
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
      str_detect(h2, "Veranstaltungsreihen") ~ FALSE,
      title == "Ingeborg-Bachmann-Preis" & str_detect(h2, "Preise") ~ FALSE,
      title == "Theodor-Storm-Gesellschaft" ~ FALSE,
      h2 == "Liste der Juroren des Leopold-Wandl-Preises (unvollständig)" ~ FALSE,
      str_detect(h2, "Themenstellungen") ~ FALSE,
      str_detect(h2, "Sonderpreisjury") ~ FALSE,
      str_detect(name, "Frankfurter Buchmesse") ~ FALSE,
      str_detect(h2, "Kategorien") ~ FALSE,
      str_detect(h2, "Quelle") ~ FALSE,
      str_detect(h2, "Anthologien") ~ FALSE,
      str_detect(h2, "Anthologien") ~ FALSE,
      str_detect(h2, "Ausstellungskataloge") ~ FALSE,
      str_detect(h2, "Filmografie") ~ FALSE,
      str_detect(h2, "Gemeinschaftswerke und Anthologien des Autorenkreises") ~ FALSE,
      str_detect(h3, "Bearbeitungen (in Auswahl)") ~ FALSE,
      str_detect(h3, "Übersetzungen (in Auswahl)") ~ FALSE,
      is.na(year) | year == "" ~ FALSE, # no year
      TRUE ~ TRUE
    ),
    link = url_decode_utf(link)
  ) |>
  filter(keep) |>
  separate_rows(name, link, sep = ";") |>
  separate_rows(year, sep = ";") |>
  # manually correct wiki urls
  mutate(link = case_when(
    link == "/wiki/Volker_H._Altwasser" ~ "/wiki/Volker_Altwasser",
    TRUE ~ link
  ), link = str_squish(link))

# check for names in text if no wiki link is provided
# prizes_miss <- filter(prizes_all, is.na(link))

# test <- sapply(dnb_authors_wiki$name, grep, prizes_miss$text) |>
#   lapply(function(x) {
#     paste(as.character(x), collapse = ";")
#   })



# load list of wiki
authors_wiki <- readRDS("../data/dnb_books_prize.RDS") |>
  distinct(name, wikipedia) |>
  transmute(link = wikipedia |>
    str_remove("https://de.wikipedia.org") |>
    url_decode_utf()) |>
  filter(link != "NA") 


authors_prizes_wiki <- left_join(authors_wiki, prizes_all, by = "link")

authors_prizes_miss <- filter(authors_prizes_wiki, is.na(title))


# /wiki/Alex_Capus missing -> aber Preise auf Personenseite
# /wiki/Isabel_Fargo_Cole -> Nominierungen und Auszeichnung Übersetzung



group_by(url_prize) |>
  mutate(no_year_in_chapter = sum(!is.na(year)) == 0) |>
  ungroup()



chapters <- data.frame(table(prizes_all$h2[!prizes_all$keep]))


test <- filter(prizes_all, !keep)
test2 <- filter(prizes_all, multi_year)


# ! after split -> remove duplicates over prize and year (still some errors)

prizes_clean <- filter(prizes_all, keep) |>
  separate_rows(name, link, by = " ; ") |>
  # remove authors without valid wiki pages
  filter(!grepl("edit&redlink=1", link, TRUE))




# check for multiple years
# 1. keep latest, or 2. see if all years in front, or 3. expected value in row


a <- "1991;1992"

b <- str_split(a, pattern = ";") |>
  unlist() |>
  as.numeric()

if (length(b) == 2) {
  if (diff(b, 1) < 2) max(b)
}

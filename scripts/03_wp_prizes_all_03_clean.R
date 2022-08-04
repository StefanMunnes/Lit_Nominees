
# check for multiple years
# 1. keep latest, or 2. see if all years in front, or 3. expected value in row

prizes_raw <- readRDS("../data/wiki_prizes_raw.RDS")

prizes_raw$year[prizes_raw$url_prize == "/wiki/Walter-Kempowski-Literaturpreis"] <- str_extract(prizes_raw$h3, "[0-9]{4,4}")

prizes_all <- prizes_raw |>
  mutate(
    keep = case_when(
      note == "empty" ~ FALSE,
      str_detect(h2, "Mehrfachgewinner") ~ FALSE,
      str_detect(h2, "Kritikerjury") ~ FALSE,
      str_detect(h2, "Die Jury") ~ FALSE,
      str_detect(h2, "Auswahlverfahren und Jury") ~ FALSE,
      str_detect(h2, "Publikationen") ~ FALSE,
      str_detect(h2, "Werke") ~ FALSE,
      str_detect(h2, "Veröffentlichungen") ~ FALSE,
      str_detect(h2, "Belege") ~ FALSE,
      str_detect(h2, "Schriftenreihe der ILG") ~ FALSE,
      str_detect(h2, "Veranstaltungsreihen") ~ FALSE,
      str_detect(h2, "Organisation") ~ FALSE,
      str_detect(h2, "Referenzen") ~ FALSE,
      str_detect(title, "Ingeborg-Bachmann-Preis") & str_detect(h2, "Preise") ~ FALSE,
      is.na(year) | year == "" ~ FALSE,
      TRUE ~ TRUE
    ),
    multi_year = str_detect(year, ";") # check if keep just latest year
  ) |>
  group_by(url_prize) |>
  mutate(no_year_in_chapter = sum(!is.na(year)) == 0) |>
  ungroup()


test <- filter(prizes_all, !keep)
# test2 <- filter(prizes_all, multi_year)


prizes_clean <- filter(prizes_all, keep) |>
  separate_rows(name, link, by = " ; ") |>
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

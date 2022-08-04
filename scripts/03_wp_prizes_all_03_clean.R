
# check for multiple years
# 1. keep latest, or 2. see if all years in front, or 3. expected value in row

prizes_raw <- readRDS("../data/wp_prizes_raw.RDS")


prizes_all <- prizes_raw |>
  mutate(
    year = case_when(
      title == "Walter-Kempowski-Literaturpreis" ~ str_extract(3, "[0-9]{4,4}"),
      TRUE ~ year
    ),
    keep = case_when(
      note == "empty" ~ FALSE,
      str_detect(h2, "Mehrfachgewinner") ~ FALSE,
      str_detect(h2, "Kritikerjury") ~ FALSE,
      str_detect(h2, "Schriftenreihe der ILG") ~ FALSE,
      str_detect(h2, "Veranstaltungsreihen") ~ FALSE,
      title == "Ingeborg-Bachmann-Preis" & str_detect(h2, "Preise") ~ FALSE,
      is.na(year) | year == "" ~ FALSE,
      TRUE ~ TRUE
    ),
    multi_year = str_detect(year, ";") # check if keep just latest year
  ) |>
  group_by(url_prize) |>
  mutate(no_year_in_chapter = sum(!is.na(year)) == 0) |>
  ungroup()


chapters <- data.frame(table(prizes_all$h2[!prizes_all$keep]))


test <- filter(prizes_all, !keep)
# test2 <- filter(prizes_all, multi_year)


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

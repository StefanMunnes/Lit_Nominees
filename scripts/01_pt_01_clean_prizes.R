# load raw excel data about nominee information by book prices

# ---- 1. load raw data from 6 same structered excel files at once ----
prizes <- c(
  "aspekte", "leipziger", "oesterreich_debuet", "oesterreich",
  "schweiz", "text_sprache"
)

prizes_raw_ls <- lapply(paste0("../data/prizes_xlsx/", prizes, "_buchpreis.xlsx"), read_xlsx)
names(prizes_raw_ls) <- prizes

prizes_raw <- bind_rows(prizes_raw_ls, .id = "Preis") %>%
  select(!contains("Quelle"))



# ---- 2. load and clean different formated deutscher Buchpreis sepratly ----
dbp_raw <- read_xlsx("../data/prizes_xlsx/deutscher_buchpreis.xlsx", skip = 3) %>%
  separate_rows(Gewonnen, Shortlist, Longlist, sep = "; ") %>%
  # produce some duplicates if books in multiple columns in excel
  pivot_longer(c(Gewonnen, Shortlist, Longlist),
    names_to = "Shortlist", values_to = "book",
    values_drop_na = T
  ) %>%
  unique() %>%
  mutate(
    Name = str_replace(Name, "(.+)(,\\ )(.+)", "\\3 \\1"),
    Geburtsjahr = ifelse(Geburtsdatum > 2020,
      as.Date(Geburtsdatum, "1899-12-30") %>%
        format("%Y") %>% as.numeric(),
      Geburtsdatum
    ),
    Sterbejahr = ifelse(Sterbedatum > 2020,
      as.Date(Sterbedatum, "1899-12-30") %>%
        format("%Y") %>% as.numeric(),
      Sterbedatum
    ),
    Muttersprache = str_replace_all(Muttersprache, "[ (?)]", ""),
    Literaturinstitut = sub("(ja|nein)[()A-z,. ]+", "\\1", Literaturinstitut),
    Jahr = str_sub(book, 1, 4),
    Titel = str_replace(book, "([0-9 ]*\\()(.*)(\\))", "\\2"),
    Gewonnen = ifelse(Shortlist == "Gewonnen", "ja", "nein"),
    Shortlist = ifelse(Shortlist == "Longlist", "nein", "ja"),
    Verlag = ifelse(str_detect(Verlag, Jahr), Verlag, Verlag2) %>%
      str_replace("[[:space:]]\\([0-9,; ]+\\)", ""),
    Jahr = as.numeric(Jahr),
    Preis = "deutscher"
  ) %>%
  select(!c(Verlag2, Geburtsdatum, Sterbedatum, book))


# ---- 3. combine all prizes togeher and rename and recode variables ----
prizes_df <- bind_rows(prizes_raw, dbp_raw) %>%
  rename(
    name = Name,
    female = Geschlecht,
    language = Muttersprache,
    ybirth = Geburtsjahr,
    ydeath = Sterbejahr,
    academic = Hochschulabschluss,
    institute = Literaturinstitut,
    title = Titel,
    ynom = Jahr,
    publisher = Verlag,
    prize = Preis,
    shortlist = Shortlist,
    winner = Gewonnen
  )

# print and stop if unclear informations for variables
test <- subset(prizes_df, grepl("\\?", female) |
  grepl("\\?", institute) | grepl("\\?", shortlist) |
  grepl("\\?", academic) | grepl("\\?", winner), )

if (nrow(test) > 0) {
  print(test)
  stop("unclear informations for some variables")
} else {
  rm(test)
}


prizes_df <- prizes_df |>
  mutate(across(
    c(shortlist, academic, institute, winner),
    ~ ifelse(str_detect(.x, "[jJ]a"), TRUE, FALSE)
  ),
  female = ifelse(female == "w", TRUE, FALSE),
  prize = as.factor(prize),
  shortlist = ifelse(prize %in% c(
    "aspekte", "leipziger", "oesterreich_debuet",
    "schweiz", "text_sprache"
  ), NA, shortlist),
  language = case_when(
    str_detect(language, "(Deutsch/)|(/Deutsch)") ~ "german+",
    str_detect(language, "Deutsch") ~ "german",
    !str_detect(language, "Deutsch") &
      !str_detect(language, "/") ~ "foreign",
    !str_detect(language, "Deutsch") ~ "foreign+"
  ),
  language = as.factor(language)
  ) |>
  separate(title,
    into = c("title", "subti"), sep = "[.][^.]*\\s*",
    fill = "right", extra = "merge"
  ) |>
  mutate(
    title = case_when(
      str_detect(title, "Vienna") ~ "Vienna",
      str_detect(title, "Die schärfsten Gerichte") ~ "Die schärfsten Gerichte der tatarischen Küche",
      str_detect(title, "Wann wird es endlich wieder so") ~ "Wann wird es endlich wieder so, wie es nie war",
      str_detect(title, "Notizen und Details") ~ "Notizen und Details 1964-2007",
      str_detect(title, "Ach, diese Lücke") ~ "Ach, diese Lücke, diese entsetzliche Lücke",
      str_detect(title, "Bild von einer Lüge") ~ "Bild von der Lüge",
      str_detect(title, "Blasmusikpop") ~ "Blasmusikpop oder Wie die Wissenschaft in die Berge kam",
      str_detect(title, "Das Glück der Frau Pfeifer") ~ "Das Glück von Frau Pfeiffer",
      str_detect(title, "Die Erfindung der Roten Armee Fraktion") ~ "Die Erfindung der Roten Armee Fraktion durch einen manisch depressiven Teenager im Sommer 1969",
      str_detect(title, "Die Liebe in großen Zügen") ~ "Die Liebe in groben Zügen",
      str_detect(title, "ich bin ein Feld voller Raps") ~ "ich bin ein Feld voller Raps verstecke die Rehe und leuchte wie dreizehn Ölgemälde übereinandergelegt",
      str_detect(title, "Reise bis an den Rand des Universums") ~ "Reise an den Rand des Universums",
      str_detect(title, "Wann wird es endlich wieder so") ~ "Wann wird es endlich wieder so, wie es nie war",
      str_detect(title, "An einem klaren, eiskalten Januarmorgen") ~ "An einem klaren, eiskalten Januarmorgen zu Beginn des 21. Jahrhunderts",
      str_detect(title, "Und was hat das mit mir zu tun") ~ "Und was hat das mit mir zu tun?",
      str_detect(title, "Mein Vater war ein Mann") ~ "Mein Vater war ein Mann an Land und im Wasser ein Walfisch",
      str_detect(title, "Schau mich an, wenn ich mit dir rede") ~ "Schau mich an, wenn ich mit dir rede!",
      str_detect(title, "Der Fuchs und Dr") ~ "Der Fuchs und Dr. Shimamura",
      str_detect(title, "Flut und Boden") ~ "Flut und Boden",
      str_detect(title, "Toni und Moni") ~ "Toni und Moni",
      str_detect(title, "3$") ~ "3000 Euro",
      str_detect(title, "Lebt wohl, Ihr Genossen und Geliebten") ~ "Lebt wohl, Ihr Genossen und Geliebten!",
      str_detect(title, "^Laura") ~ "Laura",
      TRUE ~ title
    ),
    subti = case_when(
      title == "Flut und Boden" ~ "Roman einer Familie",
      title == "Toni und Moni" ~ "oder: Anleitung zum Heimatroman",
      title == "Laura" ~ " oder die Tücken der Kunst; Roman",
      TRUE ~ subti
    ),
    title = str_squish(title),
    url_name = clean(name),
    match_id = cr_match_id(url_name, title)
  ) |>
  # remove wrong person (just illustrator, not author)
  filter(name != "Nanne Meyer")


# print and stop if inconsistent informations for each authors
test <- group_by(prizes_df, prize, ynom, match_id) %>%
  filter(n() > 1)

if (nrow(test) > 0) {
  print(test)
  stop("inconsistent informations for some authors")
} else {
  rm(test)
}


saveRDS(prizes_df, file = "../data/pt_prizes.RDS")

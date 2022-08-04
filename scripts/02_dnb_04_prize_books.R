
# load data
prizes_df <- readRDS("../data/pt_prizes.RDS")
dnb_books_all <- readRDS("../data/dnb_books_all.RDS")
dnb_authors_all <- readRDS("../data/dnb_authors.RDS")

# ---- 1. merge nominees with DNB data for correct author ID (and filter correct book) ----
prizes_dnb_unique <- left_join(prizes_df, dnb_books_all, by = c("match_id")) |>
  group_by(match_id, prize, ynom) |>
  mutate(
    same_match_id = n(), # info about number of book variants

    # indicator for sorting (keep early, german printed books)
    yearlater = ifelse(year > ynom, 1, 0), # in year or before year of nomination
    nogerman = ifelse(language.y != "ger", 1, 0), # german version
    nobook = ifelse(str_detect(pages, "(Online)|(CD)|(DVD)"), 1, 0) # printed books
  ) |>
  # add potential missing information to all variants of book
  # fill author IDs: same books twice, but without ID in first cases
  fill(c(author_id, ddc, subject, genre, topic), .direction = "updown") |>
  arrange(yearlater, nobook, year, nogerman, edition) |>
  filter(row_number() == 1) |>
  ungroup()


# check for authors & books with missing IDs (add later manually if possible)
# filter(prizes_dnb_unique, is.na(author_id)) |> select(match_id)

# check for ddc and language of nomineed books
# table(prizes_dnb_unique$ddc)
# table(prizes_dnb_unique$language.y)


# ---- 2. create small data.frame to match with dnb_all data to get correct author ID ----
prizes_authors_id <- prizes_dnb_unique |>
  select(author_search, author_id) |>
  unique() |>
  na.omit() |>
  rename(author_id_prize = author_id) |>
  # add missing ID manually
  bind_rows(data.frame(
    author_search = c("Koeck, Thomas", "Boettcher, Bas"),
    author_id_prize = c("106568679X", "129373796")
  ))



# ---- 3. filter DNB authors by correct ID from list of books of prizes ----
dnb_books_prize <- dnb_books_all |>
  full_join(prizes_authors_id, by = "author_search") |>
  # even authors with same name and book title -> different IDs
  # get all IDs for authors (checked later and removed three errors)
  group_by(match_id) |>
  mutate(
    author_id_n = n_distinct(author_id),
    author_id_all = ifelse(author_id_n > 1,
      paste(na.omit(unique(author_id)), collapse = ";"),
      author_id
    )
  ) |>
  # if just one unique author ID -> fill empty to keep them all
  group_by(author_search) |>
  arrange(author_id_all) |>
  mutate(
    author_id_n = n_distinct(author_id_all, na.rm = TRUE),
    author_id_all = ifelse(author_id_n == 1, author_id_all[1], author_id_all)
  ) |>
  # keep just valid titles (author_id correct or if missing: belletristic)
  mutate(
    keep = case_when(

      # if author_id_prize (correct ID) is in author_id (all if multiple combined)
      str_detect(author_id_all, author_id_prize) ~ TRUE,

      # if missing: belletristic
      is.na(author_id_all) & (str_detect(ddc, "(^8.+)|B") | str_detect(subject, "(^8.+)|B")) ~ TRUE,
      TRUE ~ FALSE
    ),
    keep = case_when(

      # keep just german publications
      !str_detect(language, "ger|gsw") ~ FALSE,
      str_detect(publisher, "(Germanic Studies)|(Peters Group)") ~ TRUE,
      str_detect(publisher, "(Mouton)|(Helsingfors)|(Göteborg)|(Athena$)|(Athi̲na)|(Saint-Nazaire)|(Ōsaka)|(Wrocław)|(México)|(Chur)|(Athi̲na)|(Tel Aviv)|(London)|(New York)|(Manchester)|(San Diego)|(Paris)|(Todmorden)|(Maastricht)|(Eumo Editorial,)|(Argentinien)|(Austin)|(Shoshihanjitsukan)|(Todmorden)|(Montmorency)|(Devesset)|(Cairo)|(Černivci)|(Athēna)|(Pantin)|(São Paulo)|(Porto)|(Ourense)|(San Antonio)|(İstanbul)|(Vianen)|(Washington)|(Malmö)|(Zagreb)|(Bratislava)|(Antwerpen)|(Hanoi)|(Međunarodni)|(Taipei)|(København)|(Amsterdam)|(Stockholm)|(Kyôto)|(Oslo)|(RedFish)|(Warszawa)|(Poznań)|(Kraków)|(Madrid)|(Barcelona)|(Beograd)|(Viby)|(Milano)|(Roma$)|(Utrecht)|(Brussum)|(Baarn)|(Helsingissä)|(Espoo)|(Ljubljana)|(Budapest)|(Lisboa)|(Istanbul)|(Minnesota)|(Lausanne)|(Alidades)|(Holderbank SO)|(Helsingissä)|(Tallinn)|(Praha)|(Westbroek)|(Torino)|(Zaprešić)|(Alinea)|(Ronsosha)|(Bei jing)|(Zlín)|(Shang hai)") ~ FALSE,

      # remove same name and title - title wrong for one author
      id == "1021842842" ~ FALSE, # uwe-timm_gegen-das-_39
      id == "951971999" ~ FALSE, # thomas-meyer_stand-und-_16

      # remove wrong classified book for multiple authors
      match_id == "joerg-albrecht_hamburg_7" ~ FALSE, # joerg-albrecht_hamburg_7

      # remove books that are just readings of own oder other books
      str_detect(title, "\\sliest[:, ]") ~ FALSE,

      # additional erros (found while checking for slightly wrong named titles)
      id == "1189429640" ~ FALSE, # corrupt title, just a review: https://d-nb.info/1189429640
      id == "1030126534" ~ FALSE, # not uniquely assignable to one book although German title
      id == "117430622X" ~ FALSE, # two books in one book
      id == "1163886785" ~ FALSE, # two books in one book
      id == "121961047X" ~ FALSE, # two books in one book
      id == "1079524665" ~ FALSE, # two books in one book
      id == "950109061" ~ FALSE, # two books in one book
      id == "1048428648" ~ FALSE, # two books in one book
      id == "1247088383" ~ FALSE, # two books in one book
      id == "1209294761" ~ FALSE, # reading by another person
      id == "1209292084" ~ FALSE, # reading by another person
      id == "1199129089" ~ FALSE, # reading by another person
      id == "831101857" ~ FALSE, # not a book, chapter of another book
      id == "1204582319" ~ FALSE, # three books
      id == "931082889" ~ FALSE, # Schnurrer-Geschichten
      id == "1237634121" ~ FALSE, # Programmheft der Nibelungen-Festspiele 2021
      id == "1219618160" ~ FALSE, # Die Frankfurtnacht - Panikherz. Das Live-Dokument

      TRUE ~ keep
    )
  ) |>
  filter(keep) |>
  # # harmonize books; slightly different titles, but same books from same author
  mutate(title = case_when(
    str_detect(title, "Abstecher") & author_search == "Walser, Martin" ~ "Der Abstecher",
    str_detect(title, "Alle 7 Wellen") ~ "Alle sieben Wellen",
    str_detect(title, "Am Felsfenster morgens") ~ "Am Felsfenster morgens (und andere Ortszeiten 1982 - 1987)",
    str_detect(title, "Auf den Tag") ~ "Auf den Tag genaue Gedichte",
    str_detect(title, "Autorenwitwe") ~ "Die Autorenwitwe",
    str_detect(title, "Berlin in 100 Kapiteln") ~ "Berlin in 100 Kapiteln - von denen leider nur 13 fertig wurden",
    str_detect(title, "Bluemoon") ~ "Bluemoon Baby",
    str_detect(title, "Böselkraut") ~ "Böselkraut und Ferdinand",
    str_detect(title, "Bündner Trilogie") ~ "Die Bündner Trilogie",
    str_detect(title, "Bulle & Bär: Der Weg des Schwerts") ~ "Der Weg des Schwerts",
    str_detect(title, "Cromwell und andere Stücke") ~ "Cromwell",
    str_detect(title, "Das Gift der Welt") ~ "Das Gift der Welt",
    str_detect(title, "Das Glück ist eine Bohne") ~ "Das Glück ist eine Bohne",
    str_detect(title, "Das habe ich jetzt akustisch nicht verstanden") ~ "Das habe ich jetzt akustisch nicht verstanden",
    str_detect(title, "Das Schweigen am") ~ "Das Schweigen am andern Ende des Rüssels",
    str_detect(title, "Das war ich nicht") ~ "Das war ich nicht",
    str_detect(title, "Das Wetter vor") ~ "Das Wetter vor 15 Jahren",
    str_detect(title, "((Der fremde Freund)|(Drachenblut))") ~ "Der fremde Freund|Drachenblut",
    str_detect(title, "Der Goalie bin") ~ "Der Goalie bin ig",
    str_detect(title, "Der Grundri(ß|ss) von Grillparzer[s]* Bühnenkunst") ~ "Der Grundriss von Grillparzers Bühnenkunst",
    str_detect(title, "Der Kotti") ~ "Der Kotti - die Versteigerung von No. 36",
    str_detect(title, "Dialog Christ - Marxist") ~ "Dialog Christ-Marxist",
    str_detect(title, "Die Gallist_l28sche Krankheit") ~ "Die Gallistl'sche Krankheit",
    str_detect(title, "Die Inseln im Landmeer und neue Gedichte") ~ "Die Inseln im Landmeer",
    str_detect(title, "Die Lehmann-Trilogie") ~ "Die Lehmann Trilogie",
    str_detect(title, "Die Liebe zu Zeit") ~ "Die Liebe zur Zeit des Mahlstädter Kindes",
    str_detect(title, "Die Rosenbaum-Doktrin") ~ "Die Rosenbaum-Doktrin",
    str_detect(title, "Die Ritter der Tafelrunde und andere Stücke") ~ "Die Ritter der Tafelrunde",
    str_detect(title, "Die Vermessung der Welt") ~ "Die Vermessung der Welt",
    str_detect(title, "Don Juan \\(erzählt von ihm selbst\\)") ~ "Don Juan",
    str_detect(title, "Dorfgeschichten") ~ "Dorfgeschichten 1960",
    str_detect(title, "du livre") ~ "L'Emergence du livre",
    str_detect(title, "Duden, Schreiben auf Reisen") ~ "Schreiben auf Reisen",
    str_detect(title, "Duden, Schreiben dicht am Leben") ~ "Schreiben dicht am Leben",
    str_detect(title, "Duden Kreatives Schreiben - Schreiben über mich selbst.") ~ "Schreiben über mich selbst",
    str_detect(title, "Ein Flugzeug über dem Haus") ~ "Ein Flugzeug über dem Haus und andere Geschichten",
    str_detect(title, "Ein Haus auf dem Land/ Eine Wohnung in der Stadt") ~ "Ein Haus auf dem Land",
    str_detect(title, "Ein Mann von 40 Jahren") ~ "Ein Mann von vierzig Jahren",
    str_detect(title, "Einladung zum Lever Bourgeoise") ~ "Einladung zum Lever Bourgeois",
    str_detect(title, "Ende des Vogelgesanges") ~ "Ende des Vogelgesangs",
    str_detect(title, "Erfahrungen beim Verfassen einer Sonntagsrede") ~ "Erfahrungen beim Verfassen einer Sonntagsrede",
    str_detect(title, "Es Härz für Gaukler") ~ "Ein Herz für Gaukler",
    str_detect(title, "Fährmann, hol über!") ~ "Fährmann, hol über! Oder Wie man das Johannesevangelium pfeift",
    str_detect(title, "Gaud gägen Nordwind") ~ "Gut gegen Nordwind",
    str_detect(title, "Generationen") ~ "Generationen-Bilder",
    str_detect(title, "Gentlemen") ~ "Gentlemen, wir leben am Abgrund",
    str_detect(title, "Geschmeidig, brutal, snobistisch und sexy") ~ "Geschmeidig, brutal und sexy",
    str_detect(title, "Grand Hotel") ~ "Grandhotel",
    str_detect(title, "Hänsel und Gretel") ~ "Hänsel und Gretel und der Hund und der Bär",
    str_detect(title, "Helene Hegemann über Patti Smith - KiWi Musikbibliothek, Band 13") ~ "Helene Hegemann über Patti Smith, Christoph Schlingensief, Anarchie und Tradition",
    str_detect(title, "Herzl relo_372ded") ~ "Herzl reloaded",
    str_detect(title, "Himmel unter Berlin") ~ "Der Himmel unter Berlin",
    str_detect(title, "Ich dyl an ich") ~ "Ich Dylan ich",
    str_detect(title, "Ja und Nein") ~ "Ja und Nein",
    str_detect(title, "Jenseitsnovelle \\(DAISY\\)") ~ "Jenseitsnovelle",
    str_detect(title, "Kaspar Mauser") ~ "Kaspar Mauser - die Feigheit vorm Freund",
    str_detect(title, "Kirillov") ~ "Kirillow",
    str_detect(title, "Kristof Magnusson über Pet Shop Boys") ~ "Kristof Magnusson über Pet Shop Boys, queere Vorbilder und musikalischen Mainstream",
    str_detect(title, "Lasalle fragt Herrn Herbert nach Sonja") ~ "Lassalle fragt Herrn Herbert nach Sonja",
    str_detect(title, "Leeres Viertel Rub' Al -Khali") ~ "Leeres Viertel - Rubʿ al-Khali",
    str_detect(title, "Lieber Mischa .. der Du fast Schlomo Adolf Grinblum geheißen hättest, es tut mir so leid, dass ich Dir das nicht ersparen konnte: Du bist ein Jude ..") ~ "Lieber Mischa",
    str_detect(title, "Liebesmale, scharlachrot") ~ "Liebesmal, scharlachrot",
    str_detect(title, "London – Lieblingsorte") ~ "London",
    str_detect(title, "Mal hören, was noch kommt/Jetzt, wo alles zu spät is. Zwei Erzählungen. Trivialroman") ~ "Mal hören, was noch kommt",
    str_detect(title, "Manifest") ~ "Manifest zur riesen Schreibmaschine",
    str_detect(title, "Mauer-Park") ~ "Mauer Park",
    str_detect(title, "Meienbergs Tod/Die sexuellen Neurosen unserer Eltern/Der Bus") ~ "Meienbergs Tod",
    str_detect(title, "Mozart - im Innern seiner Sprachen") ~ "Mozart im Innern seiner Sprachen",
    str_detect(title, "Meine Zeit, mein Tier") ~ "Mandelstam. Meine Zeit, mein Tier",
    str_detect(title, "Nach Agyppten") ~ "Nach Ägypten",
    str_detect(title, "Mitternachtsweg") ~ "Mitternachtsweg",
    str_detect(title, "Neujahr von Juli Zeh.") ~ "Neujahr",
    str_detect(title, "Pampa-Blues") ~ "Pampa Blues",
    str_detect(title, "Poesias") ~ "Poesias dals prüms pleds",
    str_detect(title, "Poesie und Natur/Natur und Poesie") ~ "Poesie und Natur",
    str_detect(title, "Professor Gottfried Keller?") ~ "Gottfried Keller",
    str_detect(title, "Raumlicht") ~ "Raumlicht, der Fall Evelyne B.",
    str_detect(title, "Sägespäne für mein Herzbluten") ~ "Sägespäne für mein Herzbluten",
    str_detect(title, "Scherbenpark von Alina Bronsky.") ~ "Scherbenpark",
    str_detect(title, "Seemannsbraut") ~ "Die Seemannsbraut",
    str_detect(title, "September-Song") ~ "September Song",
    str_detect(title, "Sex 2") ~ "Sex II",
    str_detect(title, "Simple Stories") ~ "Simple Storys",
    str_detect(title, "Sinfonie Nr. 9") ~ "Sinfonia N. 9",
    str_detect(title, "Sprung in den Papierkorb") ~ "Der Sprung in den Papierkorb",
    str_detect(title, "Stille in Prag") ~ "Die Stille in Prag",
    str_detect(title, "Tagesgezeiten") ~ "Tageszeiten",
    str_detect(title, "Tanz der Tiefseequalle") ~ "Tanz der Tiefseequalle",
    str_detect(title, "Theaterstücke 1986-2008") ~ "Theaterstücke",
    str_detect(title, "Tschick von Wolfgang Herrndorf. Königs Erläuterungen.") ~ "Tschick",
    str_detect(title, "Über die Schädlichkeit des Tabaks oder: Ein älterer Herr hält eine Rede vor Abiturienten") ~ "Über die Schädlichkeit des Tabaks",
    str_detect(title, "Umwertung aller Werte?") ~ "Umwertung aller Werte? Deutsche Literatur im Urteil Nietzsches",
    str_detect(title, "Warum die Sache schief geht") ~ "Warum die Sache schiefgeht",
    str_detect(title, "Wie das Schwein zu Tanze ging") ~ "Wie das Schwein zum Tanze ging", # check
    str_detect(title, "Wie der Soldat das") ~ "Wie der Soldat das Grammofon repariert",
    str_detect(title, "Wunde r") ~ "WUNDER",
    str_detect(title, "Zmittst im Gjätt uss") ~ "Mitten im Nirgendwo",
    str_detect(title, "Zweisamkeit der Einzelgänger") ~ "Die Zweisamkeit der Einzelgänger",
    TRUE ~ title
  )) |>
  # summarize multiple book variants and keep just first printed version
  group_by(match_id) |>
  mutate(
    title_variants = n(), # info about number of book variants

    # indicator for sorting (keep printed books)
    nobook = ifelse(str_detect(pages, "(Online)|(CD)|(DVD)"), 1, 0),
    across(
      c(genre, topic, keyword),
      ~ paste(na.omit(unique(.x)), collapse = ";")
    )
  ) |>
  arrange(match_id, nobook, year) |> # sort for year first (books for nomination)

  # add potential missing information to all variants of book
  fill(c(ddc, subject), .direction = "updown") |>
  filter(row_number() == 1) |>
  # fill missing information by author
  group_by(author_search) |>
  fill(c(author_id, author_year), .direction = "updown") |>
  ungroup() |>
  # check for books with multiple titles by same (shortened) match_id
  # -> slightly different titles -> clean further and split subtitle from title
  mutate(match_id_short = str_extract(match_id, "^.+?_[^_]{1,5}")) |>
  group_by(match_id_short) |>
  mutate(
    titles_multi = n_distinct(title) > 1,
    title_short = ifelse(titles_multi, title, NA),
    title_short = case_when(
      # split on colon
      str_detect(title, "[:].+") ~ str_extract(title, ".*?:") |> str_remove("[:]"),
      # split on dot (if not in front of Nr.)
      str_detect(title, "(?!N[ro])\\s*[.].+") ~ str_extract(title, ".*?(?!N[ro])\\s*[.]") |> str_remove("[.]"),
      # split on hyphen
      str_detect(title, " [-].+") ~ str_extract(title, ".*?-") |> str_remove("[-]"),
      # split on or (oder)
      str_detect(title, " oder.+") ~ str_extract(title, ".*?oder") |> str_remove("oder"),
      TRUE ~ title
    ),
    title_short = str_remove_all(title_short, "[[:punct:]]") |>
      tolower() |>
      str_squish()
  ) |>
  group_by(author_search, title_short) |>
  arrange(author_search, desc(title_variants), year) |> # keep with most variants or earliest
  mutate(title_variants = sum(title_variants)) |> # add all title-variantes togehter
  filter(row_number() == 1) |>
  ungroup() |>
  # add author information from DNB (wikipedia/wikidata infos)
  left_join(dnb_authors_all, by = "author_id") |>
  # rename and select necessary columns
  rename(url_book_dnb = link) |>
  rename_with(
    ~ paste0(.x, "_dnb"),
    c(
      authors, author_id, title, subtitle, publisher, year,
      isbn, keyword, genre, ddc, subject, title_variants, contribs
    )
  ) |>
  select(
    author, ends_with("_dnb"), match_id,
    author_id_dnb, url_book_dnb, title_variants_dnb, name, wikipedia, wikidata
  )


saveRDS(dnb_books_prize, "../data/dnb_books_prize.RDS")

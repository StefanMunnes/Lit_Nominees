
# ---- clean and prepare DNB author & title data ----

dnb_books_all <- readRDS("../data/dnb_books_raw.RDS") |>

  mutate(

  ### 1 split and clean author informations (names + IDs)

    # combine all authors and contributors with role in parentheses
    contribs = paste(author, contrib, sep = ";"),

    # remove wrongly added parentheses with involvement
    author = str_remove(author, "\\s*\\([a-z]+\\)"),

    # add parentheses with autor involvement for author
    author = ifelse(!is.na(author), paste(author, "(aut)"), NA),

    # add single author with multiple authors in contrib to split and select right one
    author = ifelse(!is.na(contrib), stri_join(author, contrib, sep = "; "), author),
    author_id = ifelse(!is.na(contrib_id), stri_join(author_id, contrib_id, sep = "; "), author_id),

    # add all authors together for books with multiple authors in one variable
    authors = map_chr(str_extract_all(author, "([^;]+?)(\\(aut\\))"),
                      ~ str_c(.x, collapse = ";"))) |>

  # split mutliple authors/contributors in single rows -> filter later by name
  separate_rows(author, author_id, sep = "; ") |>

  # remove person-books if not author (info came from contrib after separate)
  filter(str_detect(author, "\\(aut\\)")) |>

  mutate(

    # some names with year of birth
    author = str_remove_all(author, "[0-9]+"),

    # remove author info parentheses
    author = str_remove(author, "\\(aut\\)") |>

    # clean
      stri_trans_general("de-ASCII") |> # Umlaute
      str_remove_all("\\'|\\.") |>  # Ol'ga; Seitz, Clemens J.
      str_remove_all("[()]") |>
      str_remove(",*\\s*-*\\s*$") |>
      str_squish(),

    # correct names (back to original long name)
    across(c(author, author_search),
           ~ case_when(str_detect(.x, "Cole, Isabel") ~ "Cole, Isabel Fargo",
                       str_detect(.x, "Altwasser, Volker") ~ "Altwasser, Volker Harry",
                       str_detect(.x, "Salzmann, Marianna") ~ "Salzmann, Sasha Marianna",
                       str_detect(.x, "Carmen Francesca") ~ "Banciu, Carmen-Francesca",
                       TRUE ~ .x)),

    # name inconsistent (see xlsx-file for manually checking)
    author = case_when(author == "Kaiman Kehlmann, Daniel" ~ "Kehlmann, Daniel",
                       author == "Adler, Helena Este" ~ "Adler, Helena",
                       author == "Bodrozic Marica, Marica" ~ "Bodrozic, Marica",
                       author == "Monika Helfer, Monika" ~ "Helfer, Monika",
                       author == "Gauss, Karl Markus" ~ "Gauss, Karl-Markus",
                       author == "Ti mu" ~ "Timm, Uwe",
                       author == "Xia lang si ji" ~ "Schalansky, Judith",

                       TRUE ~ author),

    # first letter of each part of name in uppercase
    auhor = str_to_title(author),

    # NA for author_id if NOID wildcard (keep right order for spliting contribs)
    author_id = ifelse(author_id == "NOID", NA, author_id),

    # create correct ID for later scrape purpose
    author_id = str_remove_all(author_id, "\\(DE-588\\)")
  ) |>


  ### 2 keep just observations with same author name as search name (separated co-authors)
  filter(author_search == author) |> # was checked manually if names just vary slightly


  ### 3 clean up title data
  mutate(

    # remove some special characters in 5 titles
    title = str_remove_all(title, "(\u0098)|(\u009c)"),

    # problems with wrong Umlaute
    across(contains("title"), stri_trans_nfc),

    # remove additional parantheses in title
    title = str_remove(title, "\\(.+?kürzt.*?\\s*(Lesung|Fassung)*\\)\\s*$"),
    title = str_remove_all(title, "\\(((neobooks Singles*)|(.*?Hörspiel.*?)|(MP3.*?)|(.*?Download))\\)\\s*$"),
    title = str_remove_all(title, "\\(((Autorisierte Lesefassung)|(Erweiterte Ausgabe)|(.*?Autorenlesung)|(NA))\\)\\s*$"),
    title = str_remove_all(title, "(\\(Markus-Cheng-Reihe.+\\))|(\\s-\\sLiteratur-Quickie)|(\\.\\sKönigs Erläuterungen)\\s*$"),
    title = str_remove(title, "^(Duve|Timm|Nadolny|Walser|Schulz[e]*), "), # name of author in front of some titles

    # change (sub-)titles manually to fit with PT and publisher
    title = case_when(
      str_detect(title, "Brüder und Schwestern") ~ "Brüder und Schwestern",
      str_detect(title, "Notizen und Details") ~ "Notizen und Details 1964-2007",
      str_detect(title, "Rechtes Denken") ~ "rechtes denken/europa verteidigen/sterben helfen",
      str_detect(title, "Toni und Moni") ~ "Toni und Moni",
      str_detect(title, "Der Träumer") ~ "Die Träumer",
      str_detect(title, "Alias") ~ "Alias",
      str_detect(title, "^Laura")  ~ "Laura",
      str_detect(title, "Feld voller Raps") ~ "ich bin ein Feld voller Raps verstecke die Rehe und leuchte wie dreizehn Ölgemälde übereinandergelegt",
      str_detect(title, "Coby County") ~ "Schimmernder Dunst über CobyCounty",

      TRUE ~ title),

    title = str_squish(title),

    subtitle = case_when(
      str_detect(title, "Brüder und Schwestern") ~ stri_join(str_extract(title, "die Jahre.*"), subtitle, sep = " ; "),
      str_detect(title, "Alias") ~ "oder das wahre Leben. Roman",
      str_detect(title, "^Laura")  ~ "oder die Tücken der Kunst. Roman",
      str_detect(title, "Toni und Moni") ~ "oder: Anleitung zum Heimatroman",
      str_detect(title, "Feld voller Raps") ~ "Neue Gedichte",

      TRUE ~ subtitle),

    # clean year
    year = case_when(str_detect(year, ".+?s.+?\\|.+") ~
                       str_replace(year, ".+?s([0-9]{4,4}).+?\\|.+", "\\1"), # e.g. 020123s2001    gw |||||bm||| 00||||ger
                     str_detect(year, "[0-9]{4,4}") ~ str_extract(year, "[0-9]{4,4}"), # just clean 4 digits years left
                     str_detect(year, "o\\.\\s*J\\.") ~ "", # ohne Jahr, abbreviation
                     str_detect(year, "\\[|\\]") ~ str_remove_all(year, "[\\[\\]]"), # remove brackets
                     str_detect(year, "^2[0-9]$") ~ paste0("20", year), # add century if just two digits
                     TRUE ~ year) |> as.numeric(),

    # create match_id (and name in right order before)
    name = str_replace(author, "(.+?),\\s(.+)", "\\2 \\1"),
    match_id = cr_match_id(clean(name), title))


saveRDS(dnb_books_all, "../data/dnb_books_all.RDS")

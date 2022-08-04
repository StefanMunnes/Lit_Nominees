
# Ziel: Datensatz der für alle deutschsprachigen Preise Wikipedia Links der
#   der Gewinner:innen enthält, über die später gematcht und gezählt werden kann
#   Datensatz enthält Jahr und Name des Preises + Wikipediaseite

# Übersicht über verschiedene Darstellungen der Preisträger:innen
# 1. einfache Liste in einem Abschnitt Preisträger
# 2. einfache Tabelle in einem Abschnitt Preisträger
# 3. Tabellen mit mehreren Spalten (mehrere Preise pro Jahr, oder Jahre)
# 4. Tabellen mit rowspan (zumindest ein Jahr über mehrer Jahre)
# 5. Listen und Tabellen zusammen
# 6. Listen/Tabellen nach Jahren als Überschrift oder nur bold
# 7. genestete Listen -> Unterlisten müssen Jahre hinzugefügt bekommen und
#     aus erster Liste entfernen
# 8. Listen nach Jahren (aber kein heading tag für split, sondern dl/dt)


# ---- extract prize and author information from raw htmls to data.frame ----

# load list of raw htmls with urls as names
htmls_ls <- readRDS("../data/wp_prizes_htmls.RDS")

# prepare vectors and regular expressions for extraction function
chapters_ignore <- c(
  "Einzelnachweise", "Zitatnachweis", "Siehe auch", "Fußnoten",
  "Jury", "Die Jury", "Quellen", "Geschichte", "Literatur",
  "Weiterführende Literatur", "Organisation",
  "Notizen", "Anmerkungen", "Weblinks", "Weblink",
  "Mitglieder", "Mitglieder (Auswahl)"
)

# set names of table columns to be extracted in loop function
table_winner <- c("Preisträger", "Autor")

# set regular expression of year to extract (some are still different in funct)
regex_year <- "\\b(19[1-9]|20[0-2])[0-9]\\b"


# source function to extract informations from raw html
source("../scripts/03_wp_prizes_all_02_func.R")


# run function over all raw htmls -> get list of data.frames for each prize
prizes_ls <- wiki_prizes(htmls_ls)

# bind to one data.frame
prizes_raw <- bind_rows(prizes_ls)


saveRDS(prizes_raw, file = "../data/wp_prizes_raw.RDS")

# [names(htmls_ls) == "/wiki/Die_Kogge"]

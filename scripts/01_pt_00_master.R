if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  "readxl",
  "tidyr", # separate(), pivot_longer()
  "dplyr", # mutate(), distinct(), full_join()
  "stringr", # str_detect(), str_replace(), str_extract()
  "stringi", # stri_trans_general()
  "reshape2", # melt
  "rvest", # read_html(), html_nodes(), html_text()
  "purrr", # keep()
  "ggplot2",
  "forcats",
  "rdnb", # Deutsche Nationalbibliothek
  "httr",
  "xml2",
  "jsonlite", # fromJSON
  # "wikipediatrend", # Wikidata
  "lubridate", # interval
  "stargazer",
  "haven",
  "sandwich",
  "margins",
  "dotwhisker",
  "ggpubr",
  "modelsummary",
  "flextable",
  "officer",
  "arsenal",
  "broom",
  "fastDummies"
)

try(setwd("scripts/"))


# create function to clean names
clean <- function(var) {
  tolower(var) |>
    stringr::str_remove_all("[!\"#$%&'()*+,.:;<=>?@^_\\[\\]{|}~/]") |> # keep -
    stringr::str_replace_all(" ", "-") |>
    stringi::stri_trans_general("de-ASCII")
}

cr_match_id <- function(var1, var2, len = 10) {
  paste(
    var1,
    substr(clean(var2), 1, len),
    stringr::str_length(clean(var2)),
    sep = "_"
  )
}

url_decode_utf <- function(x) {
  y <- URLdecode(x)
  Encoding(y) <- "UTF-8"
  y
}

# source("scripts/01_prizes_xlsx.R", print.eval = T, encoding = "utf-8")
#
# # run carefully: scrapes web data from perlentaucher.de with delay time
# source("scripts/02_books_pt.R", print.eval = T, encoding = "utf-8")
#
# # run carefully: scrapes web data from perlentaucher.de with delay time
# source("scripts/03_reviews_pt.R", print.eval = T, encoding = "utf-8")
#
# source("scripts/04_combine_data.R")

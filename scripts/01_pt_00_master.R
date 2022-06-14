
if(!require("pacman")) install.packages("pacman")
pacman::p_load("readxl",      # read_xlsx()
               "tidyr",       # separate(), pivot_longer()
               "dplyr",       # mutate(), distinct(), full_join()
               "stringr",     # str_detect(), str_replace(), str_extract()
               "stringi",     # stri_trans_general()
               "reshape2",    # melt
               "rvest",       # read_html(), html_nodes(), html_text()
               "purrr",       # keep()
               "ggplot2",
               "rdnb", "httr", "xml2") # Deutsche Nationalbibliothek


# create function to clean names
clean <- function(var) {
  tolower(var) |>
    str_remove_all("[!\"#$%&'()*+,.:;<=>?@^_\\[\\]{|}~/]") |> # keep -
    str_replace_all(" ", "-") |>
    stri_trans_general("de-ASCII") # in alter Version latin-ascii und Umlaute händisch
}

cr_match_id <- function(var1, var2, len = 10) {
  paste(var1, substr(clean(var2), 1, len), str_length(clean(var2)), sep = "_")
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

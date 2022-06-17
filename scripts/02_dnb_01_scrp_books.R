
# ---- 1. prepare unique author names for DNB scraping loop ----
authors_dnb <- readRDS("../data/pt_prizes.RDS") |>
  pull(name) |>
  unique() |>
  stri_trans_general("de-ASCII") |>
  str_remove_all("\\.") |>
  str_replace("(.*)\\s([A-z-]+$)", "\\2, \\1") |>

  # correct name (two last names)
  str_replace("Abonji, Melinda Nadj", "Nadj Abonji, Melinda") |>

  # shorten names (otherwise not found at DND) -> change back later
  str_replace("Altwasser, Volker Harry", "Altwasser, Volker") |>
  str_replace("Cole, Isabel Fargo", "Cole, Isabel") |>
  append("Salzmann, Marianna") # same as original: Salzmann, Sasha Marianna



# ---- 2. scrape DNB by author names ----

# load modified functions from dnb-package to get also (co-)author id and byears
source("dnb_fnct/gets.R")
source("dnb_fnct/utils.R")


dnb_books_ls <- lapply(authors_dnb, function(author) {

  Sys.sleep(1)

  message(author)

  dnb_advanced_sm(paste0("atr='", author), limit = "all", clean = T) # "' AND mat='books'"
})


names(dnb_books_ls) <- authors_dnb

dnb_books_raw <- bind_rows(dnb_books_ls, .id = "author_search")

saveRDS(dnb_books_raw, file = "../data/dnb_books_raw.RDS")

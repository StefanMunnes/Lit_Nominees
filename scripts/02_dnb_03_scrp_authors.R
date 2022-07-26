
# ---- scrape author information from DNB with unique IDs ----

dnb_authors_ids <- readRDS("../data/dnb_books_all.RDS") |>
  pull(author_id) |>
  unique() |>
  na.omit()


url_base <- "https://hub.culturegraph.org/entityfacts/"

dnb_authors_ls <- lapply(dnb_authors_ids, function(id) {
  Sys.sleep(1)

  message(id)

  url <- paste0(url_base, id)

  json <- GET(url) |>
    content(as = "text") |>
    fromJSON()

  list <- list(
    author_link = json$`@id`, author_name = json$preferredName,
    author_surname = json$surname, author_forname = json$forename,
    name_variant = json$variantName, pseudonym = json$pseudonym$preferredName,
    gender = json$gender$label,
    academic = json$academicDegree,
    ybirth = json$dateOfBirth, ydeath = json$dateOfDeath,
    pbirth = json$placeOfBirth$preferredName,
    pdeath = json$placeOfDeath$preferredName,
    pactiv = json$placeOfActivity$preferredName,
    country = json$associatedCountry$preferredName,
    profession = json$professionOrOccupation$preferredName,
    other = json$biographicalOrHistoricalInformation,
    affiliation = json$affiliation$preferredName,
    wikipedia = json$sameAs$`@id`[json$sameAs$collection$abbr == "dewiki"],
    wikidata = json$sameAs$`@id`[json$sameAs$collection$abbr == "WIKIDATA"]
  )

  map(list, ~ ifelse(is.null(.x), NA, paste(.x, collapse = ";"))) |>
    data.frame()
})

names(dnb_authors_ls) <- dnb_authors_ids

dnb_authors_df <- bind_rows(dnb_authors_ls, .id = "author_id") |>
  mutate(across(c(wikipedia, wikidata), ~ na_if(.x, "")))


# ---- 1.2  get missing wikipedia links from wikidata ----
wiki_miss <- filter(dnb_authors_df, is.na(wikipedia) & !is.na(wikidata))
#|> mutate(wikidata = ifelse())

# https://www.wikidata.org/wiki/Q112533155

wiki_miss_new <- lapply(wiki_miss$wikidata, function(url) {
  message(url)

  json <- GET(paste0(url, ".json")) |>
    content(as = "text") |>
    fromJSON()

  wikipedia <- json[[1]][[1]]$sitelinks$dewiki$url

  data.frame(
    wikidata = url,
    wikipedia_new = ifelse(is_empty(wikipedia), NA, wikipedia)
  )
}) |>
  bind_rows() |>
  filter(!is.na(wikipedia_new))


# ---- 1.3 add missing wikipedia links to first data.frame -----
dnb_authors_df <- full_join(dnb_authors_df, wiki_miss_new, by = "wikidata") |>
  mutate(wikipedia = ifelse(is.na(wikipedia), wikipedia_new, wikipedia)) |>
  select(!wikipedia_new)


saveRDS(dnb_authors_df, file = "../data/dnb_authors.RDS")

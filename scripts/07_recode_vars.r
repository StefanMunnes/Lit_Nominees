
nominees <- readRDS("../data/nominees.RDS")

nominees_rec <- nominees |>
  # Recode language
  mutate(
    language_german = case_when(
      language == "foreign" ~ FALSE,
      language == "foreign+" ~ FALSE,
      TRUE ~ TRUE
    ),
    # age as groups
    age_group = case_when(
      age_nom < 43 ~ "18-42",
      between(age_nom, 43, 66) ~ "43-66",
      age_nom > 66 ~ "67-92"
    ) |> factor(level = c("18-42", "43-66", "67-92")),
    # jury gender share as groups
    jury_group = case_when(
      jury_fem < 0.5 ~ "male",
      jury_fem == 0.5 ~ "even",
      jury_fem > 0.5 ~ "female"
    ) |> factor(level = c("male", "even", "female")),
    #  reviewers gender share as groups
    revs_group = case_when(
      revs_fem < 0.5 ~ "male",
      revs_fem == 0.5 ~ "even",
      revs_fem > 0.5 ~ "female"
    ) |> factor(level = c("male", "even", "female")),
    prevprize = case_when(
      wikiprizes_pre == 0 ~ "none",
      wikiprizes_pre > 0 & wikiprizes_pre < 8 ~ "medium",
      wikiprizes_pre >= 8 ~ "many"
    ) |> factor(level = c("none", "medium", "many")),
    # prevbooks as groups
    prevbooks = case_when(
      books_dnb_prev == 0 ~ "none",
      books_dnb_prev > 0 & books_dnb_prev < 11 ~ "medium",
      books_dnb_prev > 10 ~ "many"
    ) |> factor(level = c("none", "medium", "many")),
    # sentiment variation 0 if no variation but valid sentiment values
    senti_vari = ifelse(!is.na(senti_mean) & is.na(senti_vari), 0, senti_vari),
    # gen homophily variable
    homophily = case_when(
      jury_group == "male" & !female ~ TRUE,
      jury_group == "female" & female ~ TRUE,
      TRUE ~ FALSE
    ),
    # wikiviews NA = 0 & topcoding to erase extrem values
    wv_mean = ifelse(is.na(wv_mean), 0, wv_mean),
    wv_mean = ifelse(wv_mean > 100, 100, wv_mean),
    # nonfiction
    nonfiction = case_when(
      poetry == "Y" ~ TRUE,
      str_detect(tpcs, "Lyrik") ~ TRUE,
      str_detect(topics_orig, "Biografie|Autobiografie|Lyrik") ~ TRUE,
      TRUE ~ FALSE
    ),
    # revs N -> NA to 0
    revs_n = ifelse(is.na(revs_n), 0, revs_n),
    # create Zeitgeist variables
    metoo = ifelse(ynom < 2017, "Before #metoo", "After #metoo") |>
      as.factor() |> forcats::fct_relevel(rev),
    syria = ifelse(ynom < 2015, "Before 2015", "After 2015") |>
      as.factor() |> forcats::fct_relevel(rev),
    # create two dimensional variables from metric ones
    across(
      c(
        revs_n, senti_mean, senti_vari, wv_mean,
        wikiprizes_pre, books_dnb_prev
      ),
      ~ case_when(
        is.na(.x) ~ "none",
        .x < mean(.x, na.rm = TRUE)
        ~ sprintf("< mean (%.1f)", mean(.x, na.rm = TRUE)),
        .x > mean(.x, na.rm = TRUE)
        ~ sprintf("> mean (%.1f)", mean(.x, na.rm = TRUE))
      ) |>
        as.factor(),
      .names = "{.col}_cat"
    ),
    across(senti_mean_cat:senti_vari_cat, relevel, "none")
  )


# Save dataset
saveRDS(nominees_rec, file = "../data/nominees_rec.RDS")

# Export to Stata
write_dta(nominees_rec, path = "../data/nominees_rec.dta", version = 15)

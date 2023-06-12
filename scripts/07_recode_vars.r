nominees <- readRDS("../data/nominees.RDS")

nominees_rec <- nominees |>
  mutate(
    # dichotome variables from metric ones: split by median; before NAs -> 0
    across(
      c(revs_n, wikiprizes_pre, books_dnb_prev, pub_reputation_mean, wv_mean),
      ~ ifelse(is.na(.x), 0, .x)
    ),
    across(
      c(
        revs_n, wikiprizes_pre, books_dnb_prev, pub_reputation_mean, wv_mean,
        age_nom
      ),
      ~ case_when(
        .x <= median(.x, na.rm = TRUE)
        ~ sprintf("<= median (%.1f)", median(.x, na.rm = TRUE)),
        .x > median(.x, na.rm = TRUE)
        ~ sprintf("> median (%.1f)", median(.x, na.rm = TRUE))
      ) |>
        as.factor(),
      .names = "{.col}_cat"
    ),
    # sentiment variation 0 if no variation but valid sentiment values
    senti_vari = ifelse(!is.na(senti_mean) & is.na(senti_vari), 0, senti_vari),
    senti_qual_cat = case_when(
      is.na(senti_mean) ~ "none",
      senti_mean <= mean(senti_mean, na.rm = TRUE) &
        senti_vari <= mean(senti_vari, na.rm = TRUE) ~ "clearly low",
      senti_mean <= mean(senti_mean, na.rm = TRUE) &
        senti_vari > mean(senti_vari, na.rm = TRUE) ~ "disputed low",
      senti_mean > mean(senti_mean, na.rm = TRUE) &
        senti_vari > mean(senti_vari, na.rm = TRUE) ~ "disputed high",
      senti_mean > mean(senti_mean, na.rm = TRUE) &
        senti_vari <= mean(senti_vari, na.rm = TRUE) ~ "clearly high"
    ) |>
      as.factor() |>
      relevel(ref = "clearly low"),
    # nonfiction
    nonfiction = case_when(
      poetry == "Y" ~ TRUE,
      str_detect(tpcs, "Lyrik") ~ TRUE,
      str_detect(topics_orig, "[Bb]iografie|Lyrik") ~ TRUE,
      .default = FALSE
    ),
    # Recode language
    language_nongerman = ifelse(str_detect(language, "foreign"), TRUE, FALSE),
    # create Zeitgeist variables
    metoo = ifelse(ynom < 2017, "Before #metoo", "After #metoo") |>
      as.factor() |> forcats::fct_relevel(rev),
    syria = ifelse(ynom < 2015, "Before 2015", "After 2015") |>
      as.factor() |> forcats::fct_relevel(rev),
    # jury gender share as groups
    jury_group = case_when(
      jury_fem < 0.5 ~ "more male",
      jury_fem == 0.5 ~ "even",
      jury_fem > 0.5 ~ "more female"
    ) |>
      as.factor() |>
      relevel(ref = "even"),
  )

nominees_rec <- fastDummies::dummy_cols(nominees_rec, select_columns = "prize")


# Save dataset
saveRDS(nominees_rec, file = "../data/nominees_rec.RDS")

# Export to Stata
write_dta(nominees_rec, path = "../data/nominees_rec.dta", version = 15)


nominees <- readRDS("../data/nominees.RDS")

nominees_rec <- nominees |>
  mutate(
    # dichotome variables from metric ones: split by median; before NAs -> 0
    across(
      c(revs_n, wikiprizes_pre, books_dnb_prev, pub_reputation_mean, wv_mean),
      ~ ifelse(is.na(.x), 0, .x)
    ),
    across(
      c(revs_n, wikiprizes_pre, books_dnb_prev, pub_reputation_mean, wv_mean),
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
      is.na(senti_mean) ~ 0,
      senti_mean <= mean(senti_mean, na.rm = TRUE) &
        senti_vari <= mean(senti_vari, na.rm = TRUE) ~ 1,
      senti_mean <= mean(senti_mean, na.rm = TRUE) &
        senti_vari > mean(senti_vari, na.rm = TRUE) ~ 2,
      senti_mean > mean(senti_mean, na.rm = TRUE) &
        senti_vari > mean(senti_vari, na.rm = TRUE) ~ 3,
      senti_mean > mean(senti_mean, na.rm = TRUE) &
        senti_vari <= mean(senti_vari, na.rm = TRUE) ~ 4
    ) |>
      factor(labels = c(
        "none", "clearly low", "disputed low", "disputed high", "clearly high"
      )) |>
      relevel(ref = "clearly low"),
    # jury gender share as groups
    jury_group = case_when(
      jury_fem < 0.5 ~ "male",
      jury_fem == 0.5 ~ "even",
      jury_fem > 0.5 ~ "female"
    ) |> factor(level = c("male", "even", "female")),
    # gen homophily variable
    homophily = case_when(
      jury_group == "male" & !female ~ TRUE,
      jury_group == "female" & female ~ TRUE,
      .default ~ FALSE
    ),
    # Recode language
    language_german = case_when(
      language == "foreign" ~ FALSE,
      language == "foreign+" ~ FALSE,
      .default ~ TRUE
    ),
    # nonfiction
    nonfiction = case_when(
      poetry == "Y" ~ TRUE,
      str_detect(tpcs, "Lyrik") ~ TRUE,
      str_detect(topics_orig, "Biografie|Autobiografie|Lyrik") ~ TRUE,
      .default ~ FALSE
    ),
    metoo = ifelse(ynom < 2017, "Before #metoo", "After #metoo") |>
      as.factor() |> forcats::fct_relevel(rev),
    syria = ifelse(ynom < 2015, "Before 2015", "After 2015") |>
      as.factor() |> forcats::fct_relevel(rev),
    # # reviewers gender share as groups
    # revs_group = case_when(
    #   revs_fem < 0.5 ~ "male",
    #   revs_fem == 0.5 ~ "even",
    #   revs_fem > 0.5 ~ "female"
    # ) |> factor(level = c("male", "even", "female")),
    # prevprize = case_when(
    #   wikiprizes_pre == 0 ~ "none",
    #   wikiprizes_pre > 0 & wikiprizes_pre < 8 ~ "medium",
    #   wikiprizes_pre >= 8 ~ "many"
    # ) |> factor(level = c("none", "medium", "many")),
    # # prevbooks as groups
    # prevbooks = case_when(
    #   books_dnb_prev == 0 ~ "none",
    #   books_dnb_prev > 0 & books_dnb_prev < 11 ~ "medium",
    #   books_dnb_prev > 10 ~ "many"
    # ) |> factor(level = c("none", "medium", "many")),
    # # age as groups
    # age_group = case_when(
    #   age_nom < 43 ~ "18-42",
    #   between(age_nom, 43, 66) ~ "43-66",
    #   age_nom > 66 ~ "67-92"
    # ) |> factor(level = c("18-42", "43-66", "67-92")),
    # create Zeitgeist variables
  )


# Save dataset
saveRDS(nominees_rec, file = "../data/nominees_rec.RDS")

# Export to Stata
write_dta(nominees_rec, path = "../data/nominees_rec.dta", version = 15)

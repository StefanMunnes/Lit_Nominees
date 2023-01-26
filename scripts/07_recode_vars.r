nominees <- readRDS("../data/nominees.RDS")

nominees <- nominees |>
  # Recode language
  mutate(
    language_german = case_when(
      language == "foreign" ~ FALSE,
      language == "foreign+" ~ FALSE,
      TRUE ~ TRUE
    ),
    # age as groups
    age_group = case_when(
      age_nom >= 18 & age_nom <= 42 ~ "18-42",
      age_nom > 42 & age_nom <= 66 ~ "43-66",
      age_nom > 66 & age_nom <= 93 ~ "67-92"
    ) |> factor(level = c("18-42", "43-66", "67-92")),
    # jury gender share as groups
    jury_group = case_when(
      jury_fem >= 0.0 & jury_fem <= 0.49 ~ "male",
      jury_fem == 0.5 ~ "even",
      jury_fem >= 0.51 & jury_fem <= 1.0 ~ "female"
    ) |> factor(level = c("male", "even", "female")),
    #  reviewers gender share as groups
    revs_group = case_when(
      revs_fem >= 0.0 & revs_fem <= 0.49 ~ "male",
      revs_fem == 0.5 ~ "even",
      revs_fem >= 0.51 & revs_fem <= 1.0 ~ "female"
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
      books_dnb_prev >= 11 ~ "many"
    ) |> factor(level = c("none", "medium", "many")),
    # sentiment variation as
    senti_vari = ifelse(!is.na(senti_mean) & is.na(senti_vari), 0, senti_vari),
    # gen homophily variable
    homophily = case_when(
      jury_group == "male" & female == FALSE ~ 1,
      jury_group == "female" & female == TRUE ~ 1,
      TRUE ~ 0
    ),
    # wikiviews
    wv_mean = ifelse(is.na(wv_mean) | wv_mean == 0, 1, wv_mean),
    # Calculate wv_mean_log new
    wv_mean_log = log(wv_mean),
    wv_mean = ifelse(wv_mean >= 100, 100, wv_mean),
    nonfiction = case_when(
      poetry == "Y" ~ TRUE,
      str_detect(tpcs, "Lyrik") ~ TRUE,
      str_detect(topics_orig, "Biografie") ~ TRUE,
      str_detect(topics_orig, "Autobiografie") ~ TRUE,
      str_detect(topics_orig, "Lyrik") ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  select(!c(group_jury, group_revs))

# Save dataset
saveRDS(nominees, file = "../data/nominees.RDS")

# Export to Stata
write_dta(nominees, path = "../data/nominees.dta", version = 15)

nominees <- readRDS("../data/nominees.RDS")

nominees <- nominees |>
#Recode language
  mutate(
    language_german = case_when(
      language == "foreign" ~ FALSE,
      language == "foreign+" ~ FALSE,
      TRUE ~ TRUE
      ),
# Recode age as groups
# Create categories
    age_group = case_when(
      age_nom >= 18 & age_nom <= 42 ~ "18-42",
      age_nom > 42 & age_nom <= 66 ~ "43-66",
      age_nom > 66 & age_nom <= 93 ~ "67-92"
      ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("18-42", "43-66", "67-92")
      ),
  # Recode Jury as groups
    group_jury = case_when(
      jury_fem >= 0.0 & jury_fem <= 0.49 ~ "male",
      jury_fem == 0.5 ~ "even",
      jury_fem >= 0.51 & jury_fem <= 1.0 ~ "female"
      ),
    group_jury = factor(
      group_jury,
      level = c("male", "even", "female")
      ),
  #  Recode Reviewer as groups
    group_revs = case_when(
      revs_fem >= 0.0 & revs_fem <= 0.49 ~ "male",
      revs_fem == 0.5  ~ "even",
      revs_fem >= 0.51 & revs_fem <= 1.0  ~ "female"
      ),
    group_revs = factor(
      group_revs,
      level = c("male", "even", "female")
      ),
    senti_vari = case_when(
      !is.na(senti_mean) & is.na(senti_vari) ~ 0,
      TRUE ~ senti_vari
      ),
  # gen homophily variable 
    homophily = case_when(
      group_jury == "male" & female == FALSE ~ 1,
      group_jury == "female" & female == TRUE ~ 1,
      TRUE ~ 0
      ),
    prevprize = case_when(
      wikiprizes_pre == 0  ~ "none",
      wikiprizes_pre > 0 & wikiprizes_pre < 8 ~ "medium",
      wikiprizes_pre >= 8 ~ "many"
      ),
    prevprize = factor(
      prevprize,
      level = c("none", "medium", "many")
      ),
    prevbooks = case_when(
      books_dnb_prev == 0 ~ "none",
      books_dnb_prev > 0 & books_dnb_prev < 11 ~ "medium",
      books_dnb_prev >= 11 ~ "many"
      ),
    prevbooks = factor(
      prevbooks,
      level = c("none", "medium", "many")
      ),
    # Recode wikiviews
    wv_mean = case_when(
      is.na(wv_mean) ~ 1,
      wv_mean == 0 ~ 1,
      TRUE ~ wv_mean
      ),
    # Calculate wv_mean_log new
    wv_mean_log = log(wv_mean),
    wv_mean = case_when(
    wv_mean >= 100 ~ 100,
    TRUE ~ wv_mean
    ))

# Save dataset
saveRDS(nominees, file = "../data/nominees.RDS")

# Export to Stata
write_dta(nominees, path = "../data/nominees.dta", version = 15)

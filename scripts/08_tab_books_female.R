library(gtsummary)
library(gt)
library(flextable)


# -- 1. Load data --
nominees <- readRDS("../data/nominees_rec.RDS") |>
  distinct(title, .keep_all = TRUE)

# -- 2. Summary statistics --
# By Gender
table_female <- nominees |>
  # select vars to describe
  select(
    female,
    senti_mean,
    revs_n,
    books_dnb_prev, nom_prize_n, wikiprizes_pre, pub_reputation_mean, wv_mean,
    language, age_nom
  ) |>
  mutate(language = case_when(
    language == "foreign" ~ TRUE,
    language == "foreign+" ~ TRUE,
    TRUE ~ FALSE
  )) |>
  tbl_summary(
    # group by female
    by = female,
    # Don't display NA
    missing = "no",
    # Define Variable labels
    label = list(
      senti_mean ~ "Review sentiment",
      # senti_vari ~ "Variance in review sentiment",
      revs_n ~ "# Reviews",
      books_dnb_prev ~ "# Previously published books",
      nom_prize_n ~ "# Previous nominations",
      wikiprizes_pre ~ "# Nominations for other prizes",
      pub_reputation_mean ~ "Publisher status",
      wv_mean ~ "Wikipedia views",
      language ~ "Nongerman background",
      age_nom ~ "Age at nomination"
    ),
    # 2 digits for continuous and categorical variables
    digits = list(
      all_continuous() ~ 2,
      all_categorical() ~ 2
    ),
    # Recode numeric variables with < 10 levels(defaults to type categorical)
    type = list(
      nom_prize_n ~ "continuous"
    ),
    # Calculate Mean, SD and n
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}%",
      all_dichotomous() ~ "{p}%"
    )
  ) |>
  # Add t.test statistic
  add_p(list(all_continuous() ~ "t.test")) |>
  # Add siginficance stars
  add_significance_stars() |>
  # Modify footnote
  modify_footnote(all_stat_cols() ~ "Mean (standard deviation in parentheses) \n Percentage for categorical variables", # nolint
    p.value = "Paired t-test (Chi-squared test for categorical variables) \n *p<0.05; **p<0.01; ***p<0.001" # nolint
  ) |>
  # Modify header
  modify_header(
    label = "**Variable**",
    stat_1 = "**Books by Men (N = {n})**",
    stat_2 = "**Books by Women (N = {n})**"
  ) |>
  # modify_spanning_header(c("stat_1", "stat_2") ~ "**Female**") |>
  modify_caption("**Summary Statistics by Gender**")

# export to word
table_female |>
  as_flex_table() |>
  save_as_docx(path = "../output/tables/tab_books_female.docx")

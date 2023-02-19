pacman::p_load("modelsummary", "flextable", "officer")

sect_properties <- prop_section(
  page_size = page_size(
    orient = "landscape",
  ),
  page_mar(top = NULL),
  type = "continuous"
)

modelsummary(models_log[c(1, 2, 3, 7, 8)],
  vcov = ~prize, # clustered SE's
  stars = c("*" = .05, "**" = .01, "***" = 0.001),
  coef_map = coef_labs,
  gof_map = "nobs", # only display N
  estimate = "{estimate} ({std.error}) {stars}",
  statistic = NULL,
  shape = term ~ model,
  notes = list(
    "* = .05, ** = .01, *** = 0.001",
    "Standard errors in parentheses. All models include control for debut prize"
  ),
  title = "Logistic Probability Model",
  output = "flextable"
) |>
  autofit() |>
  fontsize(size = 9, part = "all") |>
  padding(padding = 0, part = "all") |>
  save_as_docx(
    path = "../output/tables/tab_models_log.docx",
    pr_section = sect_properties
  )

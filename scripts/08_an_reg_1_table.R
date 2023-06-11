sect_properties <- prop_section(
  page_size = page_size(
    orient = "landscape",
  ),
  # page_mar(top = NULL),
  type = "continuous"
)

modelsummary(margins_log,
  # vcov = ~prize, # clustered SE's
  # stars = c("*" = .05, "**" = .01, "***" = 0.001),
  coef_map = coef_labs,
  gof_map = "nobs", # only display N
  estimate = "{estimate}",
  statistic = NULL,
  shape = term ~ model,
  title = "Average Marginal Effects",
  output = "flextable"
) |>
  autofit() |>
  fontsize(size = 8, part = "all") |>
  padding(padding = 1, part = "all") |>
  save_as_docx(
    path = "../output/tables/tab_models_log_qual.docx",
    pr_section = sect_properties
  )

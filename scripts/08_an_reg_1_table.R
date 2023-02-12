sect_properties <- prop_section(
  page_size = page_size(
    orient = "landscape",
  ),
  page_mar(top = NULL),
  type = "continuous"
)

modelsummary(models_log,
  vcov = ~prize, # clustered SE's
  stars = c("*" = .05, "**" = .01, "***" = 0.001),
  coef_map = coef_labs,
  gof_map = "nobs", # only display N
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  shape = term ~ model,
  notes = list(
    "* = .05, ** = .01, *** = 0.001",
    "Standard errors in parentheses. All models include control for debut prize"
  ),
  title = "Linear Probability Model",
  output = "flextable"
) |>
  # width(j = 2:7, width = 1) |>
  # width(j = 1, width = 2.5) |>
  autofit() |>
  fontsize(size = 9, part = "all") |>
  padding(padding = 0, part = "all") |>
  save_as_docx(
    path = "../output/tables/table_log.docx",
    pr_section = sect_properties
  )

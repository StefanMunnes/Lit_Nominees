# add papge characteristics
sect_properties <- prop_section(
  page_size = page_size(orient = "landscape"),
  type = "continuous"
)


# 1. table: without standard error
modelsummary(margins_log[1:5],
  coef_map = coef_labs,
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
    path = "../output/tables/tab_log_margins.docx",
    pr_section = sect_properties
  )


# 2. table: with confidence intervall
modelsummary(margins_log,
  stars = c("*" = .05, "**" = .01, "***" = 0.001),
  coef_map = coef_labs,
  estimate = "{estimate}\n({std.error}){stars}",
  statistic = NULL,
  shape = term ~ model,
  title = "Average Marginal Effects",
  output = "flextable"
) |>
  autofit() |>
  fontsize(size = 8, part = "all") |>
  padding(padding = 1, part = "all") |>
  save_as_docx(
    path = "../output/tables/tab_log_margins_fe_se.docx",
    pr_section = sect_properties
  )



# 3. table: with goodness of fit statistics to add manualy to margin tables
gm <- modelsummary::gof_map
gm$omit <- "FALSE"
gm$fmt[gm$raw == "r2.tjur"] <- 3

modelsummary(models_log,
  coef_omit = -1,
  gof_omit = "Lik|RMSE",
  gof_map = gm,
  shape = term ~ model,
  output = "flextable"
) |>
  autofit() |>
  fontsize(size = 8, part = "all") |>
  save_as_docx(
    path = "../output/tables/tab_log_margins_gof.docx",
    pr_section = sect_properties
  )

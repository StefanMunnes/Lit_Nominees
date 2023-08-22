# ---- 1. create order and labels for variables (add median manual) ----
vars <- c(
  "senti_mean", "senti_vari", "revs_n",
  "books_dnb_prev", "wikiprizes_pre",
  "pub_reputation_mean", "wv_mean",
  "age_nom"
)

# sapply(vars, function(x) median(nominees_an[[x]], na.rm = TRUE))

vars_lab <- c(
  "Mean sentiment (6)", "Variance sentiment (0.67)", "# reviews (4)",
  "# previous books (5)", "# previous prizes (4)",
  "Publisher reputation (4.4)", "Wikipedia views (8.6)",
  "Age (43)"
)


# ---- 2. load nominees and long format data with variables and values ----
nominees_cat <- readRDS("../data/nominees_rec.RDS") |>
  select(all_of(vars)) |>
  pivot_longer(everything(),
    names_to = "variables", values_to = "values",
    values_drop_na = TRUE
  ) |>
  mutate(variables = factor(variables, levels = vars, labels = vars_lab))


# ---- 3. draw violin plot, splitted by variable and add median
plot_cat_vars <- ggplot(nominees_cat, aes(y = variables, x = values)) +
  geom_violin(draw_quantiles = 0.5) +
  facet_wrap(~variables, scales = "free", ncol = 1) +
  theme_bw() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    strip.text = element_text(size = 20),
    strip.background = element_blank()
  )


ggsave(
  file = "../output/graphs/plot_cat_vars.png",
  plot = plot_cat_vars, dpi = 600, scale = 1.2, height = 12, width = 11
)
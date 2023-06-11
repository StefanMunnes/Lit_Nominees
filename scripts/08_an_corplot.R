library(corrplot)


data_cor <- get_all_vars(model_formulars[[5]], nominees_an) |>
  mutate(across(where(is.factor), as.numeric))

cor_tab <- cor(data_cor)

corrplot(cor_tab,
  type = "upper", order = "original",
  tl.col = "black", tl.srt = 45
)

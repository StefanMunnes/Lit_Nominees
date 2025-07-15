files <- list.files(
  "../data/publisherstatus/",
  pattern = "^Verlagsbewertungen_.+\\.xlsx$",
  full.names = TRUE
)


list <- lapply(files, function(file) {
  data <- readxl::read_xlsx(path = file)
  data <- data[, c(1, 2, 3)]

  names(data) <- c(
    "publisher",
    "reputation",
    "prominence"
  )

  data$coder <- stringr::str_replace(
    file,
    "Verlagsbewertungen_(.+).xlsx",
    "\\1"
  )

  return(data)
})

data <- bind_rows(list) |>
  filter(coder != "Svenja") |>
  pivot_longer(cols = c(reputation, prominence), names_to = "metric") |>
  pivot_wider(
    names_from = coder,
    values_from = c(value),
    names_prefix = "coder_"
  ) |>
  rowwise() |>
  mutate(
    n_coder = sum(!is.na(c_across(starts_with("coder_")))),
    mean = mean(c_across(starts_with("coder_")), na.rm = TRUE),
    sd = sd(c_across(starts_with("coder_")), na.rm = TRUE)
  ) |>
  select(
    publisher,
    metric,
    n_coder,
    mean,
    sd
  )

cor(
  data$mean[data$metric == "reputation"],
  data$mean[data$metric == "prominence"],
  method = "pearson",
  use = "complete.obs"
)

# Correlation: 0.7917669

write.csv(
  data,
  file = "../data/publisherstatus_coded.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

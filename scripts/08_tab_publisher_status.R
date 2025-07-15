data_publisher <- read.csv("../data/publisherstatus_coded.csv")

# load data and category of by-varialbe to all observations
data_publisher_full <- readRDS("../data/nominees_rec.RDS") |>
  filter(row_number() == 1, .by = match_id) |> # keep just on book level
  group_by(publisher) |>
  mutate(n_books = n(), senti_pub_mean = mean(senti_mean, na.rm = TRUE)) |>
  filter(row_number() == 1) |>
  full_join(data_publisher, by = "publisher") |>
  filter(!is.na(publisher))

# following publisher were rated but without books:
# Faber und Faber, Rogner und Bernhard Verlag, Rotbuch Verlag

cor(
  data_publisher_full$senti_mean[data_publisher_full$metric == "reputation"],
  data_publisher_full$mean[data_publisher_full$metric == "reputation"],
  use = "complete.obs"
)

# 0.0949303

publisher_rating_missing <- data_publisher_full |>
  filter(is.na(n_coder) | n_coder == 0, !is.na(n_books)) |> # before: is.na(n_coder)
  distinct(publisher, .keep_all = TRUE) |>
  select(publisher, n_books) |>
  arrange(publisher)

tab_pubslisher <- data_publisher_all |>
  select(
    publisher,
    n_books,
    n_coder,
    metric,
    mean,
    sd,
    senti_pub_mean
  ) |>
  arrange(publisher, metric) |>
  filter(n_coder > 0) |> # before: !is.na(n_coder)
  filter(metric == "reputation") |>
  select(!metric) |>
  flextable() |>
  merge_v(
    j = ~publisher,
    target = c("publisher", "n_coder", "n_books", "senti_pub_mean")
  ) |>
  set_header_labels(
    publisher = "Publisher",
    n_books = "# Books",
    n_coder = "# Ratings",
    # metric = "Metric",
    mean = "Mean",
    sd = "SD",
    senti_pub_mean = "Ø Rating of Books"
  ) |>
  colformat_num(na_str = ".") |>
  colformat_char(na_str = ".") |>
  colformat_double(digits = 2, na_str = ".") |>
  set_table_properties(width = 1, layout = "autofit") |>
  hline(part = "body")

doc <- read_docx()
doc <- body_add_flextable(doc, value = tab_pubslisher)
doc <- body_add_par(doc, " ")
doc <- body_add_par(
  doc,
  "Following publishers (# Books) have no ratings from human coders: "
)
doc <- body_add_par(
  doc,
  paste0(
    publisher_rating_missing$publisher,
    " (",
    publisher_rating_missing$n_books,
    ")",
    collapse = ", "
  )
)

print(doc, target = "../output/tables/tab_publisher_status.docx")

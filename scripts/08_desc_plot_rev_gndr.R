
nominees_rec <- readRDS("../data/nominees_rec.RDS") |> select(url_book)
reviews_gndr <- readRDS("../data/reviews_gndr.RDS")

reviews_gndr_nom <- left_join(nominees_rec, reviews_gndr, by = "url_book")



# ---- 1. Proportion of female and male reviewers over time ----
rev_gndr_year <- reviews_gndr_nom |>
  mutate(year = year(date)) |>
  filter(!is.na(rev_gndr)) |>
  count(rev_gndr, year, sort = TRUE) |>
  mutate(freq = n / sum(n), .by = year)

plot_rev_gndr_year <- rev_gndr_year |>
  ggplot(aes(y = freq, x = year, color = rev_gndr)) +
  geom_line(linewidth = 1.5) +
  geom_hline(yintercept = 0.5) +
  scale_color_discrete(breaks = c("M", "F")) +
  labs(color = "Gender", x = "", y = "") +
  theme_bw()

ggsave(
  file = "../output/graphs/plot_rev_gndr_year.png",
  plot = plot_rev_gndr_year, dpi = 600, scale = 0.7, height = 5, width = 11
)



# ---- 2. overview of proportion of # of reviews of top reviewers ----
rev_gndr_name <- reviews_gndr_nom |>
  separate_rows(rev_name, sep = ";") |>
  count(rev_name, rev_gndr, sort = TRUE) |>
  mutate(
    cum = cumsum(n),
    cumprop = cum / sum(n)
  ) |>
  arrange(cum) |>
  mutate(
    rev_name = ifelse(is.na(rev_name), "Unknown", rev_name),
    rev_name = factor(rev_name, levels = unique(rev_name))
  )

## get descriptives for reviewers and gender
sum(rev_gndr_name$n) # # of reviews: 2949
nrow(rev_gndr_name) # # of unique reviewer: 501
(tab  <- table(rev_gndr_name$rev_gndr, useNA = "always"))
prop.table(tab) # F: 198 (40%) M: 302 (60%)

data_top_50 <- filter(rev_gndr_name, cumprop < 0.5)
# 39 (7.78%) of reviewers wrote half (1475) of all reviews (2949)
sum(data_top_50$n) # 1475
length(data_top_50$n) # 39
table(data_top_50$rev_gndr) # F: 20, M: 38, NA: 1

# Number of reviews from the most important reviewers F: 538, M: 894
summarize(data_top_50, n = sum(n), .by = rev_gndr)


## create plot of most important reviewers
plot_revs_gndr_top <- filter(rev_gndr_name, cumprop < 0.5) |>
  ggplot(aes(x = rev_name)) +
  geom_bar(
    aes(y = n, fill = rev_gndr),
    stat = "identity"
  ) +
  geom_line(aes(y = cum / (sum(rev_gndr_name$n) / 125)), group = 1) +
  coord_flip() +
  labs(fill = "Gender", x = "") +
  ylim(0, 125) +
  scale_y_continuous(
    name = "Observations",
    breaks = seq(0, 125, 25),
    sec.axis = sec_axis(
      trans = ~.,
      name = "Cumulative relative frequency",
      labels = function(b) {
        paste0(round((b / 1.25), 0), "%")
      }
    )
  ) +
  theme_bw()

ggsave(
  file = "../output/graphs/plot_revs_gndr_top.png",
  plot = plot_revs_gndr_top, dpi = 600, scale = 0.9, height = 9, width = 11
)

reviews_gndr <- readRDS("../data/reviews_gndr.RDS")


# ---- 1. overview of proportion of # of reviews of top reviewers ----
rev_gndr_name <- reviews_gndr |>
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

# # of reviews: 7409
# # of unique reviewer: 1131
# # of reviews written by F and M
prop.table(table(rev_gndr_name$rev_gndr)) # F: 2512 (34%) M: 4831 (66%)

# 59 (5.22%) of reviewers wrote half (3695) of all reviews (7409)
sum(rev_gndr_name$n[rev_gndr_name$cumprop < 0.5]) # 3695
length(rev_gndr_name$n[rev_gndr_name$cumprop < 0.5]) # 59
table(rev_gndr_name$rev_gndr[rev_gndr_name$cumprop < 0.5]) # F: 20, M: 38, NA: 1


plot_revs_gndr_top <- filter(rev_gndr_name, cumprop < 0.5) |>
  ggplot(aes(x = rev_name)) +
  geom_bar(
    aes(y = n, fill = rev_gndr),
    stat = "identity"
  ) +
  geom_line(aes(y = cum / (sum(rev_gndr_name$n) / 200)), group = 1) +
  coord_flip() +
  labs(
    title = "Number of reviews from the most important reviewers",
    fill = "Gender", x = ""
  ) +
  ylim(0, 200) +
  scale_y_continuous(
    name = "Observations",
    breaks = seq(0, 175, 25),
    sec.axis = sec_axis(
      trans = ~.,
      name = "Cumulative frequency",
      labels = function(b) {
        paste0(round((b / 2), 0), "%")
      }
    )
  ) +
  theme_bw()

ggsave(
  file = "../output/graphs/plot_revs_gndr_top.png",
  plot = plot_revs_gndr_top, dpi = 600, scale = 0.9, height = 12, width = 11
)


# ---- 2. Proportion of female and male reviewers over time ----
rev_gndr_year <- reviews_gndr |>
  mutate(year = year(date)) |>
  filter(!is.na(rev_gndr)) |>
  count(rev_gndr, year, sort = TRUE) |>
  mutate(freq = n / sum(n), .by = year)

plot_rev_gndr_year <- rev_gndr_year |>
  ggplot(aes(y = freq, x = year, color = rev_gndr)) +
  geom_line(linewidth = 1.5) +
  geom_hline(yintercept = 0.5) +
  scale_color_discrete(breaks = c("M", "F")) +
  labs(
    title = "Proportion of female and male reviewers over time",
    color = "Gender", x = "", y = ""
  ) +
  theme_bw()

ggsave(
  file = "../output/graphs/plot_rev_gndr_year.png",
  plot = plot_rev_gndr_year, dpi = 600, scale = 0.7, height = 12, width = 11
)

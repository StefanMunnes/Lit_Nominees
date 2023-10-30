nominees_rec <- readRDS("../data/nominees_rec.RDS") |>
  select(url_book) |>
  distinct() |>
  na.omit()
reviews_gndr <- readRDS("../data/reviews_gndr.RDS")

reviews_gndr_nom <- left_join(nominees_rec, reviews_gndr, by = "url_book")



# ---- 1. Proportion of female and male reviewers over time ----
rev_gndr_year <- reviews_gndr_nom |>
  mutate(year = year(date)) |>
  filter(!is.na(rev_gndr)) |>
  count(rev_gndr, year, sort = TRUE) |>
  mutate(prop = n / sum(n), .by = year)


rev_gndr_year2 <- reviews_gndr_nom |>
  mutate(year = year(date)) |>
  distinct(year, rev_name, rev_gndr) |>
  na.omit() |>
  count(rev_gndr, year, sort = TRUE) |>
  mutate(prop = n / sum(n), .by = year) |>
  bind_rows(rev_gndr_year, .id = "Object") |>
  mutate(Object = case_when(Object == 1 ~ "Reviewer", Object == 2 ~ "Reviews"))


plot_rev_gndr_year <- rev_gndr_year2 |>
  ggplot(aes(y = prop, x = year, color = rev_gndr, linetype = Object)) +
  geom_line(linewidth = 1.5) +
  geom_hline(yintercept = 0.5) +
  scale_color_discrete(breaks = c("M", "F")) +
  labs(color = "Gender", x = "", y = "") +
  theme_bw()


plot_rev_gndr_year <- rev_gndr_year |>
  ggplot(aes(y = prop, x = year, color = rev_gndr)) +
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
sum(rev_gndr_name$n) # # of reviews: 2473
nrow(rev_gndr_name) # # of unique reviewer: 501
(tab <- table(rev_gndr_name$rev_gndr, useNA = "always"))
prop.table(tab) # F: 198 (39.5%) M: 302 (60.3%)

summarize(rev_gndr_name, sum = sum(n), .by = rev_gndr) |>
  mutate(prop = sum / sum(sum)) # F: 945 (38.2) M: 1827 (61.7)


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

data_top_50 <- filter(rev_gndr_name, cumprop < 0.5)
# 39 (7.78%) of reviewers wrote half (1226) of all reviews (2473)
sum(data_top_50$n) # 1226
length(data_top_50$n) # 40
table(data_top_50$rev_gndr) # F: 15, M: 25

# # of reviews from the most important reviewers F: 458, M: 768
summarize(data_top_50, n = sum(n), .by = rev_gndr)

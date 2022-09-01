
# 1. load functions necessary for all later scripts
source("03_wp_prizes_all_01_func.R", encoding = "UTF-8")

# 2. scrape htmls of all german speaking prizes from wikipedia
source("03_wp_prizes_all_02_htmls.R", encoding = "UTF-8")

# 3. extracting winners (and nominees) wiki-urls from raw htmls
source("03_wp_prizes_all_03_extr.R", encoding = "UTF-8")

# 4. clean list of authors urls and merge with given list of nominees wiki urls
source("03_wp_prizes_all_04_clean.R", encoding = "UTF-8")

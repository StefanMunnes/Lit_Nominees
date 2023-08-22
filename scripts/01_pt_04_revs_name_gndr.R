# ---- 1. load and prepare list of forenames and genders ----
# https://www.heise.de/ct/ftp/07/17/182/ ---

# namezip = "data/oth/names_heise.zip"
# if (!file.exists(namezip)) {
#  download.file("ftp://ftp.heise.de/pub/ct/listings/0717-182.zip",
#                destfile = namezip)
#  unzip(namezip, exdir = "../data/names_heise")
# }

names.raw <- read.delim("../data/names_heise/nam_dict.txt",
  header = F, skip = 362, fileEncoding = "UTF-8"
)
names.raw$V1 <- substr(names.raw$V1, 1, 86)
names.raw$gender <- trimws(substr(names.raw$V1, 1, 3))
names.raw$name <- trimws(substr(names.raw$V1, 4, 28)) # %>% stri_trans_general("latin-ascii")
names.raw$laute <- substr(names.raw$V1, 29, 30)
names.raw$nums <- gsub(" ", "", substr(names.raw$V1, 31, 86))
names.raw$freq <- nchar(names.raw$nums)

# calculate digitsum (Quersumme) to filter common names
sum <- sapply(names.raw$nums, function(x) {
  gsub("[ABCD]", "9", x) %>%
    as.character() %>%
    strsplit(split = "") %>%
    unlist() %>%
    as.numeric() %>%
    sum()
})

names.raw <- cbind(names.raw, sum)
rm(sum)

# Namen mit Sonderzeichen (<...>) Umlauten, daher doppelt (-), sowie "äquvalente Namen" löschen
names.short <- names.raw[(!grepl("<", names.raw$name) &
  !grepl("-", names.raw$laute) &
  !grepl("=", names.raw$gender)), ]

# nur Namen behalten, die wenigstens in 2 Ländern vorkommen oder eine summierte Relevanz von 4 haben
names.short <- names.short[(names.short$freq > 1 | names.short$sum > 3), 2:3]

# Namen mit "+" bereinigen, entweder Bindestrich oder ohne Leerzeichen
names.dash <- names.short[grepl("\\+", names.short$name), ]
names.dash$name <- gsub("\\+", "-", names.dash$name)

names.short <- names.short %>%
  separate(name, into = c("one", "two"), sep = "\\+") %>%
  mutate(name = ifelse(is.na(two), one, paste0(one, tolower(two)))) %>%
  bind_rows(names.dash) %>%
  select(name, gender)

# doppelte Namen mit unterschiedlichem Geschlecht: zusammenspielen, beide behalten
names.heise <- names.short %>%
  group_by(name) %>%
  mutate(gender = paste0(gender, collapse = "")) %>%
  summarise(gender = first(gender)) %>%
  mutate(gender = case_when(
    str_detect(gender, "(F\\.*M)|(M\\.*F)") ~ "F/M",
    str_detect(gender, "F") ~ "F",
    str_detect(gender, "M") ~ "M",
    TRUE ~ "?"
  )) %>%
  filter(str_length(name) > 2) %>%
  # remove german stopwords (not typical names)
  filter(!str_detect(name, "Bei|Nur|Hat|Wen|Tat|Soo|Ton|Man|Fan|Hang|Gang|Ding|Seit|Alter")) %>%
  # add gender manualy for clear german names
  mutate(gender = case_when(
    str_detect(name, "Juli|Karen|Simone") ~ "F",
    str_detect(name, "Ilja|Gerd") ~ "M",
    TRUE ~ gender
  ))

rm(names.dash, names.raw, names.short)



# ---- 2. get names and gender from reviews ----
books_df <- readRDS("../data/pt_books.RDS")
reviews_df <- readRDS("../data/pt_reviews.RDS")


### prepare and clean reviews (remove author name(s) & "Roman")
reviews_tmp <- full_join(books_df, reviews_df, by = "url_book") %>% # add books_df for author names

  filter(!is.na(text)) %>% # remove empty text

  select(rev_id, authors, text, url_book) %>%
  distinct(rev_id, .keep_all = TRUE) |> # duplicates from books_df (multiple authors)

  mutate(
    row = row_number(),
    reviews = text,
    across(c(authors, text), ~ stri_trans_general(.x, "de-ASCII")),


    # use author name(s) as regex to remove in one step with OR
    name = str_remove_all(authors, "\\s*\\(.*\\)"),
    name = str_replace_all(name, "\\s/\\s", ")|("),
    name = paste0("(", name, ")"),
    forename = str_replace_all(name, "\\(([A-Z][a-z-]+).+?\\)", "\\1"),
    lastname = str_replace_all(name, "\\(.+?([A-Z][a-z-]+)\\)", "\\1"),
    text = str_remove_all(text, paste0(name, ".")),
    text = str_remove_all(text, paste0(lastname, ".")),
    text = str_replace_all(text, "Roman(.[^A-Z])", "\\1"),
    text = str_squish(text),

    # count and define gender by appearance of key word
    gndrf = str_count(text, "Rezensentin"),
    gndrm = str_count(text, "Rezensent[^in]"),
    rev_gndr1 = case_when(
      gndrf >= gndrm & gndrf != 0 ~ "F",
      gndrf < gndrm ~ "M"
    )
  )



# ---- 3. extract reviewer name after "Rezensent/in" ----
regex_revname <- "Rezensent[sien]*\\s[A-Z][A-z-]+\\s(von\\s)*([A-Z]\\.\\s)*[A-Z][A-z-]+(\\s[A-Z][A-z-]+)*"
regex_firsttwo <- "^[A-Z][A-z-]+\\s(von\\s)*([A-Z]\\.\\s)*[A-Z][A-z-]+"

reviews_name1 <- mutate(reviews_tmp,
  rev_name1 = str_extract(text, regex_revname),
  rev_name1 = str_remove(rev_name1, "(Rezensent[sien]*\\s)"),

  # delete authors forename and following if not in first position
  rev_name1 = str_replace(rev_name1, paste0("^(.*)\\s(", forename, ").*"), "\\1")
) %>%
  # group by first two nameparts and take just shortest one (get rid of names with unwanted attachments)
  mutate(rev_name1.2 = str_extract(rev_name1, regex_firsttwo)) %>%
  group_by(rev_name1.2) %>%
  arrange(rev_name1.2) %>%
  mutate(rev_name1 = first(rev_name1)) %>%
  ungroup() %>%
  select(!rev_name1.2)


# count occurrence of names by gender to overwrite wrong single classification in some cases
names.reviewers <- reviews_name1 %>%
  mutate(rev_name1 = str_replace_all(
    rev_name1,
    c(
      "^Peter Kunisch" = "Hans-Peter Kunisch",
      "Isenschmid$" = "Isenschmidt"
    )
  )) %>%
  group_by(rev_name1, rev_gndr1) %>%
  summarise(count = n()) %>%
  group_by(rev_name1) %>%
  arrange(rev_name1, count) %>% # sort to get most common gender classification as last
  summarise(
    gender = last(rev_gndr1),
    count = sum(count)
  ) %>%
  mutate(rev_forename = str_extract(rev_name1, "^[A-z-]+")) %>%
  na.omit()


# add names/gender from reviews to list of names/gender from heise
names <- select(names.reviewers, rev_forename, gender) %>%
  rename(name = rev_forename) %>%
  bind_rows(names.heise) %>%
  mutate(name = stri_trans_general(name, "de-ASCII")) %>%
  distinct(name, .keep_all = T) %>%
  na.omit()




# ---- 4. extract names from reviews by list of reviewer names ----

# check for each reviewer name if part of review text -> to character vector of (row)number
reviewers_list1 <- sapply(
  names.reviewers$rev_name1,
  grep,
  reviews_tmp$text
) %>%
  lapply(function(x) {
    paste(as.character(x), collapse = ";")
  })


# transform list to df with (row)number as observation and join with reviews_name1 df
reviews_name2 <- melt(reviewers_list1, value.name = "row") %>%
  rename(rev_name2 = L1) %>%
  filter(row != "") %>%
  separate_rows(row, sep = ";") %>%
  mutate(row = as.numeric(row)) %>%
  group_by(row) %>%
  summarise(rev_name2 = paste(rev_name2, collapse = ";")) %>%
  full_join(reviews_name1, by = "row")



# ---- 5. extract names by first occurrence of forename from long name list ----
reviewers_list2 <- sapply(reviews_name2$text, function(text) {
  words <- stri_match_all_regex(text, "(?=([A-Z][a-z-]+\\s(von|[A-Z])))")[[1]][, 2]
  words <- str_extract_all(words, "[A-Z][a-z-]+") %>% unlist()
  names.pos <- match(words, names$name)
  names <- words[which(!is.na(names.pos))]
  paste(names, collapse = "|")
})

reviews_name3 <- bind_cols(reviews_name2, reviewers_list2)
names(reviews_name3)[ncol(reviews_name3)] <- "rev_forename3"

regex_lastname <- "\\s(von\\s)*([A-Z]\\.\\s)*[A-Z][A-z-]+(\\s[A-Z][A-z-]+)*"

reviews_name <- reviews_name3 %>%
  mutate(
    rev_forename3 = ifelse(rev_forename3 == "", NA, rev_forename3),

    # gest just first name of extracted forenames
    rev_name3 = str_extract(text, paste0(
      "(", rev_forename3, ")",
      regex_lastname
    )),

    # group by first two nameparts and take just short one (get rid of names with unwanted attachments)
    rev_name3.2 = str_extract(rev_name3, regex_firsttwo),
    rev_name3.2 = str_remove(rev_name3, "(s|')$")
  ) %>%
  group_by(rev_name3.2) %>%
  arrange(rev_name3) %>%
  mutate(rev_name3 = first(rev_name3)) %>%
  ungroup() %>%
  mutate(
    rev_name = ifelse(is.na(rev_name1), rev_name2, rev_name1),
    rev_name = ifelse(is.na(rev_name), rev_name3, rev_name),
    rev_name = ifelse(str_detect(rev_name, ";") & !is.na(rev_name3),
      rev_name3, rev_name # take revname3 if multiple names for revname
    ),
    rev_forename = str_extract(rev_name, "^[A-Z][A-z-]+"),
    same12 = rev_name1 == rev_name2,
    same13 = rev_name1 == rev_name3,
    same23 = rev_name2 == rev_name3
  ) %>%
  select(
    rev_id, text, rev_name, rev_name1, rev_name2, rev_name3,
    rev_forename, rev_forename3, starts_with("same"), rev_gndr1
  )



# ---- 6. add gender of reviewer ----
revs_gndr <- left_join(reviews_name, names, by = c("rev_forename" = "name")) %>%
  rename(rev_gndr2 = gender) %>%
  mutate(
    rev_gndr = ifelse(is.na(rev_gndr1), rev_gndr2, rev_gndr1)
  ) |>
  select(rev_id, rev_name, rev_gndr)


# ---- 7. manually correct and add names and gender ----
revs_gndr <- revs_gndr |>
  mutate(
    rev_name = na_if(rev_name, "Des Vetters Eckfenster`"),
    rev_name = na_if(rev_name, "Michael Hamburgers Dichtung"),
    rev_name = case_when(
      rev_id == 322 ~ "Maidt-Zinke",
      rev_id == 672 ~ "Judih von Sternburg",
      rev_id == 806 ~ "Shou Aziz",
      rev_id == 859 ~ "Ulrich Greiner",
      rev_id == 1820 ~ "Thomas E. Schmidt",
      rev_id == 1898 ~ "Schamma Schahadat",
      rev_id == 2310 ~ "Fritz. J. Raddatz",
      rev_id == 2377 ~ "Rike Felka",
      rev_id == 2601 ~ "Walter van Rossum",
      rev_id == 2757 ~ "Burkhard Müller",
      rev_id == 3304 ~ "Tobis Döring",
      rev_id == 3378 ~ "Milos Vec",
      rev_id == 5089 ~ "Bahman Nirumand",
      rev_id == 6201 ~ "Mithu Sanyal",
      rev_id == 7232 ~ "G. H. Holländer",
      .default = rev_name
    ),
    rev_name = str_replace_all(
      rev_name,
      c(
        "Agnes Huefer" = "Agnes Huefner",
        "Alexander Camman.*" = "Alexander Cammann",
        "Andrea Hanna.*" = "Andrea Hanna Huenniger",
        "Andreas Isenschmidt" = "Andreas Isenschmid",
        "Astri Kaminski" = "Astrid Kaminski",
        "Beatrice Eichmann-L.*" = "Beatrice Eichmann-Leutenegger",
        "Beatrix Langer" = "Beatrix Langner",
        "Burkhardt Mueller" = "Burkhard Mueller",
        "Detlef Krumbach Vater-Sohn-Geschichte" = "Detlef Krumbach",
        "Fabian Hirschmanns Debuetroman" = "Fabian Hirschmann",
        "Fatma Aydemi" = "Fatma Aydemir",
        "Frauke Meyer-Glosau" = "Frauke Meyer-Gosau",
        "Gabrielle Kilert" = "Gabrielle Killert",
        "Guenter Kaindlstorfer" = "Guenter Kaindlsdorfer",
        "Hannah Arendts Eichmann-Buch" = "Hannah Arendt",
        "Hans Peter-Kunisch" = "Hans Peter Kunisch",
        "Holger Noltzes Rezension" = "Holger Noltzes",
        "Johan Hinrich Claussen" = "Johann Hinrich Claussen",
        "Judith von Sternbug" = "Judith von Sternburg",
        "Juerg Altwegg" = "Juergen Altwegg",
        "Juergen Bergers Rezension" = "Juergen Berger",
        "Karl Markus Gauss" = "Karl-Markus Gauss",
        "Katharina Dobler" = "Katharina Doebler",
        "Lutz Hagestedts Rezension" = "Lutz Hagestedts",
        "Manuela Reichert" = "Manuela Reichart",
        "Matthias Schnitzlers Augen" = "Matthias Schnitzler",
        ".+ke Fess.+" = "Meike Fessmann",
        "Michael Kothes" = "Michael Kohtes",
        "Michael Naumann Erzaehlungsband" = "Michael Naumann",
        "Michel Bergs Wohlwollen" = "Michel Berg",
        "Nicole Henneb.+" = "Nicole Henneberg",
        "Niels Werber" = "Niels Weber",
        "Norbert Mappes-Niediek" = "Norbert Mappes-Niedick",
        "Peter Kunsich" = "Peter Kunisch",
        "Philipp Theison" = "Philipp Theisohn",
        "Philip Theisohn" = "Philipp Theisohn",
        "Reinhard J. Brembecks Kritik" = "Reinhard J. Brembeck",
        "Reinhardt Baumgart" = "Reinhard Baumgart",
        "Rose-Maria Gropp" = "Rose-Marie Gropp",
        "Siegline Geisel" = "Sieglinde Geisel",
        "Sonja Zekri" = "Sonja Zerki",
        "Stephan Mauss" = "Stephan Maus",
        "Stegan Maus" = "Stephan Maus",
        "Tobias Lehmuhl" = "Tobias Lehmkuhl",
        "Tom Wohlfarth" = "Tom Wohlfahrt",
        "Tomas Kurianowicz" = "Tomasz Kurianowicz",
        "Ulrich Ruesenauer" = "Ulrich Ruedenauer",
        "Wiebke Porombska" = "Wiebke Porombka",
        "Uwe Pralles Rezension" = "Uwe Pralle"
      )
    ),
    rev_gndr = case_when(
      rev_id == 322 ~ "F",
      rev_id == 672 ~ "F",
      rev_id == 806 ~ "F",
      rev_id == 859 ~ "M",
      rev_id == 1820 ~ "M",
      rev_id == 1898 ~ "M",
      rev_id == 2310 ~ "M",
      rev_id == 2377 ~ "F",
      rev_id == 2601 ~ "M",
      rev_id == 2757 ~ "M",
      rev_id == 3304 ~ "M",
      rev_id == 3378 ~ "M",
      rev_id == 5089 ~ "M",
      rev_id == 6201 ~ "M",
      rev_id == 7232 ~ "M",
      rev_name == "Andreas Maier" ~ "M",
      rev_name == "Angelika Overath" ~ "F",
      rev_name == "Elke Heidenreich" ~ "F",
      rev_name == "Gottfried Benn" ~ "M",
      rev_name == "Iris Radisch" ~ "F",
      rev_name == "Judith von Sternburg" ~ "F",
      rev_name == "Julian Weber" ~ "M",
      rev_name == "Lerke von Saalfeld" ~ "F",
      rev_name == "Maika Albath" ~ "F",
      rev_name == "Meike Fessmann" ~ "F",
      rev_name == "Oliver Jungen" ~ "M",
      rev_name == "Rainer Moritz" ~ "M",
      rev_name == "Shirin Sojitrawalla" ~ "F",
      rev_name == "Viola Roggenkamp" ~ "F",
      rev_name == "Ursula Maerz" ~ "F",
      is.na(rev_name) ~ NA,
      .default = rev_gndr
    )
  )



# ---- 8. combine with review informations and save ----
reviews_gndr_df <- full_join(reviews_df, revs_gndr, by = "rev_id")

saveRDS(reviews_gndr_df, file = "../data/reviews_gndr.RDS")


# test <- group_by(reviews_gndr_df, news, rev_name) |> summarize(n = n()) |> filter(n > 1)



# ---- 9. test quality of different approaches ----
# load("data/use/revs_name_gndr.Rdata")

qualitytest <- reviews_gndr %>%
  select(starts_with("rev_name"), starts_with("same"), starts_with("rev_gndr")) %>%
  mutate_at(vars(starts_with("rev")), funs(!is.na(.))) %>%
  summarise_all(funs(
    "per" = round(mean(., na.rm = T) * 100, 2),
    sum(., na.rm = T)
  )) %>%
  gather("var", "value") %>%
  mutate(var = str_replace(var, "_([per|sum])", ".\\1")) %>%
  separate(var, c("var", "typ"), "\\.") %>%
  spread(typ, value)
qualitytest <- qualitytest[c(4:7, 9:11, 1:3, 8), ]


library(gridExtra)
library(RColorBrewer)

plot1 <- ggplot(data = qualitytest[1:4, ], aes(x = var, y = sum, fill = var)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(qualitytest$per[1:4], "%"), vjust = -.5)) +
  geom_text(aes(label = sum, vjust = 1.5)) +
  geom_hline(yintercept = nrow(reviews_gndr), linetype = "dashed") +
  scale_y_continuous(breaks = seq(0, 7000, 1000)) +
  scale_fill_manual(values = brewer.pal(8, "Set2")[1:4]) +
  xlab("") +
  ylab("") +
  labs(title = "Namensfunde") +
  theme_classic() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line(colour = "Gray90")
  )

plot2 <- ggplot(data = qualitytest[5:7, ], aes(x = var, y = sum, fill = var)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(qualitytest$per[5:7], "%"), vjust = -.5)) +
  geom_text(aes(label = sum, vjust = 1.5)) +
  geom_hline(yintercept = nrow(reviews_gndr), linetype = "dashed") +
  scale_y_continuous(breaks = seq(0, 7000, 1000)) +
  scale_fill_manual(values = brewer.pal(8, "Set2")[5:7]) +
  xlab("") +
  ylab("") +
  labs(title = "Übereinstimmung") +
  theme_classic() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line(colour = "Gray90"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

plot <- grid.arrange(plot1, plot2, nrow = 1)
# ggsave("output/graphs/revs_names.png", plot)


plot1 <- ggplot(data = qualitytest[8:10, ], aes(x = var, y = sum, fill = var)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(qualitytest$per[8:10], "%"), vjust = -.5)) +
  geom_text(aes(label = sum, vjust = 1.5)) +
  geom_hline(yintercept = nrow(reviews_gndr), linetype = "dashed") +
  scale_y_continuous(breaks = seq(0, 7000, 1000)) +
  scale_fill_manual(values = brewer.pal(8, "Set2")[1:3]) +
  xlab("") +
  ylab("") +
  labs(title = "Geschlechtsfunde") +
  theme_classic() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line(colour = "Gray90")
  )

plot2 <- ggplot(data = qualitytest[11, ], aes(x = var, y = sum, fill = var)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(qualitytest$per[11], "%"), vjust = -.5)) +
  geom_text(aes(label = sum, vjust = 1.5)) +
  geom_hline(yintercept = nrow(reviews_gndr), linetype = "dashed") +
  scale_y_continuous(breaks = seq(0, 7000, 1000)) +
  scale_fill_manual(values = brewer.pal(8, "Set2")[4]) +
  xlab("") +
  ylab("") +
  labs(title = "Übereinstimmung") +
  theme_classic() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line(colour = "Gray90"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

plot <- grid.arrange(plot1, plot2, nrow = 1)
# ggsave("output/graphs/revs_gndr.png", plot)


### data and variable overview
# names.heise     list of forenames and gender from heise.com
# names.reviewers list of forenames and gender from reviews (by Rezensent/in)
# names           combined list of forenames and gender
# reviews_df      full review text data, pre-processt (drop authors names, ...)
# reviews_name1/3 reviews with each extracted names by different approach
# reviews_name    final reviews and all name approaches, similarity
# reviews_gndr

# review          original review
# text            remove special characters & (last)name of author(s)
# text2           remove also "Roman", quotes, lowercase
# rev_name1       reviewer name: by "Resenzent/in"
# rev_name2       reviewer name: by extracted reviewer names list
# rev_name3       reviewer name: by general name list (first occurence)
# same12          reviewer name: rev_name1 == rev_name2
# same13          reviewer name: rev_name1 == rev_name3
# same23          reviewer name: rev_name2 == rev_name3
# rev_gndr1       gender of reviewer by extracted forename and namelist
# rev_gndr2       gender of reviewer by "Rezensent/in"
# same_gndr       gender: rev_gndr1 == rev_gndr2

### prepare helper functions to extract infos from htmls

`%notin%` <- Negate(`%in%`)


url_decode_utf <- function(x) {
  y <- URLdecode(x)
  Encoding(y) <- "UTF-8"
  y
}


# get prize urls from main urls with containing lists, with special nodes
scrp_urls <- function(urls, node, main_url = url_wiki) {
  sapply(urls, function(url) {
    if (node == "auto") {
      if (grepl("^Kategorie:", url)) node <- "#mw-pages li a:nth-child(1)"
      if (grepl("^Liste_", url)) node <- "#h2+ ul a:nth-child(1)"
      if (grepl("Literaturpreisen#Schweiz", url)) node <- "ul:nth-child(56) a"
      if (grepl("Literaturpreisen$", url)) node <- "h2+ ul li > a:nth-child(1)"
    }

    message(url, "\n    ", node)

    Sys.sleep(1)

    rvest::read_html(paste0(main_url, "/wiki/", url)) |>
      rvest::html_nodes(node) |>
      rvest::html_attr("href")
  }) |>
    unlist()
}


# scrape list of raw html in text format to store
scrp_htmls <- function(urls, main_url = url_wiki) {
  htmls_ls <- lapply(urls, function(url) {
    message(url)

    Sys.sleep(1)

    httr::GET(paste0(main_url, url)) |> httr::content(as = "text")
  })

  names(htmls_ls) <- urls

  return(htmls_ls)
}


# get clean text from html
html_clean_text <- function(html) {
  text <- rvest::html_text(html) |>
    stringr::str_remove_all("(\\[.+\\]|\\n)$") |>
    stringr::str_squish()

  text[purrr::is_empty(text)] <- NA

  return(text)
}


# get all urls and titles from urls
get_name_link <- function(iterator, attr) {
  href <- rvest::html_elements(iterator, "a")
  href <- href[grepl("title=", href, perl = TRUE)] # withtitle -> no footnotes

  text <- rvest::html_attr(href, attr) |>
    paste(collapse = ";")

  text[text == ""] <- NA

  return(text)
}


get_years <- function(txt) {
  stringr::str_extract_all(txt, regex_year) |>
    sapply(paste, collapse = ";") |>
    stringr::str_replace_all("^NA$", "") |>
    dplyr::na_if("")
}



wiki_prizes <- function(htmls) {
  lapply(seq_along(htmls), function(index) {
    url <- names(htmls[index])

    message(url)

    ### A) prepare list of html chapters (split by heading tags)

    # get raw html from list
    html_raw <- read_html(htmls[[index]])

    # see if wiki url to page leads to prize as subtopic (redirect to #-chapter)
    redirect_title <- str_extract(
      as.character(html_raw),
      "(?<=RedirectTargetUrl\":\"/wiki/.{1,40}#).+?(?=\")"
    ) |>
      str_replace_all("_", " ")

    # get title of wikipedia page
    wiki_title <- html_element(html_raw, "#firstHeading") |> html_text()

    # get just body of html and main field as text (remove table of content)
    html_body <- html_element(html_raw, ".mw-parser-output")

    # remove table of content
    html_remove <- html_elements(html_body, "#toc")
    xml_remove(html_remove)

    html_text <- as.character(html_body)

    # split raw html text on headings (to loop over sub-chapters and get titles)
    html_split <- unlist(strsplit(html_text, "(?=\\n<h[234]>)", perl = TRUE))

    html_list <- as.list(html_split[!grepl("^\\n$", html_split, perl = TRUE)])


    ### B) loop over html chapter text blocks -> extract list and table elements
    list_dfs <- lapply(html_list, function(text_block) {
      # create minimal html from raw text block
      html <- minimal_html(text_block)

      # create data.frame with different chapter titles per level
      df_struc <- sapply(c("h2", "h3", "h4"), function(headtag) {
        html |>
          html_elements(headtag) |>
          html_clean_text()
      }) |>
        as.list() |>
        data.frame() |>
        # if first chapter is empty -> add manually
        mutate(h2 = ifelse(sum(is.na(c_across(h2:h4))) == 3,
          "first chapter", h2
        )) |>
        # create chapter column for check in loop to skip
        unite(chapter, remove = FALSE, na.rm = TRUE) |>
        # create indicator for level of chapter (later fill higher levels)
        mutate(level = case_when(
          !is.na(h2) ~ 1, !is.na(h3) ~ 2, !is.na(h4) ~ 3
        ))


      # check if chapter title is valid (no standard) -> proceed; otherwise NULL
      # if redirected -> just check chapter with same title as link
      if (grepl(regex_skp_chpt, df_struc$chapter, perl = TRUE) &
        !grepl("preis", df_struc$chapter, ignore.case = TRUE, perl = TRUE)) {
        return(NULL)
      } else if (!is.na(redirect_title) & df_struc$chapter != redirect_title) {
        return(NULL)
      } else {

        ### 1. check if chapter contains list elements -> extract infos
        list <- html_elements(html, "li, dd")

        if (length(list) > 0) {
          # remove nested list from higher list element (appears after)
          remove_ul <- html_elements(list, "ul")
          xml_remove(remove_ul)

          # create data.frame from list elements
          df_list <- data.frame(
            text = html_clean_text(list),
            name = sapply(list, get_name_link, "title"),
            link = sapply(list, get_name_link, "href"),
            tag = str_extract(as.character(list), "<.+?>")
          ) |>
            mutate(
              year = get_years(text),
              note = ifelse(
                length(remove_ul) > 0 & is.na(year), "sublist", "list"
              )
            )

          # fill year if (sub)-chapter is year
          if (grepl("^[0-9]{4,4}$", df_struc$chapter)) {
            df_list$year <- get_years(df_struc$chapter)
            df_list$note <- paste(df_list$note, "& chapter year")
          }

          # check for sub-headings with dl/b tag -> get years and add to data
          subh_years <- html_elements(html, "dl, b") |> # years in subheadings
            html_clean_text() |>
            get_years() |>
            na.omit()
          # length of sublists (see different tags for different styls)
          subl_nds <- html_elements(html, "ul, ol, dd, dl+ p")
          subl_n <- sapply(subl_nds, function(x) length(html_elements(x, "li")))

          # if no NA & same length -> multiplie subheading years with sublists N
          if (length(subh_years) == length(subl_n)) {
            subh_years_all <- rep(subh_years, subl_n)

            # if same length as df -> add year as column
            if (nrow(df_list) == length(subh_years_all)) {
              df_list$year <- subh_years_all
              df_list$note <- paste(df_list$note, "& subhead")
              df_list$tag <- as.character(subl_nds) |>
                str_extract("<.+?>") |>
                rep(subl_n)

              # if not same length: merge year over same text
            } else if (!is_empty(subh_years_all)) {
              df_subl_year <- data.frame(
                text = html_elements(html, "ul") |> html_clean_text(),
                year = subh_years_all
              )

              df_list <- select(df_list, !year) |>
                left_join(df_subl_year, by = "text") |>
                mutate(note = paste(note, "& subhead"))
            }
          }

          # fill nested list with year from first element (sub-list oder dd)
          if (length(remove_ul) > 0 | any(df_list$tag == "<dd>")) {
            df_list <- fill(df_list, year)
          }
        } else {
          df_list <- data.frame()
        }


        ### 2. check if chapter contains table -> extract infos
        table <- html_elements(html, "table")
        df_table <- data.frame()

        if (length(table) > 0) {

          # get years from subheadings if multiple tables per chapter
          subhead <- html_elements(html, "dt, b") |> html_clean_text()
          subh_years <- get_years(subhead)

          # get name space from table for cells (see html_table from rvest)
          ns <- xml_ns(table)

          # loop over all tables (try to add year subheadings if missing column)
          ls_table <- lapply(seq_along(table), function(tab) {

            # skip (irst) if main table contains sub-tables -> they follow
            if (str_count(as.character(table[[tab]]), "<table") > 1) {
              return(NULL)
            }

            # prepare rows and cells for extraction of infos
            rows <- xml_find_all(table[tab], ".//tr", ns = ns)
            cells <- lapply(rows, xml_find_all, ".//td|.//th", ns = ns)

            # loop over table if year included -> get columns
            year_cols <- which(grepl("Jahr|Preisjahr", cells[[1]], perl = TRUE))
            winner_cols <- which(
              grepl(regex_winner_cols, cells[[1]], perl = TRUE)
            )

            # if no year column but valid year in subheading -> add column
            if (length(year_cols) == 0 && !is.na(subh_years[tab])) {

              # add new column title for year in first row
              xml_find_first(rows[1], ".//th") |>
                xml_add_sibling("th", "Jahr", .where = "before")
              # add column with year of heading for this table for all rows
              xml_find_first(rows, ".//td") |>
                xml_add_sibling("td", subh_years[tab], .where = "before")

              # rewrite rows, cells and set year and add 1 to winner column(s)
              rows <- xml_find_all(table[tab], ".//tr", ns = ns)
              cells <- lapply(rows, xml_find_all, ".//td|.//th", ns = ns)

              year_cols <- 1
              winner_cols <- winner_cols + 1
            }

            if (length(year_cols) > 0 && length(winner_cols) > 0) {

              # iterate over column(s) and rows to get year in row(s)
              years <- lapply(year_cols, function(col) {
                lapply(2:length(cells), function(row) {
                  year <- html_clean_text(cells[[row]][col]) |> get_years()

                  # check if years span over multiple rows -> add year
                  rowspan <- as.numeric(html_attr(cells[[row]][col], "rowspan"))

                  if (!is.na(rowspan)) {

                    # for length of rowspan: add year to table row node
                    for (rwspn in 1:(rowspan - 1)) {

                      # https://stackoverflow.com/questions/64329547/
                      # how-do-i-add-and-rearrange-nodes-of-an-xml-file-in-r
                      rows[[row + rwspn]] |>
                        xml_find_first(".//td") %>%
                        xml_add_sibling("td", year, .where = "before")
                    }

                    # add changes of rows to list of cells
                    # use double << for assignment outside of lapply
                    cells <<- lapply(rows, xml_find_all, ".//td|.//th", ns = ns)
                  }

                  data.frame(year = year)
                })
              }) |>
                bind_rows()


              # iterate over each column(s) and rows with winner or authors
              winners <- lapply(winner_cols, function(col) {
                lapply(2:length(cells), function(row) {
                  tryCatch(
                    {
                      data.frame(
                        text = html_clean_text(cells[[row]][col]),
                        name = get_name_link(cells[[row]][col], "title"),
                        link = get_name_link(cells[[row]][col], "href"),
                        tabhead = html_clean_text(cells[[1]][col]),
                        note = "table"
                      )
                    },
                    error = function(e) {
                      data.frame(
                        text = NA, name = NA, link = NA, tabhead = NA,
                        note = "empty cell"
                      )
                    }
                  )
                })
              }) |>
                bind_rows()

              rm(cells)

              df_tab <- cbind(years, winners)

              # add note and title variable
              if (!is.na(subh_years[tab])) {
                df_tab$note <- paste(df_tab$note, "& subhead")
                df_tab$subhead <- subhead[tab]
              }

              return(df_tab)
            } else { # if no year or winner colum(s) -> add note
              return(data.frame(note = "table & invalid columns"))
            }
          })

          df_table <- bind_rows(ls_table)
        }

        message(
          "    ", df_struc$chapter,
          " - List: ", nrow(df_list), "; Table: ", nrow(df_table)
        )

        df_chapter <- bind_rows(df_list, df_table)


        if (nrow(df_chapter) == 0) {
          df_chapter <- bind_rows(df_chapter, data.frame(note = "empty"))
        }

        # add chapter heading and level information (structure of page)
        df_chapter <- cbind(df_chapter, df_struc)

        # add note if redirected url used just this one sub-chapter
        if (!is.na(redirect_title)) {
          df_chapter$note <- paste(df_chapter$note, "& redirect")
        }


        return(df_chapter)
      }
    })

    # finalize list of data.frames from chapters for one wiki page
    df_page <- bind_rows(list_dfs) |>
      mutate(
        title = wiki_title, url_prize = url, .before = 1,
        # help variable to fill higher chapter level headings to subchapter
        group = cumsum(level < lag(level, def = first(level)))
      ) |>
      group_by(group) |>
      fill(starts_with("h")) |>
      ungroup() |>
      select(!c(chapter, group))

    return(df_page)
  })
}


multi_years <- function(var = year) {
  year_list <- stringr::str_split(var, pattern = ";") |>
    unlist() |>
    as.numeric()

  if (length(year_list) < 2) {
    return(var)
  }
  if (length(year_list) > 2) {
    return(var) # return("multi year")
  }

  # if year_list == 2
  if (abs(diff(year_list, 1)) >= 2) {
    return(var) # return("big diff")
  }

  # if diff(year_list) == 2
  return(max(year_list))
}


dnb_search <- function(title, author, year, publisher, keyword, type, language, limit=10, clean=TRUE, print=FALSE) {
  # init query
  query <- ""

  # prepare title
  if(!missing(title)) {
    title <- paste0("tit=", title, collapse=" OR ")
    title <- gsub("OR tit=+", "AND tit=", title, fixed=TRUE)
    title <- gsub("OR tit=-", "NOT tit=", title, fixed=TRUE)
    if(substr(title, 1, 5)=="tit=+" || substr(title, 1, 5)=="tit=-") stop("Do not use '+' or '-' in case of a single search string or with first string of a vector")
    query <- paste0("(", title, ")")
  }

  # prepare author
  if(!missing(author)) {
    author <- paste0("atr=", author, collapse=" OR ")
    author <- gsub("OR atr=+", "AND atr=", author, fixed=TRUE)
    author <- gsub("OR atr=-", "NOT atr=", author, fixed=TRUE)
    if(substr(author, 1, 5)=="atr=+" || substr(author, 1, 5)=="atr=-") stop("Do not use '+' or '-' in case of a single search string or with first string of a vector")
    if(query=="") query <- paste0("(", author, ")")
    else query <- paste(query, paste0("(", author, ")"), sep=" AND ")
  }

  # prepare year
  if(!missing(year)) {
    if(length(year)>1 && (tail(year, 1)-year[1]+1)==length(year)) {
      year <- paste0("jhr>=", year[1], " AND jhr<=", tail(year, 1))
    } else {
      year <- paste0("jhr=", year, collapse=" OR ")
    }
    if(query=="") query <- paste0("(", year, ")")
    else query <- paste(query, paste0("(", year, ")"), sep=" AND ")
  }

  # prepare publisher
  if(!missing(publisher)) {
    publisher <- paste0("vlg=", publisher, collapse=" OR ")
    publisher <- gsub("OR vlg=+", "AND vlg=", publisher, fixed=TRUE)
    publisher <- gsub("OR vlg=-", "NOT vlg=", publisher, fixed=TRUE)
    if(substr(publisher, 1, 5)=="vlg=+" || substr(publisher, 1, 5)=="vlg=-") stop("Do not use '+' or '-' in case of a single search string or with first string of a vector")
    if(query=="") query <- paste0("(", publisher, ")")
    else query <- paste(query, paste0("(", publisher, ")"), sep=" AND ")
  }

  # prepare keyword
  if(!missing(keyword)) {
    keyword <- paste0("sw=", keyword, collapse=" OR ")
    keyword <- gsub("OR sw=+", "AND sw=", keyword, fixed=TRUE)
    keyword <- gsub("OR sw=-", "NOT sw=", keyword, fixed=TRUE)
    if(substr(keyword, 1, 5)=="sw=+" || substr(keyword, 1, 5)=="sw=-") stop("Do not use '+' or '-' in case of a single search string or with first string of a vector")
    if(query=="") query <- paste0("(", keyword, ")")
    else query <- paste(query, paste0("(", keyword, ")"), sep=" AND ")
  }

  # prepare type
  if(!missing(type)) {
    avail.types <- c("articles", "manuscript", "biographicaldoc", "letters", "bequest", "collections", "books", "brailles", "maps", "discs", "dissertations", "online", "films", "microfiches", "multimedia", "music", "scores", "serials", "persons", "subjects", "corperations", "works", "events", "geographics")
    type <- avail.types[pmatch(type, avail.types)]
    type <- paste0("mat=", type, collapse=" OR ")
    if(query=="") query <- paste0("(", type, ")")
    else query <- paste(query, paste0("(", type, ")"), sep=" AND ")
  }

  # prepare language
  if(!missing(language)) {
    language <- paste0("spr=", language, collapse=" OR ")
    if(query=="") query <- paste0("(", language, ")")
    else query <- paste(query, paste0("(", language, ")"), sep=" AND ")
  }

  # call dnb_advanced
  df <- dnb_advanced(query=query, limit=limit, clean=clean, print=FALSE)

  # return
  if(print) print(df)
  invisible(df)
}

################################################################################

dnb_advanced_sm <- function(query, limit=10, clean=TRUE, print=FALSE) {

  # prepare limit
  if(any(limit=="all")) {
    lim <- 100
    strt <- 1
  } else if(is.numeric(limit)) {
    if(length(limit)==1) {
      lim <- limit
      strt <- 1
    } else if(length(limit)==2) {
      lim <- limit[1]
      strt <- limit[2]
    } else stop("cannot read 'limit'")
  } else stop("cannot read 'limit'")

  # make request
  req <- dnb_get_url(path="sru/dnb", query=query, limit=lim, start=strt)
  raw <- dnb_parse(req)$searchRetrieveResponse

  # print number of records
  nrec <- as.numeric(raw[["numberOfRecords"]])
  if(nrec == 0) {
    message("no records found")
    return(NULL)
  }

  # convert
  df <- dnb_to_df_sm(raw, clean = clean)

  # loop request for more than 100 records
  nend <- NULL
  if(any(limit == "all" || lim > 100)) {
    if(any(limit == "all")) {
      nend <- nrec
    } else {
      nend <- lim
      lim <- 100
    }

    strt <- as.numeric(raw[["nextRecordPosition"]])
    # pb <- txtProgressBar(min=0, max=nend, style=3)

    repeat {
      if(is_empty(strt)) break # no nextRecordPosition if last loop
      if(strt > nend) break
      req <- dnb_get_url(path="sru/dnb", query=query, limit=lim, start=strt)
      raw <- dnb_parse(req)$searchRetrieveResponse
      df_add <- dnb_to_df_sm(raw, clean=clean)
      df <- rbind(df, df_add)
      strt <- as.numeric(raw[["nextRecordPosition"]])
      # setTxtProgressBar(pb, strt)
    }
    # setTxtProgressBar(pb, nend)
    # close(pb)
    if(limit != "all") df <- df[1:nend,]
  }

  # print number of records
  if(limit == "all") message(nrec, " records found")
  else {
    if(!is.null(nend)) message(nrec, " records found (request limited to ", nend, " records)")
    else message(nrec, " records found (request limited to ", lim, " records)")
  }

  # add metadata
  attr(df, "number_of_records") <- nrec
  attr(df, "query") <- unlist(raw[["echoedSearchRetrieveRequest"]][["query"]])

  # return
  if(print) print(df)
  invisible(df)
}

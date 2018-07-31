# create environment in which to put cookie_handler
.pkgenv <- new.env(parent=emptyenv())

# alternative url is "http://apis.google.com/Cookies/OTZ"
# function to create cookie_handler, which is necessary to run get_widget()
get_api_cookies <- function(cookie_url) {
  # create new handler
  cookie_handler <- curl::new_handle()
  # VY. set options for the proxy
  curl::handle_setopt(handle=cookie_handler,.list=list(ssl_verifypeer=0L,proxyuserpwd=paste(.pkgenv[["handle_domain"]],"\\",.pkgenv[["handle_user"]],":",.pkgenv[["handle_password"]],sep=""),proxyauth=.pkgenv[["handle_proxyauth"]],proxy=.pkgenv[["handle_proxyhost"]],proxyport=.pkgenv[["handle_proxyport"]]))
  # fetch API cookies
  cookie_req <- curl::curl_fetch_memory(cookie_url, handle = cookie_handler)
  curl::handle_cookies(cookie_handler)
  # assign handler to .pkgenv environment
  .pkgenv[["cookie_handler"]] <- cookie_handler
  return(NULL)
}

check_time <- function(time) {
  stopifnot(is.character(time))

  fixed_format <- c(
    "now 1-H", # last hour
    "now 4-H", # last four hours
    "now 1-d", # last day
    "now 7-d", # last seven days
    "today 1-m", # past 30 days
    "today 3-m", # past 90 days
    "today 12-m", # past 12 months
    "today+5-y", # last 5 years (default)
    "all" # Since begening of Google Trends (2004)
  )

  ## Return TRUE if one of the basic date formats is used
  if (time %in% fixed_format) {
    return(TRUE)
  }

  ## The other possible format is by using time range

  time <- unlist(strsplit(time, " "))

  ## Need to be a vector of two
  if (length(time) != 2) {
    return(FALSE)
  }

  start_date <- anytime::anydate(time[1])
  end_date <- anytime::anydate(time[2])

  if (is.na(start_date) | is.na(end_date)) {
    return(FALSE)
  }

  ## Start date can't be after end date
  if (start_date >= end_date) {
    return(FALSE)
  }

  ## Start date can't be before 204-01-01
  if (start_date < as.Date("2004-01-01")) {
    return(FALSE)
  }

  ## End date can't be after today
  if (end_date > Sys.Date()) {
    return(FALSE)
  }

  return(TRUE)
}


get_widget <- function(comparison_item, category, gprop, hl, cookie_url) {
  token_payload <- list()
  token_payload$comparisonItem <- comparison_item
  token_payload$category <- category
  token_payload$property <- gprop

  # token_payload$comparisonItem$keyword <- curl::curl_escape(token_payload$comparisonItem$keyword)
  
  url <- URLencode(paste0(
    "https://www.google.com/trends/api/explore?property=&req=",
    jsonlite::toJSON(token_payload, auto_unbox = TRUE),
    "&tz=300&hl=", hl
  )) ## The tz part is unclear but different
  ## valid values do not change the result:
  ## clarification needed.
  
  url <- encode_keyword(url)
  
  # if cookie_handler hasn't been set up, get the requisite cookies from Google's API
  if(!exists("cookie_handler", envir = .pkgenv)){ get_api_cookies(cookie_url) }
  # get the tokens etc., using the URL and the cookie_handler
  widget <- curl::curl_fetch_memory(url, handle = .pkgenv[["cookie_handler"]])

  stopifnot(widget$status_code == 200)

  ## Fix encoding issue for keywords like österreich"
  temp <- rawToChar(widget$content)
  Encoding(temp) <- "UTF-8"

  myjs <- jsonlite::fromJSON(substring(temp, first = 6))

  widget <- myjs$widgets
}

interest_over_time <- function(widget, comparison_item) {
  payload2 <- list()
  payload2$locale <- widget$request$locale[1]
  payload2$comparisonItem <- widget$request$comparisonItem[[1]]
  payload2$resolution <- widget$request$resolution[1]
  payload2$requestOptions$category <- widget$request$requestOptions$category[1]
  payload2$requestOptions$backend <- widget$request$requestOptions$backend[1]
  payload2$time <- widget$request$time[1]
  payload2$requestOptions$property <- widget$request$requestOptions$property[1]


  url <- URLencode(paste0(
    "https://www.google.com/trends/api/widgetdata/multiline/csv?req=",
    jsonlite::toJSON(payload2, auto_unbox = T),
    "&token=", widget$token[1],
    "&tz=300"
  ))

  # ****************************************************************************
  # Downoad the results
  # ****************************************************************************
  url <- encode_keyword(url)
  
  # VY. use the handler with proxy options.
  res <- curl::curl_fetch_memory(url, handle = .pkgenv[["cookie_handler"]])

  stopifnot(res$status_code == 200)

  # ****************************************************************************
  # Format the results in a nice way
  # ****************************************************************************
  con <- textConnection(rawToChar(res$content))
  df <- read.csv(con, skip = 1, stringsAsFactors = FALSE)
  close(con)

  if (nrow(df) < 1) {
    return(NULL) ## No data
  }

  n <- nrow(df) # used to reshape the data

  df <- reshape(
    df,
    varying = names(df)[2:ncol(df)],
    v.names = "hits",
    direction = "long",
    timevar = "temp",
    times = names(df)[2:ncol(df)]
  )

  df$temp <- NULL

  df <- cbind(
    df,
    comparison_item[rep(seq_len(nrow(comparison_item)), each = n), 1:2],
    row.names = NULL
  )

  df$geo <- ifelse(df$geo == "", "world", df$geo)
  df$gprop <- ifelse(widget$request$requestOptions$property[1] == "", "web", widget$request$requestOptions$property[1])
  df$category <- widget$request$requestOptions$category[1]
  names(df)[1] <- "date"
  df$id <- NULL

  # Format the returned date
  if (unique(comparison_item$time) == "all") {
    df$date <- anytime::anydate(df$date)
  } else {
    df$date <- anytime::anytime(df$date)
  }

  return(df)
}


interest_by_region <- function(widget, comparison_item, low_search_volume) {
  i <- which(grepl("Interest by", widget$title) == TRUE)

  if (length(i) == 0) {
    return(list(NULL))
  }
  
  ## Interest by region need to be retreived individually

  # resolution <- sub(".* (\\w+)$", "\\1", widget$title[i])
  # resolution[resolution == "subregion"] <- "region"
  # resolution[resolution == "metro"] <- "dma"

  # resolution <- c(resolution, rep(c("city", "dma"), each = length(resolution)))

  ##
  resolution <-
    expand.grid(i, c(ifelse(
      grepl("world", na.omit(widget$geo)), "country", "region"
    ), "city", "dma"), stringsAsFactors = FALSE)

  resolution <- unique(resolution)

  i <- resolution$Var1
  resolution <- resolution$Var2

  ## If it is not US metro, then also search for "city"
  # if (!all(grepl("dma", resolution))) {
  #   resolution <- c(resolution, rep("city", length(resolution)))
  # }
  #

  ## If no country is specified, resolution should be "COUNTRY"
  # resolution[grepl("world", na.omit(widget$geo))] <- "country"
  resolution <- toupper(resolution)

  res <-
    mapply(
      create_geo_payload,
      i,
      resolution,
      MoreArgs = list(widget = widget, low_search_volume = low_search_volume),
      SIMPLIFY = FALSE
    )

  ## Remove duplicated
  ii <- !duplicated(res)
  res <- res[ii]
  resolution <- resolution[ii]

  ## Remove NA
  ii <- !unlist(lapply(res, is.null))
  res <- res[ii]
  resolution <- resolution[ii]


  res <- setNames(res, tolower(resolution))

  return(res)
}


create_geo_payload <- function(i, widget, resolution, low_search_volume) {
  payload2 <- list()
  payload2$locale <- unique(na.omit(widget$request$locale))
  payload2$comparisonItem <- widget$request$comparisonItem[[i]]
  payload2$resolution <- resolution
  payload2$requestOptions$backend <- widget$request$requestOptions$backend[i]
  payload2$requestOptions$property <- widget$request$requestOptions$property[i]
  payload2$requestOptions$category <- widget$request$requestOptions$category[i]
  payload2$geo <- as.list((widget$request$geo[i, , drop = FALSE]))
  payload2$includeLowSearchVolumeGeos <- low_search_volume


  url <- URLencode(paste0(
    "https://www.google.com/trends/api/widgetdata/comparedgeo/csv?req=",
    jsonlite::toJSON(payload2, auto_unbox = T),
    "&token=", widget$token[i],
    "&tz=300&hl=en-US"
  ))

  url <- encode_keyword(url)
  # VY. use the handler with proxy options.
  res <- curl::curl_fetch_memory(url, handle = .pkgenv[["cookie_handler"]])

  if (res$status_code != 200) {
    return(NULL)
  }

  con <- textConnection(rawToChar(res$content))
  df <- read.csv(con, skip = 1, stringsAsFactors = FALSE)
  close(con)

  if (nrow(df) == 0) {
    return(NULL)
  }

  n <- nrow(df) # used to reshape the data

  df <- reshape(
    df,
    varying = names(df)[2:ncol(df)],
    v.names = "hits",
    direction = "long",
    timevar = "temp",
    times = names(df)[2:ncol(df)]
  )


  kw <- do.call(rbind, widget$request$comparisonItem[[i]]$complexKeywordsRestriction$keyword)

  df <- cbind(
    df,
    kw[rep(seq_len(nrow(kw)), each = n), 2],
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  df$temp <- NULL
  # df$geo <- widget$geo[i]
  df$geo <- suppressWarnings(na.omit(unlist(widget$request$geo[i, ])))

  df$geo <- ifelse(is.null(df$geo), "world", df$geo)
  df$gprop <- ifelse(widget$request$requestOptions$property[i] == "", "web", widget$request$requestOptions$property[i])

  df$id <- NULL
  rownames(df) <- NULL

  names(df) <- c("location", "hits", "keyword", "geo", "gprop")

  return(df)
}

## Remove NA from list
na.omit.list <- function(y) {
  return(y[!sapply(y, function(x)
    all(is.na(x)))])
}

## Replace special characters in keywords like P&500 -> P%26500 
encode_keyword <- function(url) {
  gsub("(?:\\G(?!^)|\\[\\s*)[^][\\s]*\\K\\&(?!])(?=[^][]*])", "%26", url, perl = TRUE)
}

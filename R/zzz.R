# create environment in which to put cookie_handler
.pkgenv <- new.env(parent = emptyenv())

# alternative url is "http://apis.google.com/Cookies/OTZ"
# function to create cookie_handler, which is necessary to run get_widget()
get_api_cookies <- function(cookie_url) {
  # create new handler
  cookie_handler <- curl::new_handle()

  # set options for the proxy
  proxy_domain <- ifelse(is.null(.pkgenv[["handle_domain"]]), "", paste0(.pkgenv[["handle_domain"]], "\\"))
  proxy_user_pwd <- paste0(proxy_domain, .pkgenv[["handle_user"]], ":", .pkgenv[["handle_password"]])
  curl_opts <- list(
    ssl_verifypeer = 0L,
    proxyuserpwd = proxy_user_pwd,
    proxyauth = .pkgenv[["handle_proxyauth"]],
    proxy = .pkgenv[["handle_proxyhost"]],
    proxyport = .pkgenv[["handle_proxyport"]]
  )
  ## add extra curl options
  curl_opts <- append(curl_opts, .pkgenv[["handle_extra_curl_opts"]])
  curl::handle_setopt(handle = cookie_handler, .list = curl_opts)

  # fetch API cookies
  cookie_req <- curl::curl_fetch_memory(cookie_url, handle = cookie_handler)
  curl::handle_cookies(cookie_handler)
  # assign handler to .pkgenv environment
  .pkgenv[["cookie_handler"]] <- cookie_handler
  return(NULL)
}

check_time <- function(time_ranges) {
  stopifnot(is.character(time_ranges))

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

  for (tr in time_ranges) {

    ## Return TRUE if one of the basic date formats is used
    if (tr %in% fixed_format) {
      return(TRUE)
    }

    ## The other possible format is by using time range
    time <- unlist(strsplit(tr, " "))

    ## Need to be a vector of two
    if (length(time) != 2) {
      return(FALSE)
    }

    if (!grepl("T", time[1])) {
      start_date <- as.POSIXct(format(anytime::anydate(time[1], tz = "UTC"), tz = "UTC"), tz = "UTC")
      end_date <- as.POSIXct(format(anytime::anydate(time[2], tz = "UTC"), tz = "UTC"), tz = "UTC")
    } else {
      start_date <- anytime::anytime(time[1])
      end_date <- anytime::anytime(time[2])
    }

    if (is.na(start_date) | is.na(end_date)) {
      return(FALSE)
    }

    ## Start date can't be after end date
    if (start_date >= end_date) {
      return(FALSE)
    }

    ## Start date can't be before 2004-01-01
    if (start_date < as.POSIXct("2004-01-01", tz = "UTC")) {
      return(FALSE)
    }

    ## End date can't be after today
    if (end_date > as.POSIXct(Sys.time())) {
      return(FALSE)
    }
  }

  return(TRUE)
}


get_widget <- function(comparison_item, category, gprop, hl, cookie_url, tz) {
  token_payload <- list()
  token_payload$comparisonItem <- comparison_item
  token_payload$category <- category
  token_payload$property <- gprop

  url <- paste0(
    URLencode(paste0("https://trends.google.com/trends/api/explore?hl=", hl, "&tz=", tz, "&req=")),
    encode_payload(paste0(jsonlite::toJSON(token_payload, auto_unbox = TRUE)), reserved = TRUE, repeated = TRUE),
    URLencode(paste0("&tz=", tz))
  )

  # url <- encode_keyword(url)

  # if cookie_handler hasn't been set up, get the requisite cookies from Google's API
  if (!exists("cookie_handler", envir = .pkgenv)) {
    get_api_cookies(cookie_url)
  }
  # get the tokens etc., using the URL and the cookie_handler
  widget <- curl::curl_fetch_memory(url, handle = .pkgenv[["cookie_handler"]])

  stopifnot(widget$status_code == 200)

  ## Fix encoding issue for keywords like österreich"
  temp <- rawToChar(widget$content)
  Encoding(temp) <- "UTF-8"

  myjs <- jsonlite::fromJSON(substring(temp, first = 6))

  # The above will silently convert strings containing "NA" to
  # a logical NA (see: https://github.com/jeroen/jsonlite/issues/314)
  # There currently is no way to fix this other than setting simplifyVector = F
  # which likely breaks other things. Thus what follows is a hacky fix to
  # catch the likely case that somebody is looking for searches with
  # geo="NA"
  if (is.logical(myjs$widgets$request$comparisonItem[[1]]$geo$country)) {
    myjs$widgets$request$comparisonItem[[1]]$geo$country <- comparison_item$geo
    myjs$widgets$request$geo$country <- comparison_item$geo
    myjs$widgets$request$restriction$geo$country <- comparison_item$geo
    myjs$widgets$geo <- comparison_item$geo
  }

  widget <- myjs$widgets
}

interest_over_time <- function(widget, comparison_item, tz) {
  payload2 <- list()
  # if there is a mix of search and topic terms requests are all shifted by one
  # for some reason. Maybe there is a better fix for this. I don't understand
  # precisely the structure of the widget.
  # try this example:
  # topicKeys <- c("/m/05s_khw", "Assassins Creed Brotherhood", "/m/0gmg6lv")
  # vs.
  # topicKeys <- c("Assassins Creed", "Assassins Creed Brotherhood", "Assassins Creed Rogue")
  # gtrends(topicKeys, time = "all")

  # the following conditional statments are necessary when no keyword is supplied but
  # a search for a category is called for
  if (is.null(unlist(widget$request$comparisonItem))) {
    onlyCategory <- TRUE
  } else if (!any(grepl("keyword", names(unlist(widget$request$comparisonItem))))) {
    onlyCategory <- TRUE
  } else {
    onlyCategory <- FALSE
  }

  if (onlyCategory) {
    payload2$locale <- widget$request$locale[1]
    payload2$comparisonItem <- widget$request$comparisonItem[[1]]
    payload2$resolution <- widget$request$resolution[1]
    payload2$requestOptions$category <- widget$request$requestOptions$category[1]
    payload2$requestOptions$backend <- widget$request$requestOptions$backend[1]
    payload2$time <- widget$request$time[1]
    payload2$requestOptions$property <- widget$request$requestOptions$property[1]
    token_payload2 <- widget$token[1]
    url <- paste0(
      URLencode("https://www.google.com/trends/api/widgetdata/multiline/csv?req="),
      URLencode(jsonlite::toJSON(payload2, auto_unbox = T, null = "list"), reserved = TRUE),
      URLencode(paste0("&token=", token_payload2, "&tz=", tz))
    )
  } else {
    Test.For.Multiple.Timeframes <- (length(widget$request$comparisonItem[[2]]$time) != 1) &
      (length(unique(widget$request$comparisonItem[[2]]$time)) != 1) &
      (!is.null(widget$request$comparisonItem[[2]]))


    if (Test.For.Multiple.Timeframes) {
      payload2$time <- widget$request$time[head(which(!is.na(widget$request$time)), 1)]
      payload2$time <- gsub(" ", "+", payload2$time)
      payload2$resolution <- widget$request$resolution[head(which(!is.na(widget$request$resolution)), 1)]
      payload2$locale <- widget$request$locale[head(which(!is.na(widget$request$locale)), 1)]
      payload2$comparisonItem <- widget$request$comparisonItem[[2]]
      payload2$comparisonItem$geo <- widget$request$comparisonItem[[2]]$geo
      payload2$requestOptions$property <- widget$request$requestOptions$property[2]
      payload2$requestOptions$backend <- widget$request$requestOptions$backend[2]
      payload2$requestOptions$category <- widget$request$requestOptions$category[2]
      token_payload2 <- widget$token[which(widget$id == "TIMESERIES")]


      url <- paste0(
        URLencode("https://trends.google.com/trends/api/widgetdata/multirange/csv?req="),
        encode_payload(jsonlite::toJSON(payload2, auto_unbox = T, null = "list"), reserved = TRUE),
        URLencode(paste0("&token=", token_payload2, "&tz=", tz))
      )
      # url <- URLencode(paste0(
      #   "https://www.google.com/trends/api/widgetdata/multirange/csv?req=",
      #   jsonlite::toJSON(payload2, auto_unbox = T,null="list"),
      #   "&token=", token_payload2,
      #   "&tz=",tz
      # ))
    } else {
      if (!is.na(widget$request$locale[1]) | (length(unique(unlist(widget$request$comparisonItem[[1]]$geo))) > 1)) {
        payload2$locale <- widget$request$locale[1]
        payload2$comparisonItem <- widget$request$comparisonItem[[1]]
        payload2$resolution <- widget$request$resolution[1]
        payload2$requestOptions$category <- widget$request$requestOptions$category[1]
        payload2$requestOptions$backend <- widget$request$requestOptions$backend[1]
        payload2$time <- widget$request$time[1]
        payload2$requestOptions$property <- widget$request$requestOptions$property[1]
        token_payload2 <- widget$token[1]
      } else {
        payload2$locale <- widget$request$locale[2]
        payload2$comparisonItem <- widget$request$comparisonItem[[1]]
        payload2$resolution <- widget$request$resolution[2]
        payload2$requestOptions$category <- widget$request$requestOptions$category[2]
        payload2$requestOptions$backend <- widget$request$requestOptions$backend[2]
        payload2$time <- widget$request$time[2]
        payload2$requestOptions$property <- widget$request$requestOptions$property[2]
        token_payload2 <- widget$token[2]
      }
      url <- paste0(
        URLencode("https://www.google.com/trends/api/widgetdata/multiline/csv?req="),
        URLencode(jsonlite::toJSON(payload2, auto_unbox = T, null = "list"), reserved = TRUE),
        URLencode(paste0("&token=", token_payload2, "&tz=", tz))
      )
    }
  }

  # ****************************************************************************
  # Downoad the results
  # ****************************************************************************
  # url <- encode_keyword(url)

  # VY. use the handler with proxy options.
  res <- curl::curl_fetch_memory(url, handle = .pkgenv[["cookie_handler"]])

  # Something went wrong
  if (res$status_code != 200) {
    stop("Status code was not 200. Returned status code:", res$status_code)
  }

  # ****************************************************************************
  # Format the results in a nice way
  # ****************************************************************************
  con <- textConnection(rawToChar(res$content))
  df <- read.csv(con, skip = 1, stringsAsFactors = FALSE, encoding = "UTF-8")
  close(con)

  if (nrow(df) < 1) {
    return(NULL) ## No data
  }

  # Redo the substitution of + for a nice data frame
  comparison_item$keyword <- sapply(comparison_item$keyword, function(x) gsub("\\+", " ", x))
  comparison_item$keyword <- sapply(comparison_item$keyword, function(x) gsub("%2B", "+", x))

  if (!onlyCategory) {
    # This conditional statment is necessary when no keyword is supplied but
    # a search for a category is called for


    if ((length(widget$request$comparisonItem[[2]]$time) == 1) |
      (length(unique(widget$request$comparisonItem[[2]]$time)) == 1) |
      (is.null(widget$request$comparisonItem[[2]]))
    ) {
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
        comparison_item[rep(seq_len(nrow(comparison_item)), each = n), 1:3],
        row.names = NULL
      )

      df$geo <- ifelse(df$geo == "", "world", df$geo)
      df$gprop <- ifelse(widget$request$requestOptions$property[1] == "", "web", widget$request$requestOptions$property[1])
      df$category <- widget$request$requestOptions$category[1]
      names(df)[1] <- "date"
      df$id <- NULL

      # Format the returned date
      if (all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", df$date))) {
        df$date <- as.POSIXct(df$date, format = "%Y-%m-%d", tz = paste0("GMT", ifelse(tz >= 0, "+", "-"), (abs(tz) / 60)), asUTC = T)
      } else if (all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}$", df$date))) {
        df$date <- as.POSIXct(df$date, format = "%Y-%m-%dT%H", tz = paste0("GMT", ifelse(tz >= 0, "+", "-"), (abs(tz) / 60)), asUTC = T)
      } else if (all(grepl("^[0-9]{4}-[0-9]{2}$", df$date))) {
        df$date <- df$date <- as.POSIXct(paste0(df$date, "-01"),
          format = "%Y-%m-%d", tz = paste0(
            "GMT", ifelse(tz >= 0, "+", "-"),
            (abs(tz) / 60)
          ), asUTC = T
        )
      } else {
        df$date <- gsub("^([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}).*$", "\\1", df$date)
        df$date <- as.POSIXct(df$date, format = "%Y-%m-%dT%H:%M:%S", tz = paste0("GMT", ifelse(tz >= 0, "+", "-"), (abs(tz) / 60)), asUTC = T)
      }
    } else {
      n <- nrow(df) # used to reshape the data
      kw <- payload2$comparisonItem$complexKeywordsRestriction[[1]][[1]]$value
      kw <- gsub("[[:blank:]-]", ".", kw)
      dates <- df[, which(!grepl(kw, names(df)))]

      if (all(sapply(lapply(dates, function(x) grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)), function(y) all(y)))) {
        dates <- data.frame(lapply(
          dates,
          function(x) {
            as.POSIXct(x,
              format = "%Y-%m-%d",
              tz = paste0("GMT", ifelse(tz >= 0, "+", "-"), (abs(tz) / 60))
            )
          }
        ))
      } else if (all(sapply(lapply(dates, function(x) grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}$", x)), function(y) all(y)))) {
        dates <- data.frame(lapply(
          dates,
          function(x) {
            as.POSIXct(x,
              format = "%Y-%m-%dT%H",
              tz = paste0("GMT", ifelse(tz >= 0, "+", "-"), (abs(tz) / 60))
            )
          }
        ))
      } else {
        dates <- data.frame(lapply(
          dates,
          function(x) {
            gsub("^([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}).*$", "\\1", x)
          }
        ))
        dates <- data.frame(lapply(
          dates,
          function(x) {
            as.POSIXct(x,
              format = "%Y-%m-%dT%H:%M:%S",
              tz = paste0("GMT", ifelse(tz >= 0, "+", "-"), (abs(tz) / 60))
            )
          }
        ))
      }

      # dates <- data.frame(lapply(dates,function(x) anytime::anytime(x,tz=paste0("GMT",ifelse(tz>=0,"+","-"),(abs(tz)/60)),asUTC=T)))

      hits <- df[, which(grepl(kw, names(df)))]

      for (jj in 1:NCOL(dates)) {
        df_tmp <- data.frame(dates[jj], hits[jj])
        df_tmp2 <- comparison_item[rep(jj, n), 1:3]

        df_tmp2[, 1] <- ifelse(df_tmp2[, 1] == "", "world", df_tmp2[, 1])
        df_tmp2[, 3] <- ifelse(widget$request$requestOptions$property[1] == "", "web", widget$request$requestOptions$property[1])
        df_tmp2[, 4] <- widget$request$requestOptions$category[1]
        if (jj == 1) {
          df_res <- cbind(df_tmp, df_tmp2)
          names(df_res) <- c("date", "hits", "geo", "time", "gprop", "category")
        } else {
          df_tmp3 <- cbind(df_tmp, df_tmp2)
          names(df_tmp3) <- c("date", "hits", "geo", "time", "gprop", "category")
          df_res <- rbind(df_res, df_tmp3)
        }
      }
      df <- df_res
    }
  } else {
    n <- nrow(df) # used to reshape the data
    names(df) <- c("date", "hits")

    if (all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", df$date))) {
      df$date <- as.POSIXct(df$date, format = "%Y-%m-%d", tz = paste0("GMT", ifelse(tz >= 0, "+", "-"), (abs(tz) / 60)), asUTC = T)
    } else if (all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}$", df$date))) {
      df$date <- as.POSIXct(df$date, format = "%Y-%m-%dT%H", tz = paste0("GMT", ifelse(tz >= 0, "+", "-"), (abs(tz) / 60)), asUTC = T)
    } else if (all(grepl("^[0-9]{4}-[0-9]{2}$", df$date))) {
      df$date <- df$date <- as.POSIXct(paste0(df$date, "-01"),
        format = "%Y-%m-%d", tz = paste0(
          "GMT", ifelse(tz >= 0, "+", "-"),
          (abs(tz) / 60)
        ), asUTC = T
      )
    }


    comparison_item[, "geo"] <- ifelse(comparison_item[, "geo"] == "", "world", comparison_item[, "geo"])
    comparison_item[, "gprop"] <- ifelse(widget$request$requestOptions$property[1] ==
      "", "web", widget$request$requestOptions$property[1])
    comparison_item[, "category"] <- widget$request$requestOptions$category[1]
    df <- cbind(df, comparison_item[rep(1, n), 2:5])
  }

  return(df)
}


interest_by_region <- function(widget, comparison_item, low_search_volume, compared_breakdown, tz) {
  i <- which(grepl("geo_map", widget$id, ignore.case = TRUE) == TRUE)

  if (length(i) == 0) {
    return(list(NULL))
  }

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
      MoreArgs = list(widget = widget, low_search_volume = low_search_volume, compared_breakdown = compared_breakdown, tz = tz),
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


create_geo_payload <- function(i, widget, resolution, compared_breakdown, low_search_volume, tz) {
  payload2 <- list()
  payload2$locale <- unique(na.omit(widget$request$locale))
  payload2$comparisonItem <- widget$request$comparisonItem[[i]]
  payload2$resolution <- resolution
  payload2$requestOptions$backend <- widget$request$requestOptions$backend[i]
  payload2$requestOptions$property <- widget$request$requestOptions$property[i]
  payload2$requestOptions$category <- widget$request$requestOptions$category[i]
  payload2$geo <- as.list((widget$request$geo[i, , drop = FALSE]))
  payload2$includeLowSearchVolumeGeos <- low_search_volume

  # If we want compared breakdown, it will return the relative hits per
  # region/city when multiple keywords are provided.

  if (compared_breakdown) {
    payload2$dataMode <- "PERCENTAGES"
  }


  url <- paste0(
    URLencode("https://www.google.com/trends/api/widgetdata/comparedgeo/csv?req="),
    URLencode(jsonlite::toJSON(payload2, auto_unbox = T, null = "list"), reserved = TRUE),
    URLencode(paste0("&token=", widget$token[i], "&tz=", tz, "&hl=en-US"))
  )

  res <- curl::curl_fetch_memory(url, handle = .pkgenv[["cookie_handler"]])

  if (res$status_code != 200) {
    return(NULL)
  }

  con <- textConnection(rawToChar(res$content))
  df <- read.csv(con, skip = 1, stringsAsFactors = FALSE, encoding = "UTF-8")
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
  if (length(widget$request$comparisonItem[[i]]$complexKeywordsRestriction$operator) == 0) {
    kw <- do.call(rbind, widget$request$comparisonItem[[i]]$complexKeywordsRestriction$keyword)
  } else {
    value <- paste(widget$request$comparisonItem[[i]]$complexKeywordsRestriction$keyword[[1]]$value, collapse = " + ")
    type <- unique(widget$request$comparisonItem[[i]]$complexKeywordsRestriction$keyword[[1]]$type)
    kw <- data.frame(type = type, value = value)
  }

  # kw <- do.call(rbind, widget$request$comparisonItem[[i]]$complexKeywordsRestriction$keyword)

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
  return(y[!sapply(y, function(x) {
    all(is.na(x))
  })])
}

## Replace special characters in keywords like P&500 -> P%26500
encode_keyword <- function(url) {
  url <- gsub("(?:\\G(?!^)|\\[\\s*)[^][\\s]*\\K\\&(?!])(?=[^][]*])", "%26", url, perl = TRUE)
  url <- gsub("(?:\\G(?!^)|\\[\\s*)[^][\\s]*\\K\\&(?!])(?=[^][]*])", "%26", url, perl = TRUE)
  url <- gsub("(?:\\G(?!^)|\\[\\s*)[^][\\s]*\\K\\&(?!])(?=[^][]*])", "%26", url, perl = TRUE)
  url <- gsub("(?:\\G(?!^)|\\[\\s*)[^][\\s]*\\K\\&(?!])(?=[^][]*])", "%26", url, perl = TRUE)
  gsub("(?:\\G(?!^)|\\[\\s*)[^][\\s]*\\K\\&(?!])(?=[^][]*])", "%26", url, perl = TRUE)
}


map_tz2min <- function(timezone) {
  round((unclass(as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"))) - unclass(as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = timezone)))) / 60)
}

encode_payload <- function(URL, reserved = FALSE, repeated = FALSE) {
  # This is a adjusted URLencode function.
  # The , and + are removed from the reserved characters list
  if (!repeated && grepl("%[[:xdigit:]]{2}", URL, useBytes = TRUE)) {
    return(URL)
  }
  OK <- paste0(
    "[^", if (!reserved) {
      "][!'()*;=/?@#"
    }, "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz0123456789._~,:%+-",
    "]"
  )
  x <- strsplit(URL, "")[[1L]]
  z <- grep(OK, x)
  if (length(z)) {
    y <- vapply(x[z], function(x) {
      paste0("%", toupper(as.character(charToRaw(x))),
        collapse = ""
      )
    }, "")
    x[z] <- y
  }
  paste(x, collapse = "")
}

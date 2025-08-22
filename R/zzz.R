# create environment in which to put cookie_handler
.pkgenv <- new.env(parent = emptyenv())

# alternative url is "http://apis.google.com/Cookies/OTZ"
# function to create cookie_handler, which is necessary to run get_widget()
get_api_cookies <- function(cookie_url) {
  # create new handler
  cookie_handler <- curl::new_handle()

  # set options for the proxy
  proxy_domain <- ifelse(
    is.null(.pkgenv[["handle_domain"]]),
    "",
    paste0(.pkgenv[["handle_domain"]], "\\")
  )
  proxy_user_pwd <- paste0(
    proxy_domain,
    .pkgenv[["handle_user"]],
    ":",
    .pkgenv[["handle_password"]]
  )
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
  if (!is.character(time_ranges)) {
    stop("Time parameter must be character vector.", call. = FALSE)
  }

  fixed_formats <- c(
    "now 1-H", # last hour
    "now 4-H", # last four hours
    "now 1-d", # last day
    "now 7-d", # last seven days
    "today 1-m", # past 30 days
    "today 3-m", # past 90 days
    "today 12-m", # past 12 months
    "today+5-y", # last 5 years (default)
    "all" # since beginning of Google Trends (2004)
  )

  for (tr in time_ranges) {
    # Check if it's a preset format
    if (tr %in% fixed_formats) {
      next
    }

    # Parse custom time range
    time_parts <- unlist(strsplit(tr, " ", fixed = TRUE))

    if (length(time_parts) != 2L) {
      stop(
        "Custom time range must have format 'start_date end_date'.\n",
        "Invalid format: '",
        tr,
        "'",
        call. = FALSE
      )
    }

    # Parse start and end dates
    tryCatch(
      {
        if (!grepl("T", time_parts[1L], fixed = TRUE)) {
          start_date <- as.POSIXct(
            format(anytime::anydate(time_parts[1L], tz = "UTC"), tz = "UTC"),
            tz = "UTC"
          )
          end_date <- as.POSIXct(
            format(anytime::anydate(time_parts[2L], tz = "UTC"), tz = "UTC"),
            tz = "UTC"
          )
        } else {
          start_date <- anytime::anytime(time_parts[1L])
          end_date <- anytime::anytime(time_parts[2L])
        }
      },
      error = function(e) {
        stop(
          "Could not parse dates in time range: '",
          tr,
          "'.\n",
          "Use format like '2020-01-01 2020-12-31' or '2020-01-01T01 2020-01-01T23'",
          call. = FALSE
        )
      }
    )

    # Validate parsed dates
    if (is.na(start_date) || is.na(end_date)) {
      stop(
        "Invalid date(s) in time range: '",
        tr,
        "'.\n",
        "Check date format and ensure dates are valid.",
        call. = FALSE
      )
    }

    if (start_date >= end_date) {
      stop(
        "Start date must be before end date in time range: '",
        tr,
        "'.\n",
        "Start: ",
        time_parts[1L],
        ", End: ",
        time_parts[2L],
        call. = FALSE
      )
    }

    if (start_date < as.POSIXct("2004-01-01", tz = "UTC")) {
      stop(
        "Start date cannot be before 2004-01-01 (Google Trends launch).\n",
        "Invalid start date: ",
        time_parts[1L],
        call. = FALSE
      )
    }

    if (end_date > as.POSIXct(Sys.time())) {
      stop(
        "End date cannot be in the future.\n",
        "Invalid end date: ",
        time_parts[2L],
        call. = FALSE
      )
    }
  }

  TRUE
}

get_widget <- function(comparison_item, category, gprop, hl, cookie_url, tz) {
  # Initialize cookies if needed
  if (!exists("cookie_handler", envir = .pkgenv)) {
    tryCatch(
      {
        get_api_cookies(cookie_url)
      },
      error = function(e) {
        stop(
          "Failed to initialize Google Trends session during cookie acquisition:\n",
          "Could not obtain required authentication cookies from Google.\n",
          "\nPossible causes:\n",
          "  - Network connectivity issues\n",
          "  - Proxy server configuration problems\n",
          "  - Firewall blocking Google Trends access\n",
          "  - Invalid cookie URL: ",
          cookie_url,
          "\n",
          "\nOriginal error: ",
          e$message,
          call. = FALSE
        )
      }
    )
  }

  # Build API URL
  url <- build_explore_url(comparison_item, category, gprop, hl, tz)

  # Make API request
  response <- make_api_request(url, "widget initialization")

  # Parse response
  parsed_response <- parse_api_response(response)

  # Fix geographic encoding issues
  parsed_response <- fix_geo_encoding(parsed_response, comparison_item)

  return(parsed_response$widgets)
}

interest_over_time <- function(widget, comparison_item, tz) {
  # Build appropriate URL based on widget type
  url <- build_interest_over_time_url(widget, tz)

  # Fetch data from Google Trends API
  response <- fetch_trends_data(url)

  process_interest_over_time_response(response, comparison_item, widget, tz)
}

# Helper function to build URL based on widget configuration
build_interest_over_time_url <- function(widget, tz) {
  payload2 <- list()
  payload2$userConfig$userType <- widget$request$userConfig$userType[[1L]]

  # Determine request type and create appropriate payload
  onlyCategory <- is_category_only_search(widget)

  if (onlyCategory) {
    # Handle category-only searches
    payload_data <- create_category_payload(widget)
    return(
      build_widget_url(
        "multiline",
        c(payload2, payload_data$payload),
        payload_data$token,
        tz
      )
    )
  } else if (has_multiple_timeframes(widget)) {
    # Handle multi-timeframe searches
    payload_data <- create_multirange_payload(widget)
    return(build_widget_url(
      "multirange",
      c(payload2, payload_data$payload),
      payload_data$token,
      tz
    ))
  } else {
    # Handle standard multiline searches
    payload_data <- create_multiline_payload(widget)
    return(build_widget_url(
      "multiline",
      c(payload2, payload_data$payload),
      payload_data$token,
      tz
    ))
  }
}

# Helper function to fetch data with error handling
fetch_trends_data <- function(url) {
  make_api_request(url, "trends data download")
}

# Helper function to process API response
process_interest_over_time_response <- function(
  response,
  comparison_item,
  widget,
  tz
) {
  # Parse CSV response
  con <- textConnection(rawToChar(response$content))
  df <- read.csv(con, skip = 1L, stringsAsFactors = FALSE, encoding = "UTF-8")
  close(con)

  if (nrow(df) < 1L) {
    return(NULL) # No data available
  }

  # Clean keywords
  comparison_item <- clean_keywords(comparison_item)

  # Process data based on request type
  onlyCategory <- is_category_only_search(widget)

  if (onlyCategory) {
    return(process_category_data(df, comparison_item, widget, tz))
  } else if (has_multiple_timeframes(widget)) {
    return(process_multirange_data(df, comparison_item, widget, tz))
  } else {
    return(reshape_interest_over_time(df, comparison_item, widget, tz))
  }
}


interest_by_region <- function(
  widget,
  comparison_item,
  low_search_volume,
  compared_breakdown,
  tz
) {
  i <- which(grepl("geo_map", widget$id, ignore.case = TRUE) == TRUE)

  if (length(i) == 0L) {
    return(list(NULL))
  }

  resolution <-
    expand.grid(
      i,
      c(
        ifelse(
          grepl("world", na.omit(widget$geo), fixed = TRUE),
          "country",
          "region"
        ),
        "city",
        "dma"
      ),
      stringsAsFactors = FALSE
    )

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
      MoreArgs = list(
        widget = widget,
        low_search_volume = low_search_volume,
        compared_breakdown = compared_breakdown,
        tz = tz
      ),
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


create_geo_payload <- function(
  i,
  widget,
  resolution,
  compared_breakdown,
  low_search_volume,
  tz
) {
  payload2 <- list()
  payload2$locale <- unique(na.omit(widget$request$locale))
  payload2$comparisonItem <- widget$request$comparisonItem[[i]]
  payload2$resolution <- resolution
  payload2$requestOptions$backend <- widget$request$requestOptions$backend[i]
  payload2$requestOptions$property <- widget$request$requestOptions$property[i]
  payload2$requestOptions$category <- widget$request$requestOptions$category[i]
  payload2$userConfig$userType <- widget$request$userConfig$userType[[i]]
  payload2$geo <- as.list((widget$request$geo[i, , drop = FALSE]))
  payload2$includeLowSearchVolumeGeos <- low_search_volume

  # If we want compared breakdown, it will return the relative hits per
  # region/city when multiple keywords are provided.

  if (compared_breakdown) {
    payload2$dataMode <- "PERCENTAGES"
  }

  url <- build_widget_url(
    "comparedgeo",
    payload2,
    widget$token[i],
    tz,
    extra_params = list(hl = "en-US")
  )

  res <- tryCatch(
    {
      make_api_request(url, "geo data download")
    },
    error = function(e) {
      return(NULL)
    }
  )

  con <- textConnection(rawToChar(res$content))
  df <- read.csv(con, skip = 1L, stringsAsFactors = FALSE, encoding = "UTF-8")
  close(con)

  if (nrow(df) == 0L) {
    return(NULL)
  }

  n <- nrow(df) # used to reshape the data

  df <- reshape(
    df,
    varying = names(df)[2L:ncol(df)],
    v.names = "hits",
    direction = "long",
    timevar = "temp",
    times = names(df)[2L:ncol(df)]
  )
  if (
    length(
      widget$request$comparisonItem[[i]]$complexKeywordsRestriction$operator
    ) ==
      0L
  ) {
    kw <- do.call(
      rbind,
      widget$request$comparisonItem[[i]]$complexKeywordsRestriction$keyword
    )
  } else {
    value <- paste(
      widget$request$comparisonItem[[i]]$complexKeywordsRestriction$keyword[[
        1L
      ]]$value,
      collapse = " + "
    )
    type <- unique(
      widget$request$comparisonItem[[i]]$complexKeywordsRestriction$keyword[[
        1L
      ]]$type
    )
    kw <- data.frame(type = type, value = value)
  }

  # kw <- do.call(rbind, widget$request$comparisonItem[[i]]$complexKeywordsRestriction$keyword)

  df <- cbind(
    df,
    kw[rep(seq_len(nrow(kw)), each = n), 2L],
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  df$temp <- NULL
  # df$geo <- widget$geo[i]
  df$geo <- suppressWarnings(na.omit(unlist(widget$request$geo[i, ])))

  df$geo <- ifelse(is.null(df$geo), "world", df$geo)
  df$gprop <- ifelse(
    !nzchar(widget$request$requestOptions$property[i]),
    "web",
    widget$request$requestOptions$property[i]
  )

  df$id <- NULL
  rownames(df) <- NULL

  names(df) <- c("location", "hits", "keyword", "geo", "gprop")

  return(df)
}

## Replace special characters in keywords like P&500 -> P%26500
encode_keyword <- function(url) {
  url <- gsub(
    "(?:\\G(?!^)|\\[\\s*)[^][\\s]*\\K\\&(?!])(?=[^][]*])",
    "%26",
    url,
    perl = TRUE
  )
  url <- gsub(
    "(?:\\G(?!^)|\\[\\s*)[^][\\s]*\\K\\&(?!])(?=[^][]*])",
    "%26",
    url,
    perl = TRUE
  )
  url <- gsub(
    "(?:\\G(?!^)|\\[\\s*)[^][\\s]*\\K\\&(?!])(?=[^][]*])",
    "%26",
    url,
    perl = TRUE
  )
  url <- gsub(
    "(?:\\G(?!^)|\\[\\s*)[^][\\s]*\\K\\&(?!])(?=[^][]*])",
    "%26",
    url,
    perl = TRUE
  )
  gsub(
    "(?:\\G(?!^)|\\[\\s*)[^][\\s]*\\K\\&(?!])(?=[^][]*])",
    "%26",
    url,
    perl = TRUE
  )
}


map_tz2min <- function(timezone) {
  round(
    (unclass(as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"))) -
      unclass(as.POSIXct(format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S",
        tz = timezone
      )))) /
      60L
  )
}

encode_payload <- function(URL, reserved = FALSE, repeated = FALSE) {
  # This is a adjusted URLencode function.
  # The , and + are removed from the reserved characters list
  if (!repeated && grepl("%[[:xdigit:]]{2}", URL, useBytes = TRUE)) {
    return(URL)
  }
  OK <- paste0(
    "[^",
    if (!reserved) {
      "][!'()*;=/?@#"
    },
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    "abcdefghijklmnopqrstuvwxyz0123456789._~,:%+-",
    "]"
  )
  x <- strsplit(URL, "", fixed = TRUE)[[1L]]
  z <- grep(OK, x)
  if (length(z)) {
    y <- vapply(
      x[z],
      function(x) {
        paste0("%", toupper(as.character(charToRaw(x))), collapse = "")
      },
      ""
    )
    x[z] <- y
  }
  paste(x, collapse = "")
}


#' Interest by region with error handling
#' @noRd
get_interest_by_region <- function(
  widget,
  comparison_item,
  low_search_volume,
  compared_breakdown,
  tz
) {
  tryCatch(
    {
      region_results <- interest_by_region(
        widget,
        comparison_item,
        low_search_volume,
        compared_breakdown,
        tz
      )
      return(combine_region_results(region_results))
    },
    error = function(e) {
      # Return empty structure rather than failing completely
      warning(
        "Could not retrieve regional interest data: ",
        e$message,
        call. = FALSE
      )
      return(list(country = NULL, region = NULL, dma = NULL, city = NULL))
    }
  )
}

#' Related topics with error handling
#' @noRd
get_related_topics <- function(widget, comparison_item, hl, tz) {
  tryCatch(
    {
      return(related_topics(widget, comparison_item, hl, tz))
    },
    error = function(e) {
      warning(
        "Could not retrieve related topics: ",
        e$message,
        call. = FALSE
      )
      return(NULL)
    }
  )
}

#' Related queries with error handling
#' @noRd
get_related_queries <- function(widget, comparison_item, tz, hl) {
  tryCatch(
    {
      return(related_queries(widget, comparison_item, tz, hl))
    },
    error = function(e) {
      warning(
        "Could not retrieve related queries: ",
        e$message,
        call. = FALSE
      )
      return(NULL)
    }
  )
}

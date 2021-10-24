related_queries <- function(widget, comparison_item, tz, hl) {
  i <- which(grepl("related_queries", widget$id, ignore.case = TRUE) == TRUE)

  res <- lapply(i, create_related_queries_payload, widget = widget, tz = tz, hl = hl)
  res <- do.call(rbind, res)

  return(res)
}


create_related_queries_payload <- function(i, widget, tz, hl) {
  payload2 <- list()
  payload2$restriction$geo <- as.list(widget$request$restriction$geo[i, , drop = FALSE])
  payload2$restriction$time <- widget$request$restriction$time[[i]]
  payload2$restriction$originalTimeRangeForExploreUrl <- widget$request$restriction$originalTimeRangeForExploreUrl[[i]]
  payload2$restriction$complexKeywordsRestriction$keyword <- widget$request$restriction$complexKeywordsRestriction$keyword[[i]]
  payload2$restriction$complexKeywordsRestriction$operator <- widget$request$restriction$complexKeywordsRestriction$operator[[i]]
  payload2$keywordType <- widget$request$keywordType[[i]]
  payload2$metric <- widget$request$metric[[i]]
  payload2$trendinessSettings$compareTime <- widget$request$trendinessSettings$compareTime[[i]]
  payload2$requestOptions$property <- widget$request$requestOptions$property[[i]]
  payload2$requestOptions$backend <- widget$request$requestOptions$backend[[i]]
  payload2$requestOptions$category <- widget$request$requestOptions$category[[i]]
  payload2$language <- widget$request$language[[i]]
  payload2$userCountryCode <- widget$request$userCountryCode[[i]]

  url <- paste0(
    URLencode("https://www.google.com/trends/api/widgetdata/relatedsearches/csv?req="),
    URLencode(paste0(jsonlite::toJSON(payload2, auto_unbox = TRUE)), reserved = TRUE),
    URLencode(paste0("&token=", widget$token[i])),
    URLencode(paste0("&tz=", tz, "&hl=", hl))
  )

  # url <- encode_keyword(url)
  # VY. use the handler with proxy options.
  res <- curl::curl_fetch_memory(URLencode(url), handle = .pkgenv[["cookie_handler"]])

  # Something went wrong
  if (res$status_code != 200) {
    stop("Status code was not 200. Returned status code:", res$status_code)
  }

  res <- readLines(textConnection(rawToChar(res$content)))

  ## Not enough data
  ## https://trends.google.ca/trends/explore?cat=20&date=today%205-y,today%205-y&geo=CA,US&q=NHL,NFL
  if (length(res) <= 4) {
    return(NULL)
  }

  start_top <- which(grepl("TOP", res))
  start_rising <- which(grepl("RISING", res))

  if (length(start_top) == 0 | length(start_rising) == 0) {
    return(NULL) ## No data returned
  }

  top <- read.csv(textConnection(res[start_top:(start_rising - 2)]),
    row.names = NULL, encoding = "UTF-8"
  )
  top$subject <- rownames(top)
  rownames(top) <- NULL
  top <- top[, c(2, 1)]
  names(top) <- c("subject", "top")

  top <- reshape(
    top,
    varying = "top",
    v.names = "value",
    direction = "long",
    timevar = "related_queries",
    times = "top"
  )

  rising <- read.csv(textConnection(res[start_rising:length(res)]),
    row.names = NULL, encoding = "UTF-8"
  )
  rising$subject <- rownames(rising)
  rownames(rising) <- NULL
  rising <- rising[, c(2, 1)]
  names(rising) <- c("subject", "rising")

  rising <- reshape(
    rising,
    varying = "rising",
    v.names = "value",
    direction = "long",
    timevar = "related_queries",
    times = "rising"
  )

  res <- rbind(top, rising)
  res$id <- NULL
  res$geo <- unlist(payload2$restriction$geo, use.names = FALSE)
  if (length(widget$request$restriction$complexKeywordsRestriction$operator) != 0) {
    if (is.na(widget$request$restriction$complexKeywordsRestriction$operator[[i]])) {
      res$keyword <- widget$request$restriction$complexKeywordsRestriction$keyword[[i]]$value
    } else {
      res$keyword <- paste(widget$request$restriction$complexKeywordsRestriction$keyword[[i]]$value, collapse = "+")
    }
  } else {
    res$keyword <- widget$request$restriction$complexKeywordsRestriction$keyword[[i]]$value
  }


  res$category <- payload2$requestOptions$category

  return(res)
}

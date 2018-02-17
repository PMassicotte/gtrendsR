related_queries <- function(widget, comparison_item) {
  i <- which(grepl("queries", widget$title) == TRUE)

  res <- lapply(i, create_related_queries_payload, widget = widget)
  res <- do.call(rbind, res)

  return(res)
}


create_related_queries_payload <- function(i, widget) {
  payload2 <- list()
  payload2$restriction$geo <- as.list(widget$request$restriction$geo[i, , drop = FALSE])
  payload2$restriction$time <- widget$request$restriction$time[[i]]
  payload2$restriction$originalTimeRangeForExploreUrl <- widget$request$restriction$originalTimeRangeForExploreUrl[[i]]
  payload2$restriction$complexKeywordsRestriction$keyword <- widget$request$restriction$complexKeywordsRestriction$keyword[[i]]
  payload2$keywordType <- widget$request$keywordType[[i]]
  payload2$metric <- widget$request$metric[[i]]
  payload2$trendinessSettings$compareTime <- widget$request$trendinessSettings$compareTime[[i]]
  payload2$requestOptions$property <- widget$request$requestOptions$property[[i]]
  payload2$requestOptions$backend <- widget$request$requestOptions$backend[[i]]
  payload2$requestOptions$category <- widget$request$requestOptions$category[[i]]
  payload2$language <- widget$request$language[[i]]

  url <- paste0(
    "https://www.google.com/trends/api/widgetdata/relatedsearches/csv?req=",
    jsonlite::toJSON(payload2, auto_unbox = T),
    "&token=", widget$token[i],
    "&tz=300&hl=en-US"
  )


  res <- curl::curl_fetch_memory(URLencode(url))

  stopifnot(res$status_code == 200)

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

  top <- read.csv(textConnection(res[start_top:(start_rising - 2)]), row.names = NULL)
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

  rising <- read.csv(textConnection(res[start_rising:length(res)]), row.names = NULL)
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
  res$keyword <- payload2$restriction$complexKeywordsRestriction$keyword$value
  res$category <- payload2$requestOptions$category

  return(res)
}

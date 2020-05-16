related_topics <- function(widget, comparison_item, hl,tz) {
  i <- which(grepl("topics", widget$title) == TRUE)

  res <- lapply(i, create_related_topics_payload, widget = widget, hl = hl,tz=tz)
  res <- do.call(rbind, res)

  return(res)
}


create_related_topics_payload <- function(i, widget, hl, tz) {
  payload2 <- list()
  payload2$restriction$geo <-
    as.list(widget$request$restriction$geo[i, , drop = FALSE])
  payload2$restriction$time <- widget$request$restriction$time[[i]]
  payload2$restriction$originalTimeRangeForExploreUrl <-
    widget$request$restriction$originalTimeRangeForExploreUrl[[i]]
  payload2$restriction$complexKeywordsRestriction$keyword <-
    widget$request$restriction$complexKeywordsRestriction$keyword[[i]]
  payload2$restriction$complexKeywordsRestriction$operator <-
    widget$request$restriction$complexKeywordsRestriction$operator[[i]]
  payload2$keywordType <- widget$request$keywordType[[i]]
  payload2$metric <- widget$request$metric[[i]]
  payload2$trendinessSettings$compareTime <-
    widget$request$trendinessSettings$compareTime[[i]]
  payload2$requestOptions$property <-
    widget$request$requestOptions$property[[i]]
  payload2$requestOptions$backend <-
    widget$request$requestOptions$backend[[i]]
  payload2$requestOptions$category <-
    widget$request$requestOptions$category[[i]]
  payload2$language <- widget$request$language[[i]]
  payload2$userCountryCode <- widget$request$userCountryCode[[i]]

  url <- paste0(
    URLencode(
      "https://www.google.com/trends/api/widgetdata/relatedsearches/csv?req="
    ),
    URLencode(paste0(
      jsonlite::toJSON(payload2, auto_unbox = TRUE)
    ), reserved = TRUE),
    URLencode(paste0("&token=", widget$token[i])),
    URLencode(paste0("&tz=", tz, "&hl=", hl))
  )

  res <- curl::curl_fetch_memory(url, handle = .pkgenv[["cookie_handler"]])

  # Something went wrong
  if (res$status_code != 200) {
    stop(
      "Status code was not 200. Returned status code:",
      res$status_code
    )
  }

  raw_data <- readLines(textConnection(rawToChar(res$content)))
  
  # Extract TOP and RISING data
  keywords <- c("TOP", "RISING")
  
  # Return NULL if the expected keywords are not found
  if (length(keywords) == 0) {
    return(NULL)
  }
  
  index <- which(raw_data %in% keywords)
  res <- lapply(index, extract_related_topics, raw_data = raw_data)
  res <- do.call(rbind, res)
  res$id <- NULL
  res$geo <- unlist(payload2$restriction$geo, use.names = FALSE)
  
  if (length(widget$request$restriction$complexKeywordsRestriction$operator) !=
    0) {
    if (is.na(widget$request$restriction$complexKeywordsRestriction$operator[[i]])) {
      res$keyword <-
        widget$request$restriction$complexKeywordsRestriction$keyword[[i]]$value
    } else {
      res$keyword <-
        paste(
          widget$request$restriction$complexKeywordsRestriction$keyword[[i]]$value,
          collapse = "+"
        )
    }
  } else {
    res$keyword <-
      widget$request$restriction$complexKeywordsRestriction$keyword[[i]]$value
  }
  res$category <- payload2$requestOptions$category

  return(res)
}

extract_related_topics <- function(i, raw_data) {
  
  n <- length(raw_data)
  end <- min(which(raw_data[i:n] == ""))
  
  df <- read.csv(textConnection(raw_data[i:end]), row.names = NULL)
  df$subject <- rownames(df)
  rownames(df) <- NULL
  df <- df[, c(2, 1)]
  names(df) <- c("subject", tolower(colnames(df)[1]))
  
  df <- reshape(
    df,
    varying = tolower(colnames(df)[2]),
    v.names = "value",
    direction = "long",
    timevar = "related_topics",
    times = tolower(colnames(df)[2])
  )
  
}

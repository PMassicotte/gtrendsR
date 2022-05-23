related_topics <- function(widget, comparison_item, hl, tz) {
  i <- which(grepl("related_topics", widget$id, ignore.case = TRUE) == TRUE)

  res <- lapply(i, create_related_topics_payload, widget = widget, hl = hl, tz = tz)
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
  payload2$userConfig$userType <- widget$request$userConfig$userType[[i]]

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

  res <- readLines(textConnection(rawToChar(res$content)))

  start_top <- which(grepl("TOP", res))[1]
  start_rising <- which(grepl("RISING", res))[1]

  if (length(start_top) == 0 & length(start_rising) == 0) {
    return(NULL) ## No data returned
  }

  new_res <- NULL

  if (length(start_top) > 0) {
    end_top <- ifelse(length(start_rising) == 0, length(res), start_rising - 2)
    top <- read.csv(textConnection(res[start_top:end_top]),
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
      timevar = "related_topics",
      times = "top"
    )

    new_res <- rbind(new_res, top)
  }

  if (length(start_rising) > 0) {
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
      timevar = "related_topics",
      times = "rising"
    )

    new_res <- rbind(new_res, rising)
  }

  res <- new_res
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
  end <- i + min(which(raw_data[i:n] == "")) - 1

  df <- read.csv(textConnection(raw_data[i:end]), row.names = NULL, encoding = "UTF-8")
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

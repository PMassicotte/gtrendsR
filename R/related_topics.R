related_topics <- function(widget, comparison_item, hl, tz) {
  i <- which(grepl("related_topics", widget$id, ignore.case = TRUE) == TRUE)

  res <- lapply(
    i,
    fetch_related_topics_data,
    widget = widget,
    hl = hl,
    tz = tz
  )
  res <- do.call(rbind, res)

  res
}


create_related_topics_payload <- function(i, widget, hl, tz) {
  payload <- list()
  payload$restriction$geo <-
    as.list(widget$request$restriction$geo[i, , drop = FALSE])
  payload$restriction$time <- widget$request$restriction$time[[i]]
  payload$restriction$originalTimeRangeForExploreUrl <-
    widget$request$restriction$originalTimeRangeForExploreUrl[[i]]
  payload$restriction$complexKeywordsRestriction$keyword <-
    widget$request$restriction$complexKeywordsRestriction$keyword[[i]]
  payload$restriction$complexKeywordsRestriction$operator <-
    widget$request$restriction$complexKeywordsRestriction$operator[[i]]
  payload$keywordType <- widget$request$keywordType[[i]]
  payload$metric <- widget$request$metric[[i]]
  payload$trendinessSettings$compareTime <-
    widget$request$trendinessSettings$compareTime[[i]]
  payload$requestOptions$property <-
    widget$request$requestOptions$property[[i]]
  payload$requestOptions$backend <-
    widget$request$requestOptions$backend[[i]]
  payload$requestOptions$category <-
    widget$request$requestOptions$category[[i]]
  payload$language <- widget$request$language[[i]]
  payload$userCountryCode <- widget$request$userCountryCode[[i]]
  payload$userConfig$userType <- widget$request$userConfig$userType[[i]]

  payload
}

fetch_related_topics_data <- function(i, widget, hl, tz) {
  payload <- create_related_topics_payload(i, widget, hl, tz)

  url <- paste0(
    URLencode(
      "https://www.google.com/trends/api/widgetdata/relatedsearches/csv?req="
    ),
    URLencode(
      paste0(
        jsonlite::toJSON(payload, auto_unbox = TRUE)
      ),
      reserved = TRUE
    ),
    URLencode(paste0("&token=", widget$token[i])),
    URLencode(paste0("&tz=", tz, "&hl=", hl))
  )

  res <- curl::curl_fetch_memory(url, handle = .pkgenv[["cookie_handler"]])

  # Something went wrong
  if (res$status_code != 200L) {
    stop(
      "Status code was not 200. Returned status code:",
      res$status_code
    )
  }

  res <- readLines(textConnection(rawToChar(res$content)))

  # Extract top and rising data if it exists.
  res <- extract_top_rising(res)

  res$id <- NULL
  res$geo <- unlist(payload$restriction$geo, use.names = FALSE)

  if (
    length(widget$request$restriction$complexKeywordsRestriction$operator) != 0L
  ) {
    if (
      is.na(widget$request$restriction$complexKeywordsRestriction$operator[[i]])
    ) {
      res$keyword <-
        widget$request$restriction$complexKeywordsRestriction$keyword[[i]]$value
    } else {
      res$keyword <-
        paste(
          widget$request$restriction$complexKeywordsRestriction$keyword[[
            i
          ]]$value,
          collapse = "+"
        )
    }
  } else {
    res$keyword <-
      widget$request$restriction$complexKeywordsRestriction$keyword[[i]]$value
  }
  res$category <- payload$requestOptions$category

  return(res)
}

extract_related_topics <- function(i, raw_data) {
  n <- length(raw_data)
  end <- i + min(which(raw_data[i:n] == "")) - 1

  df <- read.csv(
    textConnection(raw_data[i:end]),
    row.names = NULL,
    encoding = "UTF-8"
  )
  df$subject <- rownames(df)
  rownames(df) <- NULL
  df <- df[, c(2L, 1L)]
  names(df) <- c("subject", tolower(colnames(df)[1L]))

  df <- reshape(
    df,
    varying = tolower(colnames(df)[2L]),
    v.names = "value",
    direction = "long",
    timevar = "related_topics",
    times = tolower(colnames(df)[2L])
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

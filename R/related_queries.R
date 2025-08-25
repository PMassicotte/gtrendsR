related_queries <- function(widget, comparison_item, tz, hl) {
  i <- which(grepl("related_queries", widget$id, ignore.case = TRUE) == TRUE)

  res <- lapply(
    i,
    fetch_related_queries_data,
    widget = widget,
    tz = tz,
    hl = hl
  )
  res <- do.call(rbind, res)

  res
}


create_related_queries_payload <- function(i, widget, tz, hl) {
  payload <- list()
  payload$restriction$geo <- as.list(widget$request$restriction$geo[
    i,
    ,
    drop = FALSE
  ])
  payload$restriction$time <- widget$request$restriction$time[[i]]
  payload$restriction$originalTimeRangeForExploreUrl <- widget$request$restriction$originalTimeRangeForExploreUrl[[
    i
  ]]
  payload$restriction$complexKeywordsRestriction$keyword <- widget$request$restriction$complexKeywordsRestriction$keyword[[
    i
  ]]
  payload$restriction$complexKeywordsRestriction$operator <- widget$request$restriction$complexKeywordsRestriction$operator[[
    i
  ]]
  payload$keywordType <- widget$request$keywordType[[i]]
  payload$metric <- widget$request$metric[[i]]
  payload$trendinessSettings$compareTime <- widget$request$trendinessSettings$compareTime[[
    i
  ]]
  payload$requestOptions$property <- widget$request$requestOptions$property[[
    i
  ]]
  payload$requestOptions$backend <- widget$request$requestOptions$backend[[i]]
  payload$requestOptions$category <- widget$request$requestOptions$category[[
    i
  ]]
  payload$language <- widget$request$language[[i]]
  payload$userCountryCode <- widget$request$userCountryCode[[i]]
  payload$userConfig$userType <- widget$request$userConfig$userType[[i]]

  payload
}

fetch_related_queries_data <- function(i, widget, tz, hl) {
  payload <- create_related_queries_payload(i, widget, tz, hl)

  related_queries_url <- paste0(
    URLencode(
      "https://www.google.com/trends/api/widgetdata/relatedsearches/csv?req="
    ),
    URLencode(
      paste0(jsonlite::toJSON(payload, auto_unbox = TRUE)),
      reserved = TRUE
    ),
    URLencode(paste0("&token=", widget$token[i])),
    URLencode(paste0("&tz=", tz, "&hl=", hl))
  )

  # VY. use the handler with proxy options.
  res <- curl::curl_fetch_memory(
    URLencode(related_queries_url),
    handle = .pkgenv[["cookie_handler"]]
  )

  # Something went wrong
  if (res$status_code != 200L) {
    stop("Status code was not 200. Returned status code:", res$status_code)
  }

  res <- readLines(textConnection(rawToChar(res$content)))

  ## Not enough data
  ## https://trends.google.ca/trends/explore?cat=20&date=today%205-y,today%205-y&geo=CA,US&q=NHL,NFL
  if (length(res) <= 4L) {
    return(NULL)
  }

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
      res$keyword <- widget$request$restriction$complexKeywordsRestriction$keyword[[
        i
      ]]$value
    } else {
      res$keyword <- paste(
        widget$request$restriction$complexKeywordsRestriction$keyword[[
          i
        ]]$value,
        collapse = "+"
      )
    }
  } else {
    res$keyword <- widget$request$restriction$complexKeywordsRestriction$keyword[[
      i
    ]]$value
  }

  res$category <- payload$requestOptions$category

  res
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
      NULL
    }
  )
}

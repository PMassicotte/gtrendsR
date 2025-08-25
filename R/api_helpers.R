#' API interaction helper functions for gtrends package
#'
#' @description
#' These functions handle URL construction, payload creation, and API
#' communication with consistent error handling and informative messages.
#'
#' @param keyword Character vector of keywords
#' @return Character vector of URL-encoded keywords
#' @noRd
prepare_keywords <- function(keyword) {
  prepared <- sapply(
    keyword,
    function(x) {
      y <- gsub("[+]", "%2B", x, fixed = TRUE)
      z <- gsub(" ", "+", y, fixed = TRUE)
      z
    },
    USE.NAMES = FALSE
  )

  prepared
}

#' Create comparison item data frame for API requests
#'
#' @param keyword Character vector of prepared keywords
#' @param geo Character vector of geographic regions
#' @param time Character vector of time specifications
#' @return Data frame with comparison item structure
#' @noRd
create_comparison_item <- function(keyword, geo, time) {
  data.frame(
    keyword = keyword,
    geo = geo,
    time = time,
    stringsAsFactors = FALSE
  )
}

#' Build explore API URL with proper encoding
#'
#' @param comparison_item Data frame with query parameters
#' @param category Numeric category ID
#' @param gprop Character string for Google property
#' @param hl Character language code
#' @param tz Numeric timezone offset in minutes
#' @return Character string with complete API URL
#' @noRd
build_explore_url <- function(comparison_item, category, gprop, hl, tz) {
  token_payload <- list(
    comparisonItem = comparison_item,
    category = category,
    property = gprop
  )

  base_url <- paste0(
    "https://trends.google.com/trends/api/explore?hl=",
    hl,
    "&tz=",
    tz,
    "&req="
  )

  encoded_payload <- encode_payload(
    jsonlite::toJSON(token_payload, auto_unbox = TRUE),
    reserved = TRUE,
    repeated = TRUE
  )

  final_url <- paste0(
    URLencode(base_url),
    encoded_payload,
    URLencode(paste0("&tz=", tz))
  )

  final_url
}

#' Make API request with error handling
#'
#' @param url Character string with API URL
#' @param operation Character string describing the operation (for error
#' messages)
#'
#' @return List with response content and metadata
#' @noRd
make_api_request <- function(url, operation = "API request") {
  # Ensure cookie handler exists
  if (!exists("cookie_handler", envir = .pkgenv)) {
    stop(
      "Cookie handler not initialized. This is an internal error.\n",
      "Please report this issue to the package maintainers.",
      call. = FALSE
    )
  }

  tryCatch(
    {
      # cat(operation, "\n", url, "\n\n")
      response <- curl::curl_fetch_memory(
        url,
        handle = .pkgenv[["cookie_handler"]]
      )

      # Check HTTP status
      if (response$status_code != 200L) {
        handle_http_error(response$status_code, operation)
      }

      return(response)
    },
    error = function(e) {
      if (grepl("status_code", e$message, fixed = TRUE)) {
        stop(e$message, call. = FALSE)
      }

      # Network or other curl errors
      stop(
        "Network error during ",
        operation,
        ":\n",
        e$message,
        "\n",
        "Check your internet connection and try again.",
        call. = FALSE
      )
    }
  )
}

#' Handle HTTP error responses with informative messages
#'
#' @param status_code Numeric HTTP status code
#' @param operation Character string describing the operation
#' @noRd
handle_http_error <- function(status_code, operation) {
  error_messages <- list(
    "400" = "Bad request - invalid parameters sent to Google Trends API.",
    "403" = "Access forbidden - you may have exceeded rate limits or been temporarily blocked.",
    "429" = "Too many requests - you have been rate limited. Wait before retrying.",
    "500" = "Google Trends server error - try again later.",
    "502" = "Bad gateway - Google Trends service may be temporarily unavailable.",
    "503" = "Service unavailable - Google Trends is temporarily down.",
    "504" = "Gateway timeout - Google Trends request timed out."
  )

  specific_msg <- error_messages[[as.character(status_code)]]

  if (is.null(specific_msg)) {
    specific_msg <- paste("Unexpected HTTP error", status_code)
  }

  stop(
    "HTTP error during ",
    operation,
    " (Status: ",
    status_code,
    "):\n",
    specific_msg,
    "\n",
    "If this persists, the issue may be temporary. Try again later.",
    call. = FALSE
  )
}

#' Parse API response content with encoding handling
#'
#' @param response List with API response
#' @return List with parsed JSON content
#' @noRd
parse_api_response <- function(response) {
  tryCatch(
    {
      # Handle encoding issues
      temp <- rawToChar(response$content)
      Encoding(temp) <- "UTF-8"

      # Parse JSON (skip first 5 characters for Google Trends format)
      parsed <- jsonlite::fromJSON(substring(temp, first = 6L))

      return(parsed)
    },
    error = function(e) {
      stop(
        "Failed to parse API response:\n",
        e$message,
        "\n",
        "The response format may have changed or been corrupted.",
        call. = FALSE
      )
    }
  )
}

#' Fix geographic encoding issue in parsed response
#'
#' @param parsed_response List with parsed JSON
#' @param comparison_item Data frame with original query parameters
#' @return List with corrected geographic information
#' @noRd
fix_geo_encoding <- function(parsed_response, comparison_item) {
  # Handle the case where jsonlite converts "NA" strings to logical NA
  # See: https://github.com/jeroen/jsonlite/issues/314
  if (
    is.logical(parsed_response$widgets$request$comparisonItem[[1L]]$geo$country)
  ) {
    parsed_response$widgets$request$comparisonItem[[
      1L
    ]]$geo$country <- comparison_item$geo
    parsed_response$widgets$request$geo$country <- comparison_item$geo
    parsed_response$widgets$request$restriction$geo$country <- comparison_item$geo
    parsed_response$widgets$geo <- comparison_item$geo
  }

  parsed_response
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

#' Build widget data URL for different endpoint types
#'
#' @param endpoint Character string: "multiline", "multirange", or "comparedgeo"
#' @param payload List with request payload
#' @param token Character string with API token
#' @param tz Numeric timezone offset
#' @param extra_params Named list of additional URL parameters
#' @return Character string with complete widget data URL
#' @noRd
build_widget_url <- function(
  endpoint,
  payload,
  token,
  tz,
  extra_params = list()
) {
  base_urls <- list(
    multiline = "https://www.google.com/trends/api/widgetdata/multiline/csv",
    multirange = "https://trends.google.com/trends/api/widgetdata/multirange/csv",
    comparedgeo = "https://www.google.com/trends/api/widgetdata/comparedgeo/csv"
  )

  if (!endpoint %in% names(base_urls)) {
    stop("Invalid endpoint: ", endpoint, call. = FALSE)
  }

  base_url <- base_urls[[endpoint]]
  encoded_payload <- URLencode(
    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "list"),
    reserved = TRUE
  )

  url_parts <- c(
    paste0(base_url, "?req=", encoded_payload),
    paste0("token=", token),
    paste0("tz=", tz)
  )

  # Add extra parameters
  for (name in names(extra_params)) {
    url_parts <- c(url_parts, paste0(name, "=", extra_params[[name]]))
  }

  return(paste0(URLencode(paste(url_parts, collapse = "&"))))
}

#' Check if widget represents category-only search
#' @param widget Widget object from Google Trends API
#' @return Logical indicating if this is a category-only search
#' @noRd
is_category_only_search <- function(widget) {
  if (is.null(unlist(widget$request$comparisonItem))) {
    return(TRUE)
  }

  if (
    !any(grepl(
      "keyword",
      names(unlist(widget$request$comparisonItem)),
      fixed = TRUE
    ))
  ) {
    return(TRUE)
  }

  return(FALSE)
}

#' Check if widget has multiple timeframes
#' @param widget Widget object from Google Trends API
#' @return Logical indicating if widget has multiple timeframes
#' @noRd
has_multiple_timeframes <- function(widget) {
  if (is.null(widget$request$comparisonItem[[2L]])) {
    return(FALSE)
  }

  time_lengths <- length(widget$request$comparisonItem[[2L]]$time)
  unique_times <- length(unique(widget$request$comparisonItem[[2L]]$time))

  return(time_lengths != 1L && unique_times != 1L)
}

#' Create payload for category-only requests
#' @param widget Widget object from Google Trends API
#' @return List containing the payload for category requests
#' @noRd
create_category_payload <- function(widget) {
  payload <- list()
  payload$locale <- widget$request$locale[1L]
  payload$comparisonItem <- widget$request$comparisonItem[[1L]]
  payload$resolution <- widget$request$resolution[1L]
  payload$requestOptions$category <- widget$request$requestOptions$category[1L]
  payload$requestOptions$backend <- widget$request$requestOptions$backend[1L]
  payload$time <- widget$request$time[1L]
  payload$requestOptions$property <- widget$request$requestOptions$property[1L]

  return(list(payload = payload, token = widget$token[1L]))
}

#' Create payload for multi-timeframe requests
#' @param widget Widget object from Google Trends API
#' @return List containing the payload for multi-timeframe requests
#' @noRd
create_multirange_payload <- function(widget) {
  payload <- list()

  # Get first non-NA values for core parameters
  payload$time <- widget$request$time[head(
    which(!is.na(widget$request$time)),
    1L
  )]
  payload$time <- gsub(" ", "+", payload$time, fixed = TRUE)
  payload$resolution <- widget$request$resolution[head(
    which(!is.na(widget$request$resolution)),
    1L
  )]
  payload$locale <- widget$request$locale[head(
    which(!is.na(widget$request$locale)),
    1L
  )]

  # Set comparison item and options
  payload$comparisonItem <- widget$request$comparisonItem[[2L]]
  payload$comparisonItem$geo <- widget$request$comparisonItem[[2L]]$geo
  payload$requestOptions$property <- widget$request$requestOptions$property[2L]
  payload$requestOptions$backend <- widget$request$requestOptions$backend[2L]
  payload$requestOptions$category <- widget$request$requestOptions$category[2L]

  token <- widget$token[which(widget$id == "TIMESERIES")]

  return(list(payload = payload, token = token))
}

#' Create payload for standard multiline requests
#' @param widget Widget object from Google Trends API
#' @return List containing the payload for standard requests
#' @noRd
create_multiline_payload <- function(widget) {
  payload <- list()

  # Determine which index to use (1 or 2) based on locale and geo conditions
  use_index_1 <- !is.na(widget$request$locale[1L]) ||
    (length(unique(unlist(widget$request$comparisonItem[[1L]]$geo))) > 1L)

  index <- if (use_index_1) 1L else 2L

  payload$locale <- widget$request$locale[index]
  payload$comparisonItem <- widget$request$comparisonItem[[1L]] # Always use [[1]] for comparison item
  payload$resolution <- widget$request$resolution[index]
  payload$requestOptions$category <- widget$request$requestOptions$category[
    index
  ]
  payload$requestOptions$backend <- widget$request$requestOptions$backend[index]
  payload$time <- widget$request$time[index]
  payload$requestOptions$property <- widget$request$requestOptions$property[
    index
  ]

  return(list(payload = payload, token = widget$token[index]))
}

#' Build Google Trends API URL
#' @param base_url Base URL for the API endpoint
#' @param payload List containing the request payload
#' @param token Authentication token
#' @param tz Timezone offset in minutes
#' @param use_encode_payload Whether to use custom payload encoding (for multirange)
#' @return Complete URL string for the API request
#' @noRd
build_trends_url <- function(
  base_url,
  payload,
  token,
  tz,
  use_encode_payload = FALSE
) {
  # Convert payload to JSON
  json_payload <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "list")

  # Encode the payload
  if (use_encode_payload) {
    encoded_payload <- encode_payload(json_payload, reserved = TRUE)
  } else {
    encoded_payload <- URLencode(json_payload, reserved = TRUE)
  }

  # Build complete URL
  trends_url <- paste0(
    URLencode(base_url),
    encoded_payload,
    URLencode(paste0("&token=", token, "&tz=", tz))
  )

  return(trends_url)
}

#' Download and parse CSV response from widget API
#'
#' @param url Character string with widget API URL
#' @param operation Character string describing operation (for errors)
#' @return Data frame with parsed CSV content, or NULL if empty
#' @noRd
download_widget_data <- function(url, operation = "widget data download") {
  response <- make_api_request(url, operation)

  tryCatch(
    {
      con <- textConnection(rawToChar(response$content))
      csv_data <- read.csv(
        con,
        skip = 1L,
        stringsAsFactors = FALSE,
        encoding = "UTF-8"
      )
      close(con)

      if (nrow(csv_data) == 0L) {
        return(NULL)
      }

      return(csv_data)
    },
    error = function(e) {
      stop(
        "Failed to parse CSV response during ",
        operation,
        ":\n",
        e$message,
        call. = FALSE
      )
    }
  )
}

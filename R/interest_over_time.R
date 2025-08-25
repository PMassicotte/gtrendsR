interest_over_time <- function(widget, comparison_item, tz) {
  # Build appropriate URL based on widget type
  interest_over_time_url <- build_interest_over_time_url(widget, tz)

  # Fetch data from Google Trends API
  response <- fetch_trends_data(interest_over_time_url)

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


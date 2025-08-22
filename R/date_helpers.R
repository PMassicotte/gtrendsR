#' Date parsing and formatting helper functions for gtrends package
#'
#' @description
#' These functions handle date parsing, formatting, and timezone conversions
#' consistently across the gtrends package.

#' Parse date column with automatic format detection
#'
#' @param date_vector Character vector of dates in various formats
#' @param tz Numeric timezone offset in minutes
#' @return POSIXct vector of parsed dates
#' @noRd
parse_gtrends_dates <- function(date_vector, tz) {
  if (length(date_vector) == 0L) {
    return(as.POSIXct(character(0L)))
  }

  tz_string <- format_timezone_string(tz)

  # Daily format: 2020-01-01
  if (all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", date_vector))) {
    return(as.POSIXct(
      date_vector,
      format = "%Y-%m-%d",
      tz = tz_string,
      asUTC = TRUE
    ))
  }

  # Hourly format: 2020-01-01T01
  if (all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}$", date_vector))) {
    return(as.POSIXct(
      date_vector,
      format = "%Y-%m-%dT%H",
      tz = tz_string,
      asUTC = TRUE
    ))
  }

  # Monthly format: 2020-01
  if (all(grepl("^[0-9]{4}-[0-9]{2}$", date_vector))) {
    return(as.POSIXct(
      paste0(date_vector, "-01"),
      format = "%Y-%m-%d",
      tz = tz_string,
      asUTC = TRUE
    ))
  }

  # Full datetime format: 2020-01-01T01:30:45 (possibly with more)
  cleaned_dates <- gsub(
    "^([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}).*$",
    "\\1",
    date_vector
  )

  as.POSIXct(
    cleaned_dates,
    format = "%Y-%m-%dT%H:%M:%S",
    tz = tz_string,
    asUTC = TRUE
  )
}

#' Format timezone offset into string
#'
#' @param tz Numeric timezone offset in minutes
#' @return Character string formatted as GMT+/-HH
#' @noRd
format_timezone_string <- function(tz) {
  paste0("GMT", ifelse(tz >= 0L, "+", "-"), abs(tz) / 60L)
}

#' Parse date columns in data frames with different structures
#'
#' @param df Data frame containing date columns
#' @param tz Numeric timezone offset in minutes
#' @param date_cols Character vector of column names containing dates
#' @return Data frame with parsed date columns
#' @noRd
parse_dataframe_dates <- function(df, tz, date_cols = "date") {
  for (col in date_cols) {
    if (col %in% names(df)) {
      df[[col]] <- parse_gtrends_dates(df[[col]], tz)
    }
  }
  return(df)
}

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

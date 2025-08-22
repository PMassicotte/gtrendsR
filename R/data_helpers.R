#' Data processing and reshaping helper functions for gtrends package
#'
#' @description
#' These functions handle the complex data reshaping and processing operations
#' needed to convert Google Trends API responses into clean data frames.

#' Clean and prepare keywords after URL encoding
#'
#' @param comparison_item Data frame with encoded keywords
#' @return Data frame with cleaned keywords
#' @noRd
clean_keywords <- function(comparison_item) {
  comparison_item$keyword <- sapply(
    comparison_item$keyword,
    function(x) {
      x <- gsub("\\+", " ", x, fixed = TRUE)
      x <- gsub("%2B", "+", x, fixed = TRUE)
      x
    },
    USE.NAMES = FALSE
  )

  comparison_item
}

#' Reshape interest over time data from wide to long format
#'
#' @param df Data frame in wide format from API
#' @param comparison_item Data frame with query parameters
#' @param widget List with widget configuration
#' @param tz Numeric timezone offset
#' @return Data frame in long format with proper columns
#' @noRd
reshape_interest_over_time <- function(df, comparison_item, widget, tz) {
  if (nrow(df) < 1L) {
    return(NULL)
  }

  n <- nrow(df)

  # Reshape from wide to long format
  df_long <- reshape(
    df,
    varying = names(df)[2L:ncol(df)],
    v.names = "hits",
    direction = "long",
    timevar = "temp",
    times = names(df)[2L:ncol(df)]
  )

  df_long$temp <- NULL

  # Add metadata from comparison_item
  df_long <- cbind(
    df_long,
    comparison_item[rep(seq_len(nrow(comparison_item)), each = n), 1L:3L],
    row.names = NULL
  )

  # Add additional metadata
  df_long$geo <- ifelse(!nzchar(df_long$geo), "world", df_long$geo)
  df_long$gprop <- ifelse(
    !nzchar(widget$request$requestOptions$property[1L]),
    "web",
    widget$request$requestOptions$property[1L]
  )
  df_long$category <- widget$request$requestOptions$category[1L]

  # Clean up column names and remove unnecessary columns
  names(df_long)[1L] <- "date"
  df_long$id <- NULL

  # Parse dates
  df_long$date <- parse_gtrends_dates(df_long$date, tz)

  df_long
}

#' Process multirange time series data with complex date handling
#'
#' @param df Data frame with multirange data
#' @param comparison_item Data frame with query parameters
#' @param widget List with widget configuration
#' @param tz Numeric timezone offset
#' @return Data frame with processed multirange data
#' @noRd
process_multirange_data <- function(df, comparison_item, widget, tz) {
  n <- nrow(df)

  # Extract keyword for column identification
  kw <- widget$request$comparisonItem[[2L]]$complexKeywordsRestriction[[1L]][[
    1L
  ]]$value
  kw <- gsub("[[:blank:]-]", ".", kw)

  # Separate date columns from data columns
  date_cols <- df[, which(!grepl(kw, names(df))), drop = FALSE]
  hits_cols <- df[, which(grepl(kw, names(df))), drop = FALSE]

  # Parse date columns using helper function
  date_cols <- parse_multirange_dates(date_cols, tz)

  # Combine date and hits data
  result_df <- NULL
  for (jj in seq_len(ncol(date_cols))) {
    df_temp <- data.frame(
      date = date_cols[[jj]],
      hits = hits_cols[[jj]]
    )

    # Add metadata
    meta_data <- comparison_item[rep(jj, n), 1L:3L, drop = FALSE]
    meta_data[, 1L] <- ifelse(
      !nzchar(meta_data[, 1L]),
      "world",
      meta_data[, 1L]
    )
    meta_data$gprop <- ifelse(
      !nzchar(widget$request$requestOptions$property[1L]),
      "web",
      widget$request$requestOptions$property[1L]
    )
    meta_data$category <- widget$request$requestOptions$category[1L]

    df_combined <- cbind(df_temp, meta_data)
    names(df_combined) <- c("date", "hits", "geo", "time", "gprop", "category")

    if (is.null(result_df)) {
      result_df <- df_combined
    } else {
      result_df <- rbind(result_df, df_combined)
    }
  }

  result_df
}

#' Parse dates in multirange format data frames
#'
#' @param date_cols Data frame with date columns
#' @param tz Numeric timezone offset
#' @return Data frame with parsed date columns
#' @noRd
parse_multirange_dates <- function(date_cols, tz) {
  tz_string <- format_timezone_string(tz)

  # Check if all columns have same date format
  sample_dates <- date_cols[[1L]]

  # Daily format
  if (all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", sample_dates))) {
    return(data.frame(lapply(date_cols, function(x) {
      as.POSIXct(x, format = "%Y-%m-%d", tz = tz_string)
    })))
  }

  # Hourly format
  if (all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}$", sample_dates))) {
    return(data.frame(lapply(date_cols, function(x) {
      as.POSIXct(x, format = "%Y-%m-%dT%H", tz = tz_string)
    })))
  }

  # Full datetime format (clean and parse)
  data.frame(lapply(date_cols, function(x) {
    cleaned <- gsub(
      "^([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}).*$",
      "\\1",
      x
    )
    as.POSIXct(cleaned, format = "%Y-%m-%dT%H:%M:%S", tz = tz_string)
  }))
}

#' Process category-only search results
#'
#' @param df Data frame with category search results
#' @param comparison_item Data frame with query parameters
#' @param widget List with widget configuration
#' @param tz Numeric timezone offset
#' @return Data frame with processed category data
#' @noRd
process_category_data <- function(df, comparison_item, widget, tz) {
  n <- nrow(df)
  names(df) <- c("date", "hits")

  # Parse dates
  df$date <- parse_gtrends_dates(df$date, tz)

  # Add metadata
  comparison_item[, "geo"] <- ifelse(
    !nzchar(comparison_item[, "geo"]),
    "world",
    comparison_item[, "geo"]
  )

  comparison_item[, "gprop"] <- ifelse(
    !nzchar(widget$request$requestOptions$property[1L]),
    "web",
    widget$request$requestOptions$property[1L]
  )

  comparison_item[, "category"] <- widget$request$requestOptions$category[1L]

  # Combine with metadata
  result_df <- cbind(df, comparison_item[rep(1L, n), 2L:5L])

  result_df
}

#' Reshape geographic interest data from API response
#'
#' @param df Data frame with geographic data in wide format
#' @param widget List with widget configuration
#' @param resolution Character string with geographic resolution
#' @param i Numeric index for widget element
#' @return Data frame with reshaped geographic data, or NULL if empty
#' @noRd
reshape_geographic_data <- function(df, widget, resolution, i) {
  if (nrow(df) == 0L) {
    return(NULL)
  }

  n <- nrow(df)

  # Reshape from wide to long
  df_long <- reshape(
    df,
    varying = names(df)[2L:ncol(df)],
    v.names = "hits",
    direction = "long",
    timevar = "temp",
    times = names(df)[2L:ncol(df)]
  )

  # Extract keyword information
  keywords <- extract_widget_keywords(widget, i)

  # Add keyword metadata
  df_long <- cbind(
    df_long,
    keywords[rep(seq_len(nrow(keywords)), each = n), 2L, drop = FALSE],
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  # Clean up and add geographic metadata
  df_long$temp <- NULL
  df_long$geo <- suppressWarnings(na.omit(unlist(widget$request$geo[i, ])))
  df_long$geo <- ifelse(is.null(df_long$geo), "world", df_long$geo)
  df_long$gprop <- ifelse(
    widget$request$requestOptions$property[i] == "",
    "web",
    widget$request$requestOptions$property[i]
  )
  df_long$id <- NULL

  # Set proper column names
  names(df_long) <- c("location", "hits", "keyword", "geo", "gprop")
  rownames(df_long) <- NULL

  df_long
}

#' Extract keyword information from widget structure
#'
#' @param widget List with widget configuration
#' @param i Numeric index for widget element
#' @return Data frame with keyword type and value
#' @noRd
extract_widget_keywords <- function(widget, i) {
  restriction <- widget$request$comparisonItem[[i]]$complexKeywordsRestriction

  # Handle different keyword structure formats
  if (length(restriction$operator) == 0L) {
    # Simple keyword list
    kw <- do.call(rbind, restriction$keyword)
  } else {
    # Complex keyword with operators
    value <- paste(restriction$keyword[[1L]]$value, collapse = " + ")
    type <- unique(restriction$keyword[[1L]]$type)
    kw <- data.frame(type = type, value = value, stringsAsFactors = FALSE)
  }

  kw
}

#' Combine interest by region results into final structure
#'
#' @param region_results List of data frames with different geographic
#' resolutions
#'
#' @return List with combined results by resolution type
#' @noRd
combine_region_results <- function(region_results) {
  result <- list(
    interest_by_country = do.call(
      rbind,
      region_results[names(region_results) == "country"]
    ),
    interest_by_region = do.call(
      rbind,
      region_results[names(region_results) == "region"]
    ),
    interest_by_dma = do.call(
      rbind,
      region_results[names(region_results) == "dma"]
    ),
    interest_by_city = do.call(
      rbind,
      region_results[names(region_results) == "city"]
    )
  )

  # Remove row names from all results
  result <- lapply(result, function(x) {
    if (!is.null(x)) {
      row.names(x) <- NULL
    }

    x
  })

  result
}

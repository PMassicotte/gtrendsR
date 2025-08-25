interest_by_region <- function(
  widget,
  comparison_item,
  low_search_volume,
  compared_breakdown,
  tz
) {
  i <- which(grepl("geo_map", widget$id, ignore.case = TRUE) == TRUE)

  if (length(i) == 0L) {
    return(list(NULL))
  }

  base_resolutions <- c("country", "region", "city", "dma")
  # For world-wide searches, include country resolution
  # For specific geo searches, include region resolution instead of country
  if (any(grepl("world", na.omit(widget$geo), fixed = TRUE))) {
    resolutions_to_use <- base_resolutions
  } else {
    resolutions_to_use <- base_resolutions
  }

  resolution <-
    expand.grid(
      i,
      resolutions_to_use,
      stringsAsFactors = FALSE
    )

  resolution <- unique(resolution)

  i <- resolution$Var1
  resolution <- resolution$Var2

  resolution <- toupper(resolution)

  res <-
    mapply(
      fetch_geo_data,
      i,
      resolution,
      MoreArgs = list(
        widget = widget,
        low_search_volume = low_search_volume,
        compared_breakdown = compared_breakdown,
        tz = tz
      ),
      SIMPLIFY = FALSE
    )

  ## Remove duplicated
  ii <- !duplicated(res)
  res <- res[ii]
  resolution <- resolution[ii]

  ## Remove NA
  ii <- !unlist(lapply(res, is.null))
  res <- res[ii]
  resolution <- resolution[ii]

  res <- setNames(res, tolower(resolution))

  res
}


# TODO: Do the same for all payload function
create_geo_payload <- function(
  i,
  widget,
  resolution,
  compared_breakdown,
  low_search_volume,
  tz
) {
  geo_payload <- list()
  geo_payload$locale <- unique(na.omit(widget$request$locale))
  geo_payload$comparisonItem <- widget$request$comparisonItem[[i]]
  geo_payload$resolution <- resolution
  geo_payload$requestOptions$backend <- widget$request$requestOptions$backend[i]
  geo_payload$requestOptions$property <- widget$request$requestOptions$property[
    i
  ]
  geo_payload$requestOptions$category <- widget$request$requestOptions$category[
    i
  ]
  geo_payload$userConfig$userType <- "USER_TYPE_SCRAPER"
  geo_payload$geo <- as.list((widget$request$geo[i, , drop = FALSE]))
  geo_payload$includeLowSearchVolumeGeos <- low_search_volume

  geo_payload
}

fetch_geo_data <- function(
  i,
  widget,
  resolution,
  compared_breakdown,
  low_search_volume,
  tz
) {
  payload <- create_geo_payload(
    i,
    widget,
    resolution,
    compared_breakdown,
    low_search_volume,
    tz
  )

  widget_url <- build_widget_url(
    "comparedgeo",
    payload,
    widget$token[i],
    tz,
    extra_params = list(hl = "en-US")
  )

  res <- tryCatch(
    {
      make_api_request(widget_url, "geo data download")
    },
    error = function(e) {
      # Some resolutions may not be available for all searches
      # This is normal behavior, so we silently return NULL
      NULL
    }
  )

  if (is.null(res)) {
    return(NULL)
  }

  process_geo_response(res, widget, i)
}

process_geo_response <- function(res, widget, i) {
  con <- textConnection(rawToChar(res$content))
  df <- read.csv(con, skip = 1L, stringsAsFactors = FALSE, encoding = "UTF-8")
  close(con)

  if (nrow(df) == 0L) {
    return(NULL)
  }

  n <- nrow(df)

  df <- reshape(
    df,
    varying = names(df)[2L:ncol(df)],
    v.names = "hits",
    direction = "long",
    timevar = "temp",
    times = names(df)[2L:ncol(df)]
  )
  if (
    length(
      widget$request$comparisonItem[[i]]$complexKeywordsRestriction$operator
    ) ==
      0L
  ) {
    kw <- do.call(
      rbind,
      widget$request$comparisonItem[[i]]$complexKeywordsRestriction$keyword
    )
  } else {
    value <- paste(
      widget$request$comparisonItem[[i]]$complexKeywordsRestriction$keyword[[
        1L
      ]]$value,
      collapse = " + "
    )
    type <- unique(
      widget$request$comparisonItem[[i]]$complexKeywordsRestriction$keyword[[
        1L
      ]]$type
    )
    kw <- data.frame(type = type, value = value)
  }

  df <- cbind(
    df,
    kw[rep(seq_len(nrow(kw)), each = n), 2L],
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  df$temp <- NULL
  df$geo <- suppressWarnings(na.omit(unlist(widget$request$geo[i, ])))

  df$geo <- ifelse(is.null(df$geo), "world", df$geo)
  df$gprop <- ifelse(
    !nzchar(widget$request$requestOptions$property[i]),
    "web",
    widget$request$requestOptions$property[i]
  )

  df$id <- NULL
  rownames(df) <- NULL

  names(df) <- c("location", "hits", "keyword", "geo", "gprop")

  df
}

#' Interest by region with error handling
#' @noRd
get_interest_by_region <- function(
  widget,
  comparison_item,
  low_search_volume,
  compared_breakdown,
  tz
) {
  region_results <- interest_by_region(
    widget,
    comparison_item,
    low_search_volume,
    compared_breakdown,
    tz
  )
  combine_region_results(region_results)
}

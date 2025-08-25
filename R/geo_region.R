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

  ## If it is not US metro, then also search for "city"
  # if (!all(grepl("dma", resolution))) {
  #   resolution <- c(resolution, rep("city", length(resolution)))
  # }
  #

  ## If no country is specified, resolution should be "COUNTRY"
  # resolution[grepl("world", na.omit(widget$geo))] <- "country"
  resolution <- toupper(resolution)

  res <-
    mapply(
      create_geo_payload,
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

  return(res)
}


create_geo_payload <- function(
  i,
  widget,
  resolution,
  compared_breakdown,
  low_search_volume,
  tz
) {
  payload2 <- list()
  payload2$locale <- unique(na.omit(widget$request$locale))
  payload2$comparisonItem <- widget$request$comparisonItem[[i]]
  payload2$resolution <- resolution
  payload2$requestOptions$backend <- widget$request$requestOptions$backend[i]
  payload2$requestOptions$property <- widget$request$requestOptions$property[i]
  payload2$requestOptions$category <- widget$request$requestOptions$category[i]
  payload2$userConfig$userType <- "USER_TYPE_SCRAPER"
  payload2$geo <- as.list((widget$request$geo[i, , drop = FALSE]))
  payload2$includeLowSearchVolumeGeos <- low_search_volume

  url <- build_widget_url(
    "comparedgeo",
    payload2,
    widget$token[i],
    tz,
    extra_params = list(hl = "en-US")
  )

  res <- tryCatch(
    {
      make_api_request(url, "geo data download")
    },
    error = function(e) {
      # Some resolutions may not be available for all searches
      # This is normal behavior, so we silently return NULL
      return(NULL)
    }
  )

  if (is.null(res)) {
    return(NULL)
  }

  con <- textConnection(rawToChar(res$content))
  df <- read.csv(con, skip = 1L, stringsAsFactors = FALSE, encoding = "UTF-8")
  close(con)

  if (nrow(df) == 0L) {
    return(NULL)
  }

  n <- nrow(df) # used to reshape the data

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

  # kw <- do.call(rbind, widget$request$comparisonItem[[i]]$complexKeywordsRestriction$keyword)

  df <- cbind(
    df,
    kw[rep(seq_len(nrow(kw)), each = n), 2L],
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  df$temp <- NULL
  # df$geo <- widget$geo[i]
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

  return(df)
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
  return(combine_region_results(region_results))
}
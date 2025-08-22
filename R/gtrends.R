#' Google Trends Query
#'
#' The \code{gtrends} default method performs a Google Trends query for the
#' \sQuote{query} argument and session \sQuote{session}. Optional arguments for
#' geolocation and category can also be supplied.
#'
#' @param keyword A character vector with the actual Google Trends query
#'   keywords. Multiple keywords are possible using \code{gtrends(c("NHL",
#'   "NBA", "MLB", "MLS"))}.
#'
#' @param geo A character vector denoting geographic regions for the query,
#'   default to \dQuote{all} for global queries. Multiple regions are possible
#'   using \code{gtrends("NHL", c("CA", "US"))}.
#'
#' @param time A string specifying the time span of the query. Possible values
#'   are:
#'
#'   \describe{ \item{"now 1-H"}{Last hour} \item{"now 4-H"}{Last four hours}
#'   \item{"now 1-d"}{Last day} \item{"now 7-d"}{Last seven days} \item{"today
#'   1-m"}{Past 30 days} \item{"today 3-m"}{Past 90 days} \item{"today
#'   12-m"}{Past 12 months} \item{"today+5-y"}{Last five years (default)}
#'   \item{"all"}{Since the beginning of Google Trends (2004)} \item{"Y-m-d
#'   Y-m-d"}{Time span between two dates (ex.: "2010-01-01 2010-04-03")} }
#'
#' @param category A character denoting the category, defaults to \dQuote{0}.
#'
#' @param gprop A character string defining the Google product for which the
#'   trend query if preformed. Valid options are:
#'
#'   \itemize{ \item "web" (default) \item "news" \item "images" \item "froogle"
#'   \item "youtube" }
#'
#' @param hl A string specifying the ISO language code (ex.: \dQuote{en-US} or
#'   \dQuote{fr}). Default is \dQuote{en-US}. Note that this is only influencing
#'   the data returned by related topics.
#'
#' @param tz A number specifying the minutes the returned dates should be offset
#'   to UTC. Note the parameter 'time' above is specified in UTC. E.g. choosing
#'   "time=2018-01-01T01 2018-01-01T03" and "tz=-120" will yield data between
#'   2018-01-01T03 and 2018-01-01T05, i.e. data specified to be in UTC+2.
#'
#' @param compared_breakdown Logical. Should compare breakdown the results by
#'   city and subregion? Can only be used if one `geo` is used conjointly with
#'   more than one keyword. If `TRUE`, then the relative hits across the
#'   keywords will be returned. `FALSE` by default.
#'
#' @param low_search_volume Logical. Should include low search volume regions?
#'
#' @param cookie_url A string specifying the URL from which to obtain cookies.
#'   Default should work in general; should only be changed by advanced users.
#'
#' @param onlyInterest If you only want the interest over time set it to TRUE.
#'
#' @section Categories: The package includes a complete list of categories that
#'   can be used to narrow requests. These can be accessed using
#'   \code{data("categories")}.
#'
#' @section Related topics: Note that *related topics* are not retrieved when
#'   more than one keyword is provided due to Google restriction.
#'
#' @importFrom stats na.omit reshape setNames
#' @importFrom utils URLencode read.csv head
#'
#' @return An object of class \sQuote{gtrends} (basically a list of data
#'   frames).
#'
#' @examples
#' \dontrun{
#'
#' head(gtrends("NHL")$interest_over_time)
#' head(gtrends("NHL")$related_topics)
#' head(gtrends("NHL")$related_queries)
#'
#' head(gtrends(c("NHL", "NFL"))$interest_over_time)
#'
#' head(gtrends(c("NHL", "NFL"), geo = c("CA", "US"))$interest_over_time)
#'
#' ## Interest by city
#'
#' gtrends(keyword = "obama", geo = "US-AL-630")
#'
#' ## Sport category (20)
#' data(categories)
#' categories[grepl("^Sport", categories$name), ]
#' gtrends(c("NHL", "NFL"), geo = c("CA", "US"), category = 20)
#' gtrends(geo = c("CA"), category = 20)
#'
#' ## Playing with time format
#'
#' gtrends(c("NHL", "NFL"), time = "now 1-H") # last hour
#' gtrends(c("NHL", "NFL"), time = "now 4-H") # last four hours
#' gtrends(c("NHL", "NFL"), time = "now 1-d") # last day
#' gtrends(c("NHL", "NFL"), time = "today 1-m") # last 30 days
#' gtrends(c("NHL", "NFL"), time = "today 3-m") # last 90 days
#' gtrends(c("NHL", "NFL"), time = "today 12-m") # last 12 months
#' gtrends(c("NHL", "NFL"), time = "today+5-y") # last five years (default)
#' gtrends(c("NHL", "NFL"), time = "all") # since 2004
#'
#' ## Custom date format
#'
#' gtrends(c("NHL", "NFL"), time = "2010-01-01 2010-04-03")
#'
#' ## Search from various Google's services
#'
#' head(gtrends(c("NHL", "NFL"), gprop = "news")$interest_over_time)
#' head(gtrends(c("NHL", "NFL"), gprop = "youtube")$interest_over_time)
#'
#' ## Language settings
#'
#' head(gtrends("NHL", hl = "en")$related_topics)
#' head(gtrends("NHL", hl = "fr")$related_topics)
#'
#' ## Compared breakdown
#' head(gtrends(keyword = c("nhl", "nba"), geo = "CA", compared_breakdown = FALSE)$interest_by_region)
#' head(gtrends(keyword = c("nhl", "nba"), geo = "CA", compared_breakdown = TRUE)$interest_by_region)
#' }
#' @export
gtrends <- function(
  keyword = NA,
  geo = "",
  time = "today+5-y",
  gprop = c("web", "news", "images", "froogle", "youtube"),
  category = 0L,
  hl = "en-US",
  compared_breakdown = FALSE,
  low_search_volume = FALSE,
  cookie_url = "http://trends.google.com/Cookies/NID",
  tz = 0L, # This equals UTC
  onlyInterest = FALSE
) {
  validate_keywords(keyword)
  validate_geo(geo)
  validate_time(time)
  validate_category(category)
  validate_language(hl)
  validate_compared_breakdown(compared_breakdown, geo, keyword)
  validate_parameter_combinations(keyword, geo, time)

  # Validate remaining parameters
  if (length(cookie_url) != 1L || !is.character(cookie_url)) {
    stop(
      "The 'cookie_url' parameter must be a single character string.",
      call. = FALSE
    )
  }

  if (!is.logical(low_search_volume)) {
    stop(
      "The 'low_search_volume' parameter must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.logical(onlyInterest)) {
    stop(
      "The 'onlyInterest' parameter must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  # Process timezone parameter
  tz <- validate_timezone(tz)

  # Process gprop parameter
  gprop <- match.arg(gprop, several.ok = FALSE)
  gprop <- ifelse(gprop == "web", "", gprop)

  # ****************************************************************************
  # Request a token from Google
  # ****************************************************************************

  # Prepare keywords and create comparison item
  prepared_keywords <- prepare_keywords(keyword)
  comparison_item <- create_comparison_item(prepared_keywords, geo, time)

  # Get widget configuration with error handling
  tryCatch(
    {
      widget <- get_widget_enhanced(
        comparison_item,
        category,
        gprop,
        hl,
        cookie_url,
        tz
      )
    },
    error = function(e) {
      stop(
        "Failed to initialize Google Trends session during widget configuration:\n",
        "Error: ",
        e$message,
        "\n",
        "\nPossible causes:\n",
        "  - Network connectivity issues\n",
        "  - Invalid search parameters (keyword, geography, or time range)\n",
        "  - Temporary Google Trends service unavailability\n",
        "  - Rate limiting or blocked requests\n",
        "\nTry again with different parameters or check your network connection.",
        call. = FALSE
      )
    }
  )

  # ****************************************************************************
  # Now that we have tokens, we can process the queries
  # ****************************************************************************

  # Get interest over time data with enhanced error handling
  tryCatch(
    {
      interest_over_time_data <- get_interest_over_time_enhanced(
        widget,
        comparison_item,
        tz
      )
    },
    error = function(e) {
      stop(
        "Failed to retrieve interest over time data:\n",
        e$message,
        call. = FALSE
      )
    }
  )

  if (is.null(interest_over_time_data)) {
    stop(
      "No data returned by the query.\n",
      "This may indicate:\n",
      "  - Very low search volume for your keywords\n",
      "  - Invalid keyword, location, or time parameters\n",
      "  - Temporary Google Trends service issues\n",
      "Try different search parameters or check if your keywords have sufficient search volume.",
      call. = FALSE
    )
  }

  if (!onlyInterest) {
    # Get additional data types with error handling
    tryCatch(
      {
        region_data <- get_interest_by_region_enhanced(
          widget,
          comparison_item,
          low_search_volume,
          compared_breakdown,
          tz
        )
        topics_data <- get_related_topics_enhanced(
          widget,
          comparison_item,
          hl,
          tz
        )
        queries_data <- get_related_queries_enhanced(
          widget,
          comparison_item,
          tz,
          hl
        )
      },
      error = function(e) {
        warning(
          "Some additional data could not be retrieved: ",
          e$message,
          "\nReturning interest over time data only.",
          call. = FALSE
        )
        region_data <- list(
          country = NULL,
          region = NULL,
          dma = NULL,
          city = NULL
        )
        topics_data <- NULL
        queries_data <- NULL
      }
    )

    res <- list(
      interest_over_time = interest_over_time_data,
      interest_by_country = region_data$country,
      interest_by_region = region_data$region,
      interest_by_dma = region_data$dma,
      interest_by_city = region_data$city,
      related_topics = topics_data,
      related_queries = queries_data
    )
  } else {
    res <- list(interest_over_time = interest_over_time_data)
  }
  # Clean up row names and ensure consistent structure
  res <- lapply(res, function(x) {
    if (!is.null(x) && is.data.frame(x)) {
      row.names(x) <- NULL
    }
    return(x)
  })

  class(res) <- c("gtrends", "list")

  return(res)
}

#' Plot Google Trends interest over time
#'
#' @param x A \code{\link{gtrends}} object.
#' @param ... Additional parameters passed on in method dispatch. Currently not
#'   used.
#'
#' @import ggplot2
#'
#' @return A ggplot2 object is returned silently.
#' @export
#'
#' @examples
#' \dontrun{
#' res <- gtrends("nhl", geo = c("CA", "US"))
#' plot(res)
#' }
plot.gtrends <- function(x, ...) {
  df <- x$interest_over_time

  df$hits <- if (typeof(df$hits) == "character") {
    as.numeric(gsub("<", "", df$hits, fixed = TRUE))
  } else {
    df$hits
  }

  df$legend <- paste0(df$keyword, " (", df$geo, ")")

  p <- ggplot(df, aes_string(x = "date", y = "hits", color = "legend")) +
    geom_line() +
    xlab("Date") +
    ylab("Search hits") +
    ggtitle("Interest over time") +
    theme_bw() +
    theme(legend.title = element_blank())

  print(p)
  invisible(p)
}

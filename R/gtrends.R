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
  category = 0,
  hl = "en-US",
  compared_breakdown = FALSE,
  low_search_volume = FALSE,
  cookie_url = "http://trends.google.com/Cookies/NID",
  tz = 0, # This equals UTC
  onlyInterest = FALSE
) {
  stopifnot(
    # One  vector should be a multiple of the other
    (length(keyword) %% length(geo) == 0) ||
      (length(geo) %% length(keyword) == 0) ||
      (length(time) %% length(keyword) == 0),
    is.vector(keyword),
    length(keyword) <= 5,
    length(geo) <= 5,
    length(time) <= 5,
    length(hl) == 1,
    is.character(hl),
    hl %in% language_codes$code,
    length(cookie_url) == 1,
    is.character(cookie_url)
  )

  ## Check if valid geo. There are no official source(s) that we can use to
  ## validate the entered geo code. However, we can use a regular expression to
  ## verify if the structure is valid.

  m <- regexpr("^[a-zA-Z]{2}((?:-\\w{1,3}))?(?:-\\d{1,3})?", geo)
  ret <- regmatches(geo, m)

  if (all(geo != "")) {
    if (!identical(ret, geo)) {
      stop("Country code not formatted correctly.", call. = FALSE)
    }
  }

  ## Check if valid category
  if (!all(category %in% categories[, "id"])) {
    stop(
      "Category code not valid. Please use 'data(categories)' to retreive valid codes.",
      call. = FALSE
    )
  }

  ## Check if time format is ok
  if (!check_time(time)) {
    stop("Cannot parse the supplied time format.", call. = FALSE)
  }

  if (compared_breakdown & (length(geo) != 1 | length(keyword) == 1)) {
    stop(
      "`compared breakdown` can be only used with one geo and multiple keywords.",
      call. = FALSE
    )
  }

  if (!(is.numeric(tz))) {
    if (tz %in% OlsonNames()) {
      tz <- map_tz2min(tz)
    } else {
      stop(
        "Given timezone not known. Check function OlsonNames().",
        call. = FALSE
      )
    }
  }

  gprop <- match.arg(gprop, several.ok = FALSE)
  gprop <- ifelse(gprop == "web", "", gprop)

  # ****************************************************************************
  # Request a token from Google
  # ****************************************************************************
  keyword <- sapply(keyword, function(x) {
    y <- gsub("[+]", "%2B", x)
    z <- gsub(" ", "+", y)
    return(z)
  })
  names(keyword) <- NULL
  comparison_item <- data.frame(keyword, geo, time, stringsAsFactors = FALSE)

  widget <- get_widget(comparison_item, category, gprop, hl, cookie_url, tz)

  # ****************************************************************************
  # Now that we have tokens, we can process the queries
  # ****************************************************************************

  interest_over_time <- interest_over_time(widget, comparison_item, tz)

  if (is.null(interest_over_time)) {
    stop(
      "No data returned by the query. Consider changing search parameters.",
      call. = FALSE
    )
  }

  if (!onlyInterest) {
    interest_by_region <-
      interest_by_region(
        widget,
        comparison_item,
        low_search_volume,
        compared_breakdown,
        tz
      )

    related_topics <- related_topics(widget, comparison_item, hl, tz)
    related_queries <- related_queries(widget, comparison_item, tz, hl)

    res <- list(
      interest_over_time = interest_over_time,
      interest_by_country = do.call(
        rbind,
        interest_by_region[names(interest_by_region) == "country"]
      ),
      interest_by_region = do.call(
        rbind,
        interest_by_region[names(interest_by_region) == "region"]
      ),
      interest_by_dma = do.call(
        rbind,
        interest_by_region[names(interest_by_region) == "dma"]
      ),
      interest_by_city = do.call(
        rbind,
        interest_by_region[names(interest_by_region) == "city"]
      ),
      related_topics = related_topics,
      related_queries = related_queries
    )
  } else {
    res <- list(interest_over_time = interest_over_time)
  }
  ## Remove row.names
  res <- lapply(res, function(x) {
    row.names(x) <- NULL
    x
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
    as.numeric(gsub("<", "", df$hits))
  } else {
    df$hits
  }

  df$legend <- paste(df$keyword, " (", df$geo, ")", sep = "")

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

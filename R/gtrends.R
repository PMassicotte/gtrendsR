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
#' @param time The time
#'   
#' @param category A character denoting the category, defaults to \dQuote{0}.
#'   
#' @param gprop A character string defining the Google product for which the 
#'   trend query if preformed. Valid options are \dQuote{} (empty string - web 
#'   search), \dQuote{news}, \dQuote{images}, \dQuote{froogle} and 
#'   \dQuote{youtube}. Default is \dQuote{}.
#'   
#' @section Categories: The package includes a complete list of categories that 
#'   can be used to narrow requests. These can be accessed using 
#'   \code{data("categories")}.
#'   
#' @importFrom stats na.omit reshape
#' @importFrom utils URLencode read.csv
#'   
#' @return An object of class \sQuote{gtrends} (basically a dataframe).
#'   
#' @examples
#' 
#' head(gtrends("NHL"))
#' 
#' head(gtrends(c("NHL", "NFL")))
#' 
#' head(gtrends(c("NHL", "NFL"), geo = c("CA", "US")))
#' 
#' ## Sport category (20)
#' data(categories)
#' categories[grepl("^Sport", categories$name), ]
#' head(gtrends(c("NHL", "NFL"), geo = c("CA", "US"), category = 20))
#' 
#' ## Playing with time format
#' 
#' head(gtrends(c("NHL", "NFL"), time = "now 1-H")) # last hour
#' head(gtrends(c("NHL", "NFL"), time = "now 4-H")) # last four hours
#' head(gtrends(c("NHL", "NFL"), time = "now 1-d")) # last day
#' head(gtrends(c("NHL", "NFL"), time = "today 1-m")) # last 30 days
#' head(gtrends(c("NHL", "NFL"), time = "today 3-m")) # last 90 days
#' head(gtrends(c("NHL", "NFL"), time = "today 12-m")) # last 12 months
#' head(gtrends(c("NHL", "NFL"), time = "today+5-y")) # last five years (default)
#' head(gtrends(c("NHL", "NFL"), time = "all")) # since 2004
#' 
#' ## Custom date format
#' 
#' head(gtrends(c("NHL", "NFL"), time = "2010-01-01 2010-04-03")) 
#' 
#' @export
gtrends <- function(
  keyword, 
  geo = "", 
  time = "today+5-y", 
  gprop = c("", "news", "images", "froogle", "youtube"), 
  category = 0) {
  
  stopifnot(
    # One  vector should be a multiple of the other
    (length(keyword) %% length(geo) == 0) || (length(geo) %% length(keyword) == 0),
    is.vector(keyword),
    length(keyword) <= 5,
    length(geo) <= 5,
    length(time) == 1
  )
  
  ## Check if valide geo
  if (geo != "" &&
      !all(geo %in% countries[, "country_code"]) &&
      !all(geo %in% countries[, "sub_code"])) {
    stop("Country code not valid. Please use 'data(countries)' to retreive valid codes.",
         call. = FALSE)
  }
  
  ## Check if valide category
  if (!all(category %in% categories[, "id"]))  {
    stop("Category code not valid. Please use 'data(categories)' to retreive valid codes.",
         call. = FALSE)
  }
  
  ## Check if time format is ok
  if (!check_time(time)) {
    stop("Can not parse the supplied time format.", call. = FALSE)
  }
  
  # time <- "today+5-y"
  # time <- "2017-02-09 2017-02-18"
  # time <- "now 7-d"
  # time <- "all_2006"
  # time <- "all"
  # time <- "now 4-H"
  # geo <- c("CA", "FR", "US")
  # geo <- c("CA", "DK", "FR", "US", "CA")
  # geo <- "US"
  
  gprop <- match.arg(gprop, several.ok = FALSE)
  
  # ****************************************************************************
  # Request a token from Google
  # ****************************************************************************
  
  comparison_item <- data.frame(keyword, geo, time, stringsAsFactors = FALSE)
  
  widget <- get_widget(comparison_item, category)
  
  # ****************************************************************************
  # Now that we have tokens, we can process the queries
  # ****************************************************************************
  
  df1 <- interest_over_time(widget, comparison_item)
  df2 <- interest_by_region(widget, comparison_item)
  
  res <- list(interest_over_time = df1, interest_by_region = df2)
  
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
#' res <- gtrends("nhl", geo = c("CA", "US"))
#' plot(res)
plot.gtrends <- function(x, ...) {

  df <- x$interest_over_time
  
  df$legend <-  paste(df$keyword, " (", df$geo, ")", sep = "")
  
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
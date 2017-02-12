
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
#' @return An object of class \sQuote{gtrends} (basically a dataframe).
#'   
#' @examples
#' 
#' head(gtrends2("NHL"))
#' 
#' head(gtrends2(c("NHL", "NFL")))
#' 
#' head(gtrends2(c("NHL", "NFL"), geo = c("CA", "US")))
#' 
#' ## Sport category (20)
#' data(categories)
#' categories[grepl("^Sport", categories$name), ]
#' head(gtrends2(c("NHL", "NFL"), geo = c("CA", "US"), category = 20))
#' 
#' @export
gtrends2 <- function(keyword, geo = "", time = "today+5-y", gprop = "", category = 0) {
  
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
  
  # time <- "today+5-y"
  # time <- "2017-02-09 2017-02-18"
  # time <- "now 7-d"
  # geo <- c("CA", "FR", "US")
  # geo <- c("CA", "DK", "FR", "US", "CA")
  # geo <- "US"
  
  # ****************************************************************************
  # Request a token from Google
  # ****************************************************************************
  
  comparison_item <- data.frame(keyword, geo, time, stringsAsFactors = FALSE)
  
  payload <- list()
  payload$comparisonItem <- comparison_item
  payload$category <- category
  
  url <- URLencode(paste0("https://www.google.com/trends/api/explore?property=&req=", 
                          jsonlite::toJSON(payload, auto_unbox = TRUE), 
                          "&tz=360&hl=en-US")) ## Need better than this
  
  widget <- curl::curl_fetch_memory(url)
  
  stopifnot(widget$status_code == 200)
  
  myjs <- jsonlite::fromJSON(substring(rawToChar(widget$content), first = 6))
  widget <- myjs$widgets
  
  # widget$token
  
  # ****************************************************************************
  # Now that we have a token, we can process the query
  # ****************************************************************************
  
  payload2 <- list()
  payload2$locale <- unique(na.omit(widget$request$locale))
  payload2$comparisonItem <- widget$request$comparisonItem[[1]]
  payload2$resolution <- widget$request$resolution[1]
  payload2$requestOptions$category <- unique(na.omit(widget$request$requestOptions$category))
  payload2$requestOptions$backend <- unique(na.omit(widget$request$requestOptions$backend))
  payload2$time <- unique(na.omit(widget$request$time))
  
  
  url <- paste0(
    "https://www.google.fr/trends/api/widgetdata/multiline/csv?req=",
    jsonlite::toJSON(payload2, auto_unbox = T),
    "&token=", widget$token[1],
    "&tz=360"
  )
  
  # ****************************************************************************
  # Downoad the results
  # ****************************************************************************
  
  res <- curl::curl_fetch_memory(URLencode(url))
  
  stopifnot(res$status_code == 200)
  
  # ****************************************************************************
  # Format the results in a nice way
  # ****************************************************************************
  df <- read.csv(textConnection(rawToChar(res$content)),
                  skip = 1,
                  stringsAsFactors = FALSE)
  
  df <- reshape(
    df,
    varying = names(df)[2:ncol(df)],
    v.names = "hits",
    direction = "long",
    timevar = "temp",
    times = names(df)[2:ncol(df)]
  )
  
  df$temp <- NULL
  
  df <- cbind(df, 
              comparison_item[rep(seq_len(nrow(comparison_item)), each = nrow(df)), 1:2], 
              row.names = NULL)
  
  df$geo <- ifelse(df$geo == "", "world", df$geo)
  
  names(df)[1] <- "date"
  
  # Oh, it works!
  df$date <- anytime::anytime(df$date)
  
  class(df) <- c("gtrends2", "data.frame")
  
  return(df)
  
}

# keyword <- LETTERS[1:5]
# 
# # keyword <- "trump"
# 
# # if (!(Encoding(keyword) == "UTF-8")) {
# #   keyword <- iconv(keyword, "latin1", "utf-8", sub = "byte")
# # }
# 
# library(ggplot2)
# 
# ggplot(res2, aes(x = date, y = hits)) +
#   geom_line(aes(color = paste(keyword, " (", geo, ")", sep = ""))) +
#   theme(legend.title = element_blank())


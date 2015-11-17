#' Google Trends sport data
#' 
#' Google Trends data for keywords \code{nhl}, \code{nba} and \code{nfl} between
#' 2004-01-04 and 2015-11-14.
#' 
#' @references Data Source: Google Trends (www.google.com/trends)
#' @source \url{www.google.com/trends} 
#' @docType data
#' @keywords datasets
#' @name sport_trend
#' @usage data("sport_trend")
#' @format An object of class \code{\link{gtrends}} containing:
#' \describe{
#'   \item{query}{Query information such has keywords and time of the query}
#'   \item{meta}{Meta data returned by Google Trends upon a query}
#'   \item{tend}{A data frame containing Google Trends data for each keyword}
#'   \item{regions}{A list containing one data frame with top regions hit search for each keyword}
#'   \item{topmetros}{A list containing one data frame with top metros hit search for each keyword}
#'   \item{cities}{A list containing one data frame with top cities hit search for each keyword}
#'   \item{searches}{Top related searches for each keyword}
#'   \item{rising}{Rising searches for each keyword}
#'   \item{headers}{Header information for each bloc described above}
#'   }
#' @examples
#' \dontrun{
#' ch <- gconnect("usr@gmail.com", "psw")
#' sport_trend <- gtrends(ch, c("nhl", "nba", "nfl"))
#' }
#' 
#' data("sport_trend")
#' plot(sport_trend)
NULL

#' Connect to Google account
#'
#' The resulting connection object is also stored in the package-local
#' environment from which the (internal) helper function
#' \code{.getDefaultConnection()} retrieves it as needed.
#'
#' If the environment variables \code{GOOGLE_USER} and
#' \code{GOOGLE_PASSWORD} are set, they will be retrieved in case no
#' argument has been supplied.  Similarly, the environment variable
#' \code{options("google.user")} or \code{options("google.password")} can be
#' used. Lastly, if the environment variable \code{GOOGLE_AUTOCONNECT}
#' is set to (the text string) \sQuote{TRUE}, or the the R option
#' \code{options("google.autoconnect")} is set to \sQuote{TRUE} then
#' the connection is automatically made at package load.
#'
#' @note Should you have trouble connecting, and also use two-factor authentication on
#' your Google Account, then consider creating another Google account (without
#' two-factor authentication) which should allow automated (i.e. programmatic)
#' connection here.

#' @param usr User name (ex.: yourmail@gmail.com); alternatively the 
#' environment variable \code{GOOGLE_USER} as well as 
#' \code{options("google.user")} can be used to supply the user name.
#' @param psw Account password; alternatively the environment
#' variable \code{GOOGLE_PASSWORD} as well as 
#' \code{options("google.password")} can be used to supply the password.
#' @param verbose Logical for displaying additional information
#'
#' @return A libcurl handle is returned (invisibly).
#' @import RCurl
#' @export
#' @examples
#' \dontrun{
#' # use with explicit arguments
#' ch <- gconnect("usr@gmail.com", "psw")
#' 
#' # use with arguments stored in env.var or options()
#' # this is preferred for scripts shared with others who
#' # can place their secret password in a file only they know
#' ch <- gconnect("usr@gmail.com", "psw")
#' }
gconnect <- function(usr = NULL, psw = NULL, verbose = FALSE) {
  
  loginURL <- "https://accounts.google.com/accounts/ServiceLogin"
  
  authenticateURL <- "https://accounts.google.com/ServiceLoginBoxAuth"
  
  if (is.null(usr)) {
    
    if (Sys.getenv("GOOGLE_USER") != "") usr <- Sys.getenv("GOOGLE_USER")
    
    if (getOption("google.user") != "") usr <- getOption("google.user")
    
    if (is.null(usr)) stop("No Google Username / account supplied.", 
                           call. = FALSE)
  }
  
  if (is.null(psw)) {
    
    if (Sys.getenv("GOOGLE_PASSWORD") != "") psw <- Sys.getenv("GOOGLE_PASSWORD")
    
    if (getOption("google.password") != "") psw <- getOption("google.password")
    
    if (is.null(psw)) stop("No Google password supplied.", call. = FALSE)
  }
  
  ch <- getCurlHandle()
  
  ans <- curlSetOpt(curl = ch,
                    ssl.verifypeer = FALSE,
                    useragent = getOption('HTTPUserAgent', "R"),
                    timeout = 60,         
                    followlocation = TRUE,
                    cookiejar = "./cookies",
                    cookiefile = "")
  
  galx <- .getGALX(ch)
  
  formparams <- list(Email = usr,
                     Passwd = psw,
                     GALX = galx,
                     PersistentCookie = "yes",
                     continue = "http://www.google.com/trends")
  
  authenticatePage <- postForm(authenticateURL, .params = formparams, curl = ch)
  #print(getCurlInfo(ch)$response.code)
  
  authenticatePage2 <- getURL("https://www.google.com/accounts/CheckCookie?chtml=LoginDoneHtml", curl = ch)
  #print(getCurlInfo(ch)$response.code)
  
  #if http answer is 200 then login was ok
  if (getCurlInfo(ch)$response.code == 200) {
    
    if (verbose) cat("Google login successful!\n")
  
  } else {
    
    cat("Google login failed! Check your login information.")
    return(NULL)
  }

  ## store connection handler in package-local environment
  assign("ch", ch, envir = .pkgenv)
    
  invisible(ch)
  
}


#---------------------------------------------------------------------
# This gets the GALX cookie which we need to pass back in the login 
# form we post.
#---------------------------------------------------------------------
.getGALX <- function(curl) {
  txt <- basicTextGatherer()
  
  curlPerform(url = "https://accounts.google.com/accounts/ServiceLogin", 
              curl = curl, 
              writefunction = txt$update, 
              header = TRUE, 
              ssl.verifypeer = FALSE)
  
  tmp <- txt$value()
  
  val <- grep("Cookie: GALX", strsplit(tmp, "\n")[[1]], value = TRUE)
  
  strsplit(val, "[:=;]")[[1]][3]
  
  return(strsplit(val, "[:=;]")[[1]][3])
}

#' @rdname gconnect
.getDefaultConnection <- function() {
    ch <- .pkgenv$ch
    if (is.null(ch))
        stop("No connection object has been created. Use 'gconnect()' first.",
             call.=FALSE)
    ch
}

#' Google Trends Query
#' 
#' The \code{gtrends} default method performs a Google Trends query for the 
#' \sQuote{query} argument and handle \sQuote{ch}. Optional arguments for 
#' geolocation and category can also be supplied.
#' 
#' @param query A character vector with the actual Google Trends query keywords.
#'   Multiple keywords are possible using \code{gtrends(c("NHL", "NBA", "MLB", 
#'   "MLS"))}.
#'   
#' @param geo A character vector denoting geographic regions for the query, 
#'   default to \dQuote{all} for global queries. Multiple regions are possible 
#'   using \code{gtrends("NHL", c("CA", "US"))}.
#'   
#' @param cat A character denoting the category, defaults to \dQuote{0}.
#'      
#' @param res Resolution of the trending data to be returned. One of 
#'   \code{c("1h", "4h", "1d", "7d")}. If \code{res} is provided, then 
#'   \code{start_date} and \code{end_date} parameters are ignored. See 
#'   \emph{Query resolution} for more information.
#'   
#' @param start_date Starting date using yyyy-mm-dd format. Must be greater than
#'   2004-01-01.
#'   
#' @param end_date Starting date using yyyy-mm-dd format. Must be before than 
#'   current date.
#'   
#' @param ch A valid handle which can be created via \code{\link{gconnect}}. 
#'   Users can either supply an explicit handle, or rely on the helper function 
#'   \code{.getDefaultConnection()} to retrieve the current connection handle.
#'   
#' @param ... Additional parameters passed on in method dispatch.
#'
#' @section Query resolution: By default, Google returns weekly information when
#'   the requested data spans a period greater than three months. It is also 
#'   possible to obtain \emph{daily} and \emph{hourly} information. However, 
#'   these are only available for a certain period prior to the \emph{current} 
#'   date.
#'   
#'   For instance, \code{1h}, \code{7h}, \code{1d} and \code{7d} denote 
#'   trends data for the last 1 hour, last four hours, last day and last seven 
#'   days respectively. Using one of the above \code{res} will return the 
#'   corresponding hourly data.
#'   
#'   Note that data requested for a beriod between one and three months will be 
#'   returned daily. For a  period greater than three months, data will be 
#'   always returned weekly.
#'   
#' @return An object of class \sQuote{gtrends} which is list with six elements 
#'   containing the results.
#' @examples 
#' \dontrun{
#' ch <- gconnect("usr@gmail.com", "psw")
#' 
#' gtrends(c("NHL", "NBA", "MLB", "MLS"))
#' 
#' gtrends("NHL", geo = c("CA", "US"))
#' 
#' # Search only for the sport category.
#' gtrends("NHL", geo = c("CA", "US"), cat = "0-20")
#' 
#' # Trends between 2015-01-01 and 2015-03-01 in Sweeden. Will be daily data.
#' gtrends("NHL", geo = c("SE"), start_date = "2015-01-01", end_date = "2015-03-01")
#' 
#' # Trends between 2015-01-01 and 2015-04-01 in Sweeden. Will be weekly data.
#' gtrends("NHL", geo = c("SE"), start_date = "2015-01-01", end_date = "2015-04-01")
#' 
#' # Last 4 hours trends
#' gtrends("NHL", geo = c("CA"), res = "4h")
#' 
#' # Last 7 days trends
#' gtrends("NHL", geo = c("CA"), res = "7d")
#' }
#' @export
gtrends <- function(query, geo, cat, ch, ...) {
  
  UseMethod("gtrends")
    
}

#' @importFrom utils data
#' @rdname gtrends
#' @export
gtrends.default <- function(query, 
                            geo, 
                            cat, 
                            ch, 
                            res = c(NA, "1h", "4h", "1d", "7d"),
                            start_date = as.Date("2004-01-01"),
                            end_date = as.Date(Sys.time()),
                            ...){

  if (missing(geo)) geo <- ""
  if (missing(cat)) cat <- "0"
  if (missing(ch))  ch  <- .getDefaultConnection()

  stopifnot(is.character(query),
            is.vector(query),
            length(query) <= 5, 
            length(geo) <= 5)
  
  res <- match.arg(res, several.ok = FALSE)
  
  if(length(query) > 1 & length(geo) > 1){
    stop("Can not specify multiple keywords and geo at the same time.", 
         call. = FALSE)
  }
  
  cmpt <- ifelse(length(query) > 1, "q", "geo")
  
  if(is.null(ch)) stop("You are not signed in. Please log in using gconnect().",
                       call. = FALSE)
  
  if (inherits(ch, "CURLHandle") != TRUE) {
    stop("'ch' arguments has to be result from 'gconnect()'.", 
         call. = FALSE)
  }
  
  #---------------------------------------------------------------------
  # Date verification.
  #---------------------------------------------------------------------
  
  start_date <- as.Date(start_date, "%Y-%m-%d")  
  end_date <- as.Date(end_date, "%Y-%m-%d")  
  
  if (is.na(start_date)) {
    stop("start_date is not a valid date. Please use yyyy-mm-dd format.",
         call. = FALSE)
  } 
  
  if (is.na(end_date)) {
    stop("end_date is not a valid date. Please use yyyy-mm-dd format.",
         call. = FALSE)
  } 
  
  stopifnot(start_date < end_date, 
            start_date >= as.Date("2004-01-01"), # cant be earlier than 2004
            end_date <= as.Date(Sys.time())) # cant be more than current date
  
  nmonth <- length(seq(from = start_date, to = end_date, by = "month"))
  
  if(nmonth >= 1){
    date <- paste(format(start_date, "%m/%Y"), paste(nmonth, "m", sep = ""))
  }
  
  if(!is.na(res)){
    
    # Match Google code (ex. 1-H) to a more user friendly value (1h)
    resolution_code <- data.frame(code = c("1-H", "4-H", "1-d", "7-d"),
                                  res = c("1h", "4h", "1d", "7d"), 
                                  stringsAsFactors = FALSE)
    
    res <- resolution_code$code[resolution_code$res == res]
    
    date <- paste("now" , res)
  }

  #---------------------------------------------------------------------
  # Build the query.
  #---------------------------------------------------------------------
  query <- paste(query, collapse = ",")
  
  ## Change encoding to utf-8
  if (!(Encoding(query) == "UTF-8")) {
    query <- iconv(query, "latin1", "utf-8", sub = "byte")
  }
  
  countries[, 1] <- as.character(countries[, 1])
  countries[, 2] <- as.character(countries[, 2])
  countries[which(countries[, "country"] == "Namibia"), "code"] <- "NA"
  
  if (geo != "" && !all(geo %in% countries[, "code"]) && !all(geo %in% countries[, "subcode"])) {
    stop("Country code not valid. Please use 'data(countries)' to retreive valid codes.",
         call. = FALSE)
  }
  
  geo <- paste(geo, sep = "", collapse = ", ")
  #geo <- URLencode(geo, reserved = TRUE)
  
  authenticatePage2 <- getURL("http://www.google.com", curl = ch)
  
  trendsURL <- "http://www.google.com/trends/trendsReport?"

  pp <- list(q = query, 
             cat = cat,
             cmpt = cmpt, 
             content = 1, 
             export = 1,
             date = date,
             geo = geo)
  
  resultsText <- getForm(trendsURL, .params = pp, curl = ch)
  
  if (any(grep("QUOTA", resultsText))) {
    stop("Reached Google Trends quota limit! Please try again later.")
  }
  
  queryparams <- c(query = query, 
                   cat = cat, 
                   geo = geo, 
                   time = format(Sys.time()))
  
  res <- .processResults(resultsText, queryparams)

  class(res) <- c("gtrends", "list")
    
  res
}

#' @rdname gtrends
#' @param object A \code{\link{gtrends}} object
#' @import zoo
#' @export
summary.gtrends <- function(object, ...) {
  cat("Google Trends results for:\n")
  cat(unlist(strsplit(object$query[1], ",")))
  cat("\nRequested at: ")
  cat(object$query[4])
  cat("\n\nSummary of trend:\n")
  print(summary(as.zoo.gtrends(object)))
  ## cat("\nMain regions:\n")
  ## print(head(object[["regions"]]))
  ## cat("\nMain cities:\n")
  ## print(head(object[["cities"]]))
  ## cat("\nTop searches cities:\n")
  ## print(head(object[["searches"]]))
  ## cat("\nRising searches:\n")
  ## print(head(object[["rising"]]))
  invisible(NULL)
}

#' @rdname gtrends
#' @param x A \code{\link{gtrends}} object
#' @param type A character variable selecting the type of plot;
#' permissible values are \sQuote{trends} (which is also the
#' default), \sQuote{geo}.
#' @param which Block number containing the geographical data to plot.
#' @param ind A integer selecting the result set in case of multiple
#' search terms.
#' @return When \code{type} is equal to \sQuote{trends}, the resulting 
#' ggplot2 object is returned silently.
#' @import googleVis
#' @import ggplot2
#' @importFrom graphics plot
#' @importFrom stats reshape
#' @importFrom utils data
#' @examples 
#' data("sport_trend")
#' plot(sport_trend)
#' @export
plot.gtrends <- function(x, type = c("trend", "geo"), which = 5, ind = 1L, ...){
  
  type <- match.arg(type)
  ret <- NULL # by default we return nothing
  
  if (type == "trend") {
    
    df <- x$trend
    
    df <- reshape(df,
                  varying = names(df)[mapply(is.numeric, x$trend)],
                  v.names = "hit",
                  idvar = names(df)[!mapply(is.numeric, x$trend)],
                  direction = "long",
                  times = names(df)[mapply(is.numeric, x$trend)],
                  timevar = "keyword")
    
    df$start <- as.POSIXct(df$start)
    
    p <- ggplot(df, aes_string(x = "start", y = "hit", color = "keyword")) +
      geom_line() +
      xlab("Date") +
      ylab("Search hits") +
      ggtitle("Interest over time") +
      theme_bw()
    
    print(p)
    ret <- p
    
  } else if (type == "geo") {
    
    stopifnot(ind <= length(x[which]),
              which >= 5,
              which <= length(x))
    
    block <- x[which][[ind]]
    
    # Try to find if the requested block contains geographic information.
    if(!any(tolower(block[1, ]) %in% tolower(locations$Name))){
      
      message("The requested block does not seems to contain geographical information. Please choose another block.")
      
      print(paste(1:length(x), ":", " ", names(x), sep = ""))
      
      stop(call. = FALSE)
      
    }
    
    if(all(is.na(block))) stop("Not enough search volume to show results.", 
                      call. = FALSE)
    
    df <- data.frame(loc = block[, 1], hits = block[, 2])
    
    plot(gvisGeoChart(df, 
                      "loc",
                      "hits",
                      options = list(region = "world",
                                     displayMode = "markers",
                                     resolution = "countries")))
  } 
  
  invisible(ret)
}

#' @rdname gtrends
as.zoo.gtrends <- function(x, ...) {
  z <- zoo(x[["trend"]][, -(1:2), drop = FALSE],
           order.by = x[["trend"]][, "end"])
  z
}

#' @importFrom utils read.csv
#' @importFrom stats na.omit
.processResults <- function(resultsText, queryparams) {
  
  #get back to latin1 encoding
  queryparams[1] <- iconv(queryparams[1], "utf-8", "latin1", sub = "byte")
  
  vec <- strsplit(resultsText, "\\\n{2,}")[[1]]
  
  headers <- unname(sapply(vec, function(v) strsplit(v, "\\\n")[[1]][1]))
  
  ## Make sure there are some results have been returned.
  if (length(vec) < 2) {
    stop("Not enough search volume. Please change your search terms.",
         call. = FALSE)
  }
  
  #---------------------------------------------------------------------
  # Section to deal with trend data.
  #---------------------------------------------------------------------
  
  # meta data
  meta  <- strsplit(vec[1], "\\\r\\\n")[[1]]

  # trend
  trend <- read.csv(textConnection(strsplit(vec[2], "\\\n")[[1]]),
                    skip = 1,
                    stringsAsFactors = FALSE)
  
  # block date
  
  weeks <- data.frame(date = do.call(rbind, strsplit(trend[, 1], " - ")),
                      stringsAsFactors = FALSE)
  
  trend <- trend[, mapply(is.numeric, trend), drop = FALSE]
  
  #names(trend) <- unlist(strsplit(queryparams[1], ","), use.names = FALSE)
  
  kw <- trimws(unlist(strsplit(queryparams[1], ","), use.names = FALSE))
  geo <- trimws(unlist(strsplit(queryparams[3], ","), use.names = FALSE))
  names(trend) <- make.names(paste(kw, geo))
  
  
  if(ncol(weeks) == 2){
    
    weeks <- lapply(weeks, as.POSIXct, SIMPLIFY = FALSE)
    weeks <- do.call(cbind.data.frame, weeks)
    names(weeks) <- c("start", "end")[1:ncol(weeks)]
  
  }
  
  # Either daily or hourly data
  if(ncol(weeks) == 1){
    
    if(nchar(weeks$date[1]) == 7){
      
      # Sometimes data are returned without a day. Asusme it is first day of month.
      weeks <- as.POSIXct(paste(weeks[, 1], "-01", sep = ""))
      weeks <- data.frame(start = weeks)
      
    }else if(nchar(weeks$date[1]) == 10){
      
      weeks <- as.POSIXct(weeks$date)
      weeks <- data.frame(start = weeks)
      
    }else{
      
      weeks <- as.POSIXct(weeks[, 1], format = "%Y-%m-%d-%H:%M", tz = "UTC")
      weeks <- data.frame(start = weeks)
      
    }
    
  }
  
  trend <- cbind(weeks, trend)
  
  trend <- na.omit(trend)
  
  #---------------------------------------------------------------------
  # Section to deal with geographical data
  #---------------------------------------------------------------------
  
  ## block 3+: geographical info
  start <- 3 # Always start at index 3
  
  blocks <- lapply(start:length(vec), function(i)
    read.csv(
      textConnection(strsplit(vec[i], "\\\n")[[1]]),
      skip = 1,
      stringsAsFactors = FALSE
    ))
  
  
  blocks <- Map(assign, 
                make.names(headers[start:length(headers)]), 
                value = blocks)
  
  res <- list(query = queryparams,
              meta = meta,
              trend = trend,
              headers = headers)

  
  res <- append(res, blocks)
                
 
  # res <- list(
  #   
  #   query = queryparams,
  #   meta = meta,
  #   
  #   trend = trend,
  #   
  #   regions = res[which(types == "State" | types == "Province")],
  #   topmetros = res[which(types == "DMA Region")],
  #   cities = res[which(types == "City")],
  #   
  #   # searches = schlist,
  #   # rising = rislist,
  #   
  #   headers = headers
  # )
  
  # if data was returned monthly, it will not be possible to plot maps
  res[lapply(res, length) == 0]  <- NA
  
  class(res) <- "gtrends"
  return(res)
}


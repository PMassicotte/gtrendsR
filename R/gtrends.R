##-----------------------------------------------------------------------------
##  Authors:        Philippe Massicotte and Dirk Eddelbuettel
##  Date modified:  20-03-2014
##  Description:    TODO
##-----------------------------------------------------------------------------

## TODO:
# - better authentication success checks
# - better query result checks
# - adding category code (as you mentioned)
# - restoring ability to plot regions (ahem, I broke that...)
# - plot googleVis in PDF. Hint: cat(unlist(G1$html), file="tmp.html") -----> print to PDF using system(...)

#' Connect to Google account
#'
#' The resulting connection object is also stored in the package-local
#' environment from which the (internal) helper function
#' \code{.getDefaultConnection()} retrieves it as needed.
#'
#' If the environment variables \code{GOOGLE_USER} and
#' \code{GOOGLE_PASSWORD} are set, they will be retrieved in case no
#' argument has been supplied.  Similarly, the environment variable
#' \code{GOOGLE_PASSWORD} or \code{options("google.password")} can be
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
#' @param geo A character variable denoting a geographic region for the query, 
#'   default to \dQuote{all} for global queries.
#'   
#' @param cat A character denoting the category, defaults to \dQuote{0}.
#'   
#' @param ... Additional parameters passed on in method dispatch.
#'   
#' @param res Resolution of the trending data to be returned. Either \code{week}
#'   for weekly data or \code{day} for daily data.
#'   
#' @param start_date Starting date using yyyy-mm-dd format. Must be breater than
#'   2004-01-01.
#'   
#' @param end_date Starting date using yyyy-mm-dd format. Must be before than 
#'   current date.
#'   
#' @param ch A valid handle which can be created via \code{\link{gconnect}}. 
#'   Users can either supply an explicit handle, or rely on the helper function 
#'   \code{.getDefaultConnection()} to retrieve the current connection handle.
#'   
#' @return An object of class \sQuote{gtrends} which is list with six elements 
#'   containing the results.
#' @examples 
#' \dontrun{
#' ch <- gconnect("usr@gmail.com", "psw")
#' sport_trend <- gtrends(c("NHL", "NBA", "MLB", "MLS"))
#' }
#' @export
gtrends <- function(query, geo, cat, ch, ...) {
  
  UseMethod("gtrends")
    
}

#' @rdname gtrends
#' @export
gtrends.default <- function(query, 
                            geo, 
                            cat, 
                            ch, 
                            res = "week",
                            start_date = as.Date("2004-01-01"),
                            end_date = as.Date(Sys.time()),
                            ...){

  if (missing(geo)) geo <- ""
  if (missing(cat)) cat <- "0"
  if (missing(ch))  ch  <- .getDefaultConnection()

  stopifnot(is.character(query),
            is.vector(query),
            all(res %in% c("week", "day")),
            length(res) == 1,
            length(query) <= 5)
  
  if(is.null(ch)) stop("You are not signed in. Please log in using gconnect().",
                       call. = FALSE)
  
  ## Verify the dates
  start_date <- as.Date(start_date, "%Y-%m-%d")  
  end_date <- as.Date(end_date, "%Y-%m-%d")  
  
  if (is.na(start_date)) {
    stop("start_date is not a valide date. Please use yyyy-mm-dd format.",
         call. = FALSE)
  } 
  
  if (is.na(end_date)) {
    stop("end_date is not a valide date. Please use yyyy-mm-dd format.",
         call. = FALSE)
  } 
  
  # date verification
  stopifnot(start_date < end_date, 
            start_date >= as.Date("2004-01-01"), # cant be earlier than 2004
            end_date <= as.Date(Sys.time())) # cant be more than current date
  
  # if resolution is day then maximum date difference must be less than 3 months
  nmonth <- length(seq(from = start_date, to = end_date, by = "month"))
  
  if(res == "day" & nmonth > 3){
    stop("Maximum of 3 months allowed with daily resolution.", call. = FALSE)
  }
  
  query <- paste(query, collapse = ",")
  
  ## Change encoding to utf-8
  query <- iconv(query, "latin1", "utf-8", sub = "byte")
  
  if (inherits(ch, "CURLHandle") != TRUE) {
    stop("'ch' arguments has to be result from 'gconnect()'.", 
         call. = FALSE)
  }
  
  data(countries, envir = environment())
  
  countries[, 1] <- as.character(countries[, 1])
  countries[, 2] <- as.character(countries[, 2])
  countries[which(countries[, "COUNTRY"] == "Namibia"), "CODE"] <- "NA"
  
  if (geo != "" && !geo %in% countries[, "CODE"]) {
    stop("Country code not valid. Please use 'data(countries)' to retreive valid codes.",
         call. = FALSE)
  }
  
  
  authenticatePage2 <- getURL("http://www.google.com", curl = ch)
  
  trendsURL <- "http://www.google.com/trends/trendsReport?"

  
  res <- paste(nmonth, "m", sep = "")
  
  pp <- list(q = query, 
             cmpt = "q",
             content = 1, 
             export = 1,
             date = paste(format(start_date, "%m/%Y"), res),
             geo = geo)
  
  
  #http://www.google.com/trends/trendsReport?&q=%2Cfoo%2Cbar%2Cbaz%2Cfoo&cmpt=q&content=1&export=1&date=1%2F2015%202m
  
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
#' default), \sQuote{regions} and \sQuote{cities}.
#' @param region A character variable with default
#' \sQuote{world}. Oher permissible value are country codes like
#' \sQuote{CA} or \sQuote{GB}, a US Metro code such as \sQuote{US-IL}
#' or a three-digit code for a continent or sub-continent; see the
#' help for \link[googleVis]{gvisGeoChart} for details.
#' @param resolution A character variable selecting the granularity
#' of the plot; permissble values are \sQuote{countries},
#' \sQuote{provinces} or \sQuote{metros}.
#' @param displaymode A character variable indicating the mode of
#' display, with values \sQuote{auto}, \sQuote{regions} or
#' \sQuote{markers} with latter preferable for cities.
#' @param ind A integer selecting the result set in case of multiple
#' search terms.
#' @import googleVis
#' @import RColorBrewer
#' @import ggplot2
#' @examples 
#' data("sport_trend")
#' plot(sport_trend)
#' @export
plot.gtrends <- function(x,
                         type = c("trend", "regions", "topmetros", "cities"),
                         region = "world",
                         resolution = c("countries", "provinces", "metros"),
                         displaymode = c("auto", "regions", "markers"),
                         ind = 1L,
                         ...) {
  type <- match.arg(type)
  
  resolution <- match.arg(resolution)
  
  gvisopt <- list(region = region,
                  displayMode = "markers",
                  resolution = resolution)
  
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
    
  } else if (type == "regions") {
    
    x <- x[["regions"]][[ind]]
    
    if(all(is.na(x))) stop("Not enough search volume to show results.", 
                      call. = FALSE)
    
    df <- data.frame(loc = x[, 1], hits = x[, 2])
    
    plot(gvisGeoChart(df, 'loc', 'hits', options = gvisopt))
    
  } else if (type == "topmetros") {
    
    x <- x[["topmetros"]][[ind]]
    
    if(all(is.na(x))) stop("Not enough search volume to show results.", 
                      call. = FALSE)
    
    df <- data.frame(loc = x[, 1], hits = x[, 2])
    
    plot(gvisGeoChart(df, 'loc', 'hits', options = gvisopt))
    
  } else if (type == "cities") {
    
    x <- x[["cities"]][[ind]]
    
    if(all(is.na(x))) stop("Not enough search volume to show results.", 
                      call. = FALSE)
    
    df <- data.frame(loc = x[, 1], hits = x[, 2])
    
    plot(gvisGeoChart(df, 'loc', 'hits', options = gvisopt))
  }
  
  invisible(NULL)
}

#' @rdname gtrends
as.zoo.gtrends <- function(x, ...) {
  z <- zoo(x[["trend"]][, -(1:2), drop = FALSE],
           order.by = x[["trend"]][, "end"])
  z
}

.processResults <- function(resultsText, queryparams) {
  
  #get back to latin1 encoding
  queryparams[1] <- iconv(queryparams[1], "utf-8", "latin1", sub = "byte")
  
  vec <- strsplit(resultsText, "\\\n{2,}")[[1]]
  
  ## Make sure there are some results have been returned.
  if (length(vec) < 2) {
    stop("Not enough search volume. Please change your search terms.",
         call. = FALSE)
  }
  
  ## block 1: meta data
  meta  <- strsplit(vec[1], "\\\r\\\n")[[1]]

  #---------------------------------------------------------------------
  # Section to deal with trend data.
  #---------------------------------------------------------------------
    
  ## block 2: trend
  trend <- read.csv(textConnection(strsplit(vec[2], "\\\n")[[1]]),
                    skip = 1,
                    stringsAsFactors = FALSE)
  
  weeks <- data.frame(do.call(rbind, strsplit(trend[, 1], " - ")))
  
  trend <- trend[, mapply(is.numeric, trend), drop = FALSE]
  
  is_weekly <- all(do.call(c, 
                           lapply(weeks, 
                                  grepl, 
                                  pattern = "\\d{4}-\\d{2}-\\d{2}")))
  
  is_daily <- all(do.call(c, 
                           lapply(weeks, 
                                  grepl, 
                                  pattern = "\\d{4}-\\d{2}")))
  
  # data = gtrends(c("nhl", "asasnfassaasl"), "IS") # Bug here with IS
  if(!is_weekly & !is_daily){
    stop("Not enough search volume to show results.", call. = FALSE)
  }
  
  if(is_weekly){
    
    weeks <- lapply(weeks, as.Date, SIMPLIFY = FALSE)
    weeks <- do.call(cbind.data.frame, weeks)
    names(weeks) <- c("start", "end")[1:ncol(weeks)]
  
  }else{
    
    weeks <- paste(weeks[, 1], "-01", sep = "")
    weeks <- data.frame(start = weeks)
    
    message("The number of hits was too low for daily (weekly) resolution. Results were returned using weekly (monthly) resolution instead.")
  }
  
  trend <- cbind(weeks, trend)
  
  # Verify that all requested keywords have been recevied. Sometimes not
  # enough data for some requested kw. If that happen, Google will not 
  # return data.
  wanted_kw <- make.names(tolower(unlist(strsplit(queryparams[1], ","))))
  received_kw <- names(trend)[mapply(is.numeric, trend)]
  
  if(length(setdiff(wanted_kw, received_kw) != 0)){
    message(paste("Not enough data for ", setdiff(wanted_kw, received_kw), 
                  " keyword(s).", sep = "'"))
  }
  
  
  #---------------------------------------------------------------------
  # Section to deal with geographical data
  #---------------------------------------------------------------------
  
  ## results headers -- for 'geo="US"' and three terms, we get 17 results (!!)
  headers <- unname(sapply(vec, function(v) strsplit(v, "\\\n")[[1]][1]))

  #hit_sum <- colSums(trend[, mapply(is.numeric, trend)])
  
  nkw <- length(received_kw)
  ## first set of blocks: top regions
  
  start <- 3 # Always start at index 3
  
  start <- seq(start, start + (nkw - 1))
  
  reglist <-
    lapply(start, function(i)
      read.csv(
        textConnection(strsplit(vec[i], "\\\n")[[1]]),
        skip = 1,
        stringsAsFactors = FALSE
      ))
  
  ## next (optional, if geo==US) block
  if (queryparams["geo"] == "US") {
    
    start <- max(start) + 1
    start <- seq(start, start + (nkw - 1))
    
    #metidx <- grep("Top metros", headers)
    metlist <-
      lapply(start, function(i)
        read.csv(
          textConnection(strsplit(vec[i], "\\\n")[[1]]),
          skip = 1,
          stringsAsFactors = FALSE
        ))
  }else{
    metlist <- NULL
  }
  
  ## next block: top cities
  start <- max(start) + 1
  start <- seq(start, start + (nkw - 1))
  
  citlist <-
    lapply(start, function(i)
      read.csv(
        textConnection(strsplit(vec[i], "\\\n")[[1]]),
        skip = 1,
        stringsAsFactors = FALSE
      ))
  
  ## next block: top searches
  start <- max(start) + 1
  start <- seq(start, start + (nkw - 1))
  
  schlist <-
    lapply(start, function(i)
      read.csv(
        textConnection(strsplit(vec[i], "\\\n")[[1]]),
        skip = 1,
        stringsAsFactors = FALSE,
        header = FALSE
      ))
  
  ## Set columns names
  if(length(start) != 0){
    schlist <- lapply(1:length(start), function(i) {
      names(schlist[[i]]) = c(headers[start][i], "Hits")
      schlist[[i]]
    })
  }
  
  ## nex block: rising searches
  start <- max(start) + 1
  start <- seq(start, start + (nkw - 1))
  
  rislist <- lapply(start, function(i) {
    ## broken by design: not a csv when a field can be "+1,900%" with a comma as
    ## a decimal separator -- so subst out the first comma into a semicolon
    tt <- sub(",", ";", strsplit(vec[i], "\\\n")[[1]])
    rising <- read.csv(
      textConnection(tt),
      sep = ";",
      skip = 1,
      header = FALSE,
      col.names = c("term", "change"),
      stringsAsFactors = FALSE
    )
    rising
  })
  
  res <- list(
    query = queryparams,
    meta = meta,
    trend = trend,
    regions = reglist,
    topmetros = metlist,
    cities = citlist,
    searches = schlist,
    rising = rislist,
    headers = headers
  )
  
  # if data was returned monthly, it will not be possible to plot maps
  res[lapply(res, length) ==0]  <- NA
  
  class(res) <- "gtrends"
  return(res)
}

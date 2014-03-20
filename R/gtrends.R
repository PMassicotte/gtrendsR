##-----------------------------------------------------------------------------
##  Authors:        Philippe Massicotte and Dirk Eddelbuettel
##  Date modified:  20-03-2014
##  Description:    TODO
##-----------------------------------------------------------------------------

gconnect <- function(usr, psw, verbose=FALSE) {
    loginURL <- "https://accounts.google.com/accounts/ServiceLogin"
    authenticateURL <- "https://accounts.google.com/ServiceLoginBoxAuth"
  
    ch <- getCurlHandle()
    
    ans <- (curlSetOpt(curl = ch,
                       ssl.verifypeer = FALSE,
                       useragent = getOption('HTTPUserAgent', "R"),
                       timeout = 60,         
                       followlocation = TRUE,
                       cookiejar = "./cookies",
                       cookiefile = ""))
  
    galx <- .getGALX(ch)
    authenticatePage <- postForm(authenticateURL,
                                 .params=list(Email=usr,
                                 Passwd=psw, GALX = galx,
                                 PersistentCookie= "yes",
                                 continue = "http://www.google.com/trends"), curl=ch)
  
    authenticatePage2 <- getURL("http://www.google.com", curl = ch)
  
    if (getCurlInfo(ch)$response.code == 200) {
        if (verbose) cat("Google login successful!\n")
    } else {
        if (verbose) cat("Google login failed!")
    }
    return(ch)
  
}

## This gets the GALX cookie which we need to pass back in the login form we post.
.getGALX <- function(curl) {
    txt <- basicTextGatherer()
    curlPerform(url = "https://accounts.google.com/accounts/ServiceLogin", 
                curl = curl, writefunction = txt$update, header = TRUE, ssl.verifypeer = FALSE)
    tmp <- txt$value()
  
    val <- grep("Cookie: GALX", strsplit(tmp, "\n")[[1]], value = TRUE)
    strsplit(val, "[:=;]")[[1]][3]
  
    return(strsplit(val, "[:=;]")[[1]][3])
}


##' Perform a Google Trends query
##'
##' The \code{gtrends} default method performs a Google Trends query
##' for the \sQuote{query} argument and handle \sQuote{ch}. Optional
##' arguments for geolocation and category can also be supplied.
##'
##' This function is based on the \sQuote{GTrendsR} package by
##' Philippe Massicotte which can be found at
##' \url{https://bitbucket.org/persican/gtrends}.
##' @title Google Trends Query
##' @param ch A valid handle which can be created via \code{\link{gconnect}}.
##' @param query A character variable with the actual Google Trends query keywords.
##' @param geo A character variable denoting a geographic region for
##' the query, default to \dQuote{all} for global queries.
##' @param cat A character denoting the category, defaults to \dQuote{0}.
##' @param ... Additional parameters passed on in method dispatch.
##' @return An object of class \sQuote{gtrends} which is list with six
##' elements containing the results.
##' @author Dirk Eddelbuettel based on the package by Philippe Massicotte 
##' which can be found at \url{https://bitbucket.org/persican/gtrends}
##' @seealso The original GTrendsR repository at
##' \url{https://bitbucket.org/persican/gtrends}
gtrends <- function(ch, query, geo = 'all', cat = "0", ...) {
    UseMethod("gtrends")
}


## Dirk Eddelbuettel added result processing and turned it into an S3
## method for the 'gtrends' class.

## TODO: Add support for category.

##' @rdname gtrends
gtrends.default <- function(ch, query, geo = 'all', cat = "0", ...) {
  
    ## Make sure a valide country code has been specified.
    data(countries)
    
    if(!geo %in% countries$CODE){
      stop("Country code not valide. Please use 'data(countries)' to retreive valid codes.", call. = FALSE)
    }
  
    authenticatePage2 <- getURL("http://www.google.com", curl = ch)
    
    trendsURL <- "http://www.google.com/trends/?"

    pp <- list(q = query, geo = geo, cat = cat, content = 1, export = 1, graph = 'all_csv')
  
    resultsText <- getForm(trendsURL, .params = pp, curl = ch)
  
    ##print(resultsText)
    ##print(rawToChar(resultsText))
  
    ## Sometimes we reach quota limit, in that case stop!
    if (any(grep("QUOTA", resultsText))) {
        stop("Reached Google Trends quota limit! Please try again later.") 
    }

    res <- .processResults(resultsText)
    res
}

##' @rdname gtrends
##' @param object A \code{\link{gtrends}} object
summary.gtrends <- function(object, ...) {
    cat("Google Trends results for:\n")
    cat(object[[1]][15])
    cat("\nRequested at: ")
    cat(object[[1]][4])
    cat("\n\nSummary of trend:\n")
    print(summary(as.xts.gtrends(object)))
    cat("\nMain regions:\n")
    print(head(object[[3]]))
    cat("\nMain cities:\n")
    print(head(object[[4]]))
    cat("\nTop searches cities:\n")
    print(head(object[[5]]))
    cat("\nRising searches:\n")
    print(head(object[[6]]))
    invisible(NULL)
}

##' @rdname gtrends
##' @param x A \code{\link{gtrends}} object
##' @param type A character variable selecting the type of plot;
##' permissible values are \sQuote{trends} (which is also the
##' default), \sQuote{regions} and \sQuote{cities}.
plot.gtrends <- function(x, type=c("trend", "regions", "cities"), ...) {
    type <- match.arg(type)
    if (type=="trend") {
        x <- as.xts.gtrends(x)
        plot(x, main=colnames(x))
    } else if (type=="regions") {
        df <- data.frame(loc=x$regions[,1], hits=x$regions[,1])
        plot(gvisGeoChart(df, 'loc', 'hits'))
    } else if (type=="cities") {
        df <- data.frame(loc=x$cities[,1], hits=x$cities[,1])
        plot(gvisGeoChart(df, 'loc', 'hits', options=list(displayMode="markers")))
    }
    invisible(NULL)
}

##' @rdname gtrends
as.xts.gtrends <- function(x, ...) {
    z <- xts(x[["trend"]][,3], order.by=x[["trend"]][,"end"])
    colnames(z) <- colnames(x[[2]])[3]
    z
}


## This function has been rewrite and improuved by Dirk Eddelbuettel.

## TODO: If geo is "US" there will be 7 blocs and they won't match the current structure. This happen because block 4 is "Top metros" which is only available when geo = "US".

.processResults <- function(resultsText) {

    vec <- strsplit(resultsText, "\\\n{2,}")[[1]]
    
    ## Make sure there some results have been returned.
    if(length(vec) < 6){
      stop("Not enough search volume. Please change your search terms.", call. = FALSE)
    }

    ## block 1: meta data
    meta  <- strsplit(vec[1], "\\\r\\\n")[[1]]
    
    ## block 2: trend
    trend <- read.csv(textConnection(strsplit(vec[2], "\\\n")[[1]]),
                      skip=1, stringsAsFactors=FALSE)
    weeks <- do.call(rbind, strsplit(trend[,1], " - "))
    trend <- data.frame(start=as.Date(weeks[,1]),
                        end=as.Date(weeks[,2]),
                        trend)
    trend <- trend[is.finite(trend[,4]), -3]
   
    ## block 3: top regions
    regions <- read.csv(textConnection(strsplit(vec[3], "\\\n")[[1]]),
                        skip=1, stringsAsFactors=FALSE)

    ## block 4: top cities
    cities <- read.csv(textConnection(strsplit(vec[4], "\\\n")[[1]]),
                        skip=1, stringsAsFactors=FALSE)

    ## block 5: top searches
    searches <- read.csv(textConnection(strsplit(vec[5], "\\\n")[[1]]),
                         stringsAsFactors=FALSE)

    ## block 6: rising searches
    ## broken by design: not a csv when a field can be "+1,900%" with a comma as
    ## a decimal separator -- so subst out the first comma into a semicolon
    tt <- sub(",", ";", strsplit(vec[6], "\\\n")[[1]])
    rising <- read.csv(textConnection(tt),
                       sep=";", skip=1, header=FALSE,
                       col.names=c("term", "change"),
                       stringsAsFactors=FALSE)

    res <- list(meta=meta,
                trend=trend,
                regions=regions,
                cities=cities,
                searches=searches,
                rising=rising)
    class(res) <- "gtrends"
    return(res)
}

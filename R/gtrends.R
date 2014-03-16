
## This gets the GALX cookie which we need to pass back in the login form we post.
.getGALX <- function(curl) {
    txt <- basicTextGatherer()
    curlPerform(url = "https://accounts.google.com/accounts/ServiceLogin", 
                curl = curl, writefunction = txt$update, header = TRUE, ssl.verifypeer = FALSE)
    tmp <- txt$value()
  
    val <- grep("Cookie: GALX", strsplit(tmp, "\n")[[1]], value = TRUE)
    strsplit(val, "[:=;]")[[1]][3]

    ## Phil
    return(strsplit(val, "[:=;]")[[1]][3])
}

gConnect <- function(usr, psw, verbose=FALSE) {
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

gtrends <- function(ch, query, geo = 'all', cat = "0") {
    authenticatePage2 <- getURL("http://www.google.com", curl = ch)
  
    ## get Google Insights results CSV
    ##trendsURL <- "http://www.google.com/trends/viz?"
  
    ##trendsURL <- "http://www.google.com/trends/TrendsRepport?"
    trendsURL <- "http://www.google.com/trends/?"
    ## resultsText <- getForm(trendsURL, .params = list(q = query, geo = geo, export = 1,
    ##                                   hl = 'en', content=1, graph = 'all_csv'), curl = ch,
    ##                        .opts = list(verbose = F))
  
    pp <- list(q = query, geo = geo, cat = cat, content = 1, export = 1, graph = 'all_csv')
  
    resultsText <- getForm(trendsURL, .params = pp, curl = ch)
    
  
    ##print(resultsText)
  
    ##print(rawToChar(resultsText))
  
    ## Sometimes we reach quota limit, in that case stop!
    if (any(grep("QUOTA", resultsText))) {
        stop("Reached Google Trends quota limit! Please try again later.") 
    }
    
    ##resultsText = gFormatTrends2(resultsText)
    ##
    ##resultsText$GEO = geo
    ##return(resultsText)

    res <- .processResults(resultsText)
    res
}



.processResults <- function(resultsText) {

    vec <- strsplit(resultsText, "\\\n{2,}")[[1]]

    ## block 1: meta data
    meta  <- strsplit(vec[1], "\\\r\\\n")
    
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
    rising <- read.csv(textConnection(strsplit(vec[6], "\\\n")[[1]]),
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

gtrends <- function(x) UseMethod("gtrends")
gtrends.default <- function(x) {
    plot.gtrends(x)
}
plot.gtrends <- function(x, ...) {
    x <- as.xts.gtrends(x)
    plot(x, main=colnames(x))
}
as.xts.gtrends <- function(res) {
    x <- xts(res[[2]][,3], order.by=res[[2]][,"end"])
    colnames(x) <- colnames(res[[2]])[3]
    x
}


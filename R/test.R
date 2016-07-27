library(RCurl)
library(stringr)

rm(list = ls())

usr = "persican79@gmail.com"
psw = "rvrbnvv1983147"

loginURL <- "https://accounts.google.com/accounts/ServiceLogin"

authenticateURL <- "https://accounts.google.com/ServiceLoginBoxAuth"

ch <- getCurlHandle()

ans <- curlSetOpt(curl = ch,
                  ssl.verifypeer = FALSE,
                  useragent = getOption('HTTPUserAgent', "R"),
                  timeout = 60,         
                  followlocation = TRUE,
                  cookiejar = "./cookies",
                  cookiefile = "")

# galx <- .getGALX(ch)

loginPage <- getURL(loginURL, curl = ch)

galx.match <- str_extract(string = loginPage,
                          pattern = ('name="GALX"\\s*value="([^"]+)"'))
galx <- str_replace(string = galx.match,
                    pattern = ('name="GALX"\\s*value="([^"]+)"'),
                    replacement = "\\1")
galx

formparams <- list(Email = usr,
                   Passwd = psw,
                   GALX = galx,
                   PersistentCookie = "yes",
                   continue = "http://www.google.com/trends")

authenticatePage <- postForm(authenticateURL, .params = formparams, curl = ch)
getCurlInfo(ch)$response.code

authenticatePage2 <- getURL("https://www.google.com/accounts/CheckCookie?chtml=LoginDoneHtml", curl = ch)
getCurlInfo(ch)$response.code

authenticatePage3 <- getURL("https://www.google.com", curl = ch)
getCurlInfo(ch)$response.code


pp <-
  structure(
    list(
      q = "nhl",
      cat = "0",
      cmpt = "geo",
      content = 1,
      export = 1,
      date = "01/2004 150m",
      geo = "BR"
    ),
    .Names = c("q", "cat", "cmpt", "content", "export", "date", "geo"))


trendsURL <- "http://www.google.com/trends/trendsReport?"

resultsText <- getForm(trendsURL, .params = pp, curl = ch)
resultsText

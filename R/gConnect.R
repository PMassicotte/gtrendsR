gConnect <-
function(usr, psw)
{
  loginURL <- "https://accounts.google.com/accounts/ServiceLogin"
  authenticateURL <- "https://accounts.google.com/accounts/ServiceLoginAuth"
  
  require(RCurl)
  
  ch <- getCurlHandle()
  
#   curlSetOpt(curl = ch,
#              ssl.verifypeer = FALSE,
#              useragent = "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; en-US; rv:1.9.2.13) Gecko/20101203 Firefox/3.6.13",
#              timeout = 60,
#              followlocation = TRUE,
#              cookiejar = "./cookies",
#              cookiefile = "./cookies")
  
  curlSetOpt(curl = ch,
             ssl.verifypeer = FALSE,
             useragent = "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; en-US; rv:1.9.2.13) Gecko/20101203 Firefox/3.6.13",
             timeout = 60,
             followlocation = TRUE,
             cookiejar = "./cookies",
             cookiefile = "./cookies")
  
  ## do Google Account login
  loginPage <- getURL(loginURL, curl = ch)
  
  require(stringr)
  galx.match <- str_extract(string = loginPage,
                            pattern = ignore.case('name="GALX"\\s*value="([^"]+)"'))
  galx <- str_replace(string = galx.match,
                      pattern = ignore.case('name="GALX"\\s*value="([^"]+)"'),
                      replacement = "\\1")
  
  authenticatePage <- postForm(authenticateURL, .params = list(Email = usr, Passwd = psw, GALX = galx), curl = ch, .opts = list(verbose = F))
  
  
  
  return(ch)
}

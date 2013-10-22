gConnect = function(usr, psw)
{
  loginURL <- "https://accounts.google.com/ServiceLoginBoxAuth"
  authenticateURL <- "https://accounts.google.com/accounts/ServiceLoginAuth"
  
  require(RCurl)
  
  ch <- getCurlHandle()
  
  curlSetOpt(curl = ch,
             ssl.verifypeer = FALSE,
             useragent = "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; en-US; rv:1.9.2.13) Gecko/20101203 Firefox/3.6.13",
             timeout = 60,
             httpheader = list(continue = "http://www.google.com/trends"),
             followlocation = TRUE,
             cookiejar = "./cookies",
             cookiefile = "./cookies")
  
  
  ## Perform Google Account login
  loginPage <- getURL(loginURL, curl=ch)
  
  galx.match <- str_extract(string = loginPage,
                            pattern = ignore.case('name="GALX"\\s*value="([^"]+)"'))
  

  galx <- str_replace(string = galx.match,
                      pattern = ignore.case('name="GALX"\\s*value="([^"]+)"'),
                      replacement = "\\1")
  
  authenticatePage <- postForm(authenticateURL, .params=list(Email=usr, Passwd=psw, GALX = galx, PersistentCookie= "yes", continue = "http://www.google.com/trends"), curl=ch)
  
  
  if(getCurlInfo(ch)$response.code == 200) {
    print("Google login successful.")
  } else {
    print("Google login not successful. Check your login credentials!")
  }
  
  return(ch)
}

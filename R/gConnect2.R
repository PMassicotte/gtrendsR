gConnect2 = function(usr, psw)
{
  loginURL <- "https://accounts.google.com/accounts/ServiceLogin"
  authenticateURL <- "https://accounts.google.com/ServiceLoginBoxAuth"
  
  require(RCurl)
  
  ch <- getCurlHandle()
  
  
  
  ans = (curlSetOpt(curl = ch,
                    ssl.verifypeer = FALSE,
                    useragent = getOption('HTTPUserAgent', "R"),
                    timeout = 60,         
                    followlocation = TRUE,
                    cookiejar = "./cookies",
                    cookiefile = ""))
  
  galx = getGALX(ch)
  authenticatePage <- postForm(authenticateURL, .params=list(Email=usr, Passwd=psw, GALX = galx, PersistentCookie= "yes", continue = "http://www.google.com/trends"), curl=ch)
  
  authenticatePage2 = getURL("http://www.google.com", curl = ch)
  
  if(getCurlInfo(ch)$response.code == 200) {
    print("Google login successful!")
  } else {
    print("Google login failed!")
  }
  
  return(ch)
}

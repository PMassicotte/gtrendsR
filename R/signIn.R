
getGALX = 
  # This gets the GALX cookie which we need to pass back in the login form we post.
function(curl)
{
  txt = basicTextGatherer()
  curlPerform(url = "https://accounts.google.com/accounts/ServiceLogin", 
              curl = curl, writefunction = txt$update, header = TRUE, ssl.verifypeer = FALSE)

  tmp = txt$value()
  
  val = grep("Cookie: GALX", strsplit(tmp, "\n")[[1]], val = TRUE)
  (strsplit(val, "[:=;]")[[1]])[3]

  print(val)
}



googleSignIn =
function(login = getOption("GooglePassword"), password, service = "trends",
         curl = getCurlHandle(cookiefile = "", followlocation = TRUE, ...),
         userAgent = getOption('HTTPUserAgent', "R"), ssl.verifypeer = FALSE,
         GALX = getGALX(curl), ...) 
{
   if(missing(password) && length(names(login)) > 0) {
       password = login
       login = names(login)
   }
       
   ans = postForm("https://accounts.google.com/accounts/ServiceLoginAuth", 
                 Email = login,
                 Passwd = password,
                 GALX = GALX,
                 nui = "0", hl = 'en',
                 continue = "http://www.google.com/",
                 PersistentCookie = "no", rmShown = "0", asts="",
                 service = service, curl = curl,
                 .opts = list(header = TRUE,
                              httpheader = c('User-Agent' = userAgent)))

   curl
}

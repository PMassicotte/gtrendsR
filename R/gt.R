library(jsonlite)

# ****************************************************************************
# Request a token from Google
# ****************************************************************************
payload <- list()

keyword <- c("one", "two", "three")
# time <- "today+5-y"
time <- "2017-02-09 2017-02-18"
# time <- "now 7-d"
geo <- c("CA", "FR", "US")

df <- data.frame(keyword, geo, time)

payload$comparisonItem <- df
payload$category <- 0

url <- URLencode(paste0("https://www.google.com/trends/api/explore?property=&req=", toJSON(payload, auto_unbox = T), "&tz=360&hl=en-US"))

res <- curl::curl_fetch_memory(url)

myjs <- fromJSON(substring(rawToChar(res$content), first = 6))
widget <- myjs$widgets

widget$token

# ****************************************************************************
# Now that we have a token, we can process the query
# ****************************************************************************

payload2 <- list()
payload2$locale <- unique(na.omit(widget$request$locale))
payload2$comparisonItem <- widget$request$comparisonItem[[1]]
payload2$resolution <- widget$request$resolution[1]
payload2$requestOptions$category <- unique(na.omit(widget$request$requestOptions$category))
payload2$requestOptions$backend <- unique(na.omit(widget$request$requestOptions$backend))
payload2$time <- unique(na.omit(widget$request$time))


url1 <- paste0(
  "https://www.google.fr/trends/api/widgetdata/multiline/csv?req=",
  toJSON(payload2, auto_unbox = T),
  "&token=", widget$token[1],
  "&tz=360"
)

# ****************************************************************************
# Downoad the results
# ****************************************************************************

res <- curl::curl_fetch_memory(URLencode(url1))

res <- read.csv(textConnection(rawToChar(res$content)), skip = 1, stringsAsFactors = FALSE)

stopifnot(res$status_code == 200)

nrow(res)

res2 <- reshape(
  res,
  varying = names(res)[2:4],
  v.names = "hits",
  direction = "long",
  timevar = "temp",
  times = names(res)[2:4]
)

res2 <- res2[, -2]

res2 <- cbind(res2, df[rep(seq_len(nrow(df)), each = nrow(res)), 1:2])
row.names(res2) <- NULL 
names(res2)[1] <- "date"

head(res2)

# Oh, it works!
res2$date <- anytime::anytime(res2$date)

library(ggplot2)

ggplot(res2, aes(x = date, y = hits)) +
  geom_line(aes(color = paste(keyword, " (", geo, ")", sep = ""))) +
  theme(legend.title = element_blank())


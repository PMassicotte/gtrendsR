## Using gtrendsR behind a PROXY.

The function "setHandleParameters" is needed to set proxy parameters

### Example


``` {.r}
library(gtrendsR)

gtrendsR::setHandleParameters(user="victor",password="*******",domain="mydomain",proxyhost = "10.111.124.113",proxyport = 8080,proxyauth = 8)

res <- gtrends(c("nhl", "nba"), geo = c("CA", "US"))

```


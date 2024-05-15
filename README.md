
## gtrendsR 
[![GitHub Actions Build Status](https://github.com/PMassicotte/gtrendsR/actions/workflows/ci.yaml/badge.svg)](https://github.com/PMassicotte/gtrendsR/actions/workflows/ci.yaml)
[![License](https://eddelbuettel.github.io/badges/GPL2+.svg)](http://www.gnu.org/licenses/gpl-2.0.html) 
[![CRAN](http://www.r-pkg.org/badges/version/gtrendsR)](https://cran.r-project.org/package=gtrendsR) 
[![Downloads](http://cranlogs.r-pkg.org/badges/gtrendsR?color=brightgreen)](https://www.r-pkg.org:443/pkg/gtrendsR)

`gtrendsR` provides an interface for retrieving and displaying Google Trends information. 

Trends (number of hits) over time as well as geographic representation of the results can be displayed.

### Example

In this simple example, trends for keywords `nhl`, `nba` are retrieved for Canada and USA and then plotted from R.

``` {.r}
library(gtrendsR)

res <- gtrends(c("nhl", "nba"), geo = c("CA", "US"))
plot(res)
```

### Installation

Since release 1.3.0, the package is on [CRAN](https://cran.r-project.org) and
can be installed via

``` {.r}
install.packages("gtrendsR")
```

Release-candidate packages are available in the [ghrr drat repository](https://ghrr.github.io/drat/)
and can installed via

```r
install.packages("drat")       # easier repo access + creation
drat:::add("ghrr")             # make it known
install.packages("gtrendsR")   # install it
```

Development version (which may be less stable) can be installed directly from this repository via

``` {.r}
if (!require("devtools")) install.packages("devtools")
devtools::install_github("PMassicotte/gtrendsR")
```

## Using gtrendsR behind a PROXY.

If gtrendsR should be used behind a proxy, especially with NTLM authentication mode,
you need to set the proxy parameters using "setHandleParameters" function

### Example

``` {.r}
library(gtrendsR)

setHandleParameters(user = "xxxx", password = "*******", domain = "mydomain", proxyhost = "10.111.124.113", proxyport = 8080)
res <- gtrends(c("nhl", "nba"), geo = c("CA", "US"))
```

## Additional information

- [Limits and quotas on API requests](https://developers.google.com/analytics/devguides/reporting/mcf/v3/limits-quotas)

### Authors

Philippe Massicotte and Dirk Eddelbuettel

### License

GPL (>= 2)

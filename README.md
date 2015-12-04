
## gtrendsR [![Travis-CI Build Status](https://api.travis-ci.org/PMassicotte/gtrendsR.svg?branch=master)](https://travis-ci.org/PMassicotte/gtrendsR) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/PMassicotte/gtrendsR?branch=master&svg=true)](https://ci.appveyor.com/project/PMassicotte/gtrendsR) [![Package-License](https://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html) [![CRAN](http://www.r-pkg.org/badges/version/gtrendsR)](http://cran.rstudio.com/package=gtrendsR) [![Downloads](http://cranlogs.r-pkg.org/badges/gtrendsR?color=brightgreen)](http://www.r-pkg.org/pkg/gtrendsR)

`gtrendsR` provides an interface for retrieving and displaying Google Trends
information.

Trends (number of hits) over time as well as geographic representation of the results can be displayed.

### Example

In this simple example, trends for keywords `nhl`, `nba` and `nfl` are
retrieved and then plotted from R.

``` {.r}
library(gtrendsR)
usr <- "user@gmail.com"  # alternatively store as options() or env.var
psw <- "password"        # idem
gconnect(usr, psw)       # stores handle in environment
sport_trend <- gtrends(c("nhl", "nba", "nfl"))
plot(sport_trend)        # data set also included in package
```

![result of sport_trend query](https://raw.githubusercontent.com/PMassicotte/gtrendsR/master/inst/images/sport_trend_2015-11.png)

It is also possible to plot geographical data using `googleVis` as follow.

``` {.r}
plot(sport_trend, type = "region")
plot(sport_trend, type = "cities")
```

Should you have trouble connecting, and also use two-factor authentication on
your Google Account, then consider creating another Google account (without
two-factor authentication) which should allow automated (_i.e._ programmatic)
connection here.

### Installation

Since release 1.3.0, the package is on [CRAN](http://cran.r-project.org) and
can be installed via

``` {.r}
install.packages("gtrendsR")
```

Pre-release versions can be install directly from this repository via

``` {.r}
if (!require("devtools")) install.packages("devtools")
devtools::install_github("PMassicotte/gtrendsR")
```

### Authors

Philippe Massicotte and Dirk Eddelbuettel

### License

GPL (>= 2)


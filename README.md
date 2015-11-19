
## gtrendsR [![Travis-CI Build Status](https://api.travis-ci.org/PMassicotte/gtrendsR.svg?branch=master)](https://travis-ci.org/PMassicotte/gtrendsR) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/PMassicotte/gtrendsR?branch=master&svg=true)](https://ci.appveyor.com/project/PMassicotte/gtrendsR) [![Package-License](https://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)


`gtrendsR` provides an interface for retrieving and displaying the information returned online by Google Trends. Trends (number of hits) over the time as well as geographic representation of the results can be displayed.

### Installation

The package can be installed using the following command:

``` {.r}
devtools::install_github("PMassicotte/gtrendsR")
```

There are two main functions in the package: 
- `gconnect()` to connect to Google using your credentials, and 
- `gtrends()` to perform actual queries.

``` {.r}
library(gtrendsR)
ls("package:gtrendsR")
#> [1] "gconnect" "gtrends"
```

### Examples

In this simple example, trends for keywords `nhl`, `nba` and `nfl` are retrieved.

#### Get the data

``` {.r}
library(gtrendsR)

usr <- "user@gmail.com"  # alternatively store as options() or env.var
psw <- "password"        # idem

gconnect(usr, psw)       # stores handle in environment

sport_trend <- gtrends(c("nhl", "nba", "nfl"))
```

#### Plot the data

``` {.r}
data("sport_trend")
plot(sport_trend)
```

![result of sport_trend query](inst/images/sport_trend_2015-11.png)

It is also possible to plot geographical data using `googleVis` as follow.

``` {.r}
plot(sport_trend, type = "region")
plot(sport_trend, type = "cities")
```

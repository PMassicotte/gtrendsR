\encoding{UTF-8}
\name{GTrendsR-package}
\alias{GTrendsR-package}
\alias{GTrendsR}
\docType{package}
\title{
GTrendsR: Performs Google Trends queries

}
\description{
Performs and display Google Trends queries using this sequence:\cr  
\cr 1) gConnect
\cr 2) gTrends
\cr 3) gTrendsMap
}
\details{
\tabular{ll}{
Package: \tab GTrendsR\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-05-29\cr
License: \tab Unlimited\cr
}
GTrendsR provides an interface for retrieving and displaying the information returned online by Google Trends in the R console. Trends (number of hits) over the time as well as geographic representation of the results can be displayed.
}
\author{
Philippe Massicotte \email{philippe.massicotte@uqtr.ca} with contributions from Raphaël Proulx and Marc Pépino.
}
\references{
This package is based on code from different projects.

https://github.com/abelsonlive/HIScore/blob/master/InsightsAPI.R
https://github.com/suryasev/unofficial-google-trends-api/issues/5
https://github.com/suryasev/unofficial-google-trends-api/blob/master/pyGTrends.py
}

\keyword{ package }
\examples{
\dontrun{
usr = "yourname@gmail.com"
psw = "yourpassword"
ch = gConnect(usr, psw)

location = "CA"
query = "NHL"

data = gTrends(ch, geo = location, query = query)

gTrendsMap(data)
}
}

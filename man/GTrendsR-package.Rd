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

  NB: gTrendsMap currently disabled.
}
\details{
  \tabular{ll}{
    Package: \tab GTrendsR\cr
    Type: \tab Package\cr
    Version: \tab 1.0\cr
    Date: \tab 2013-05-29\cr
    License: \tab Unlimited\cr
  }
  GTrendsR provides an interface for retrieving and displaying the
  information returned online by Google Trends in the R console. Trends
  (number of hits) over the time as well as geographic representation of
  the results can be displayed.

  This version of the package is based on a fork by Dirk Eddelbuettel,
  which was based on version 1.2 of the package above.
}
\author{
  Philippe Massicotte \email{philippe.massicotte@uqtr.ca} with
  contributions from Raphaël Proulx and Marc Pépino.

  The package was forked by Dirk Eddelbuettel who changed the result
  parsing, added \code{\link[xts]{xts}} support to trends as a time
  series, and added some S3 methods and generics.
}
\references{
  This package is based on code from different projects.

  \url{https://github.com/abelsonlive/HIScore/blob/master/InsightsAPI.R}
  \url{https://github.com/suryasev/unofficial-google-trends-api/issues/5}
  \url{https://github.com/suryasev/unofficial-google-trends-api/blob/master/pyGTrends.py}

  The original GTrendsR repository is at \url{https://bitbucket.org/persican/gtrends}.
}

\keyword{package}
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

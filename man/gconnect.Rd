\encoding{UTF-8}
\name{gconnect}
\alias{gconnect}
\title{Open a connection with Google Trends}
\description{
  Create a connection to Google Trends service using personal account
  information. In order to perform Google Trends queries, the user needs
  to own a free Google account. 
}
\usage{
  gconnect(usr=NULL, psw=NULL, verbose=FALSE)
}
\arguments{
  \item{usr}{A valid Google username (such as a GMail email
    address). The default value is \code{NULL}, and the environment
    variable \code{GOOGLE_USER}, as well as the R option
    \code{google.user} can be used to supply a value.}
  \item{psw}{A valid password associated with \code{usr}. The default
    value is \code{NULL}, and the environment variable
    \code{GOOGLE_PASSWORD}, as well as the R option
    \code{google.password} can be used to supply a value.} 
  \item{verbose}{A boolean toggle selecting quiet or verbose behaviour.}
}
\value{
  Upon a successful connection, a valid handle is returned which can be used to perform
  queries with \code{gtrends}.  
}
\author{
  Philippe Massicotte \email{philippe.massicotte@uqtr.ca} and Dirk Eddelbuettel \email{edd@debian.org} with
  contributions from Raphaël Proulx and Marc Pépino. 
}
\seealso{
  The function \code{\link[RCurl]{getCurlHandle}} in the package
  \pkg{RCurl}.
}
\examples{
\dontrun{
ch <- gconnect(usr, psw)     # will query environment and R options
}
}

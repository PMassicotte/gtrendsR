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
  gconnect(usr, psw, verbose=FALSE)
}
\arguments{
  \item{usr}{A valid Google username (such as a GMail email address).}
  \item{psw}{A valid password associated with \code{usr}.}
  \item{verbose}{A boolean toggle selecting quiet or verbose behahviour}
}
\value{
  Upon a successful connection, returns a valid handle used to perform
  queries with \code{gtrends}.  
}
\author{
  Philippe Massicotte \email{philippe.massicotte@uqtr.ca} with
  contributions from Raphaël Proulx and Marc Pépino. 
}
\examples{
\dontrun{
usr = "yourname@gmail.com"
psw = "yourpassword"
ch = gConnect(usr, psw)
}
}

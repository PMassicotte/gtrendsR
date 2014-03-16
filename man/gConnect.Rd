\encoding{UTF-8}
\name{gConnect}
\alias{gConnect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Open a connection with Google
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Create a connection to Google Trends service using personal account information. In order to perform Google Trends queries, the user needs to own a free Google account.
}
\usage{
gConnect(usr, psw, verbose=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{usr}{
A valid Google username (email).
}
  \item{psw}{
A valid password associated with \code{usr}. 
}
  \item{verbose}{A boolean toggle selecting quiet or verbose behahviour}
}
\value{
Upon a successful connection, returns a valid handle used to perform queries with \code{gTrends}. 
}
\references{
%% ~put references to the literature/web site here ~
Add link to the paper.
}
\author{
Philippe Massicotte \email{philippe.massicotte@uqtr.ca} with contributions from Raphaël Proulx and Marc Pépino.
}
\examples{
\dontrun{
usr = "yourname@gmail.com"
psw = "yourpassword"
ch = gConnect(usr, psw)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

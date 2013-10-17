\encoding{UTF-8}
\name{gTrendsMap}
\alias{gTrendsMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot the trends and the distribution maps
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function allows to rapidly visualize data returned by \code{gTrends}.  A graph with three (3) panels is displayed. The upper panel shows a graph of the search-hit over time. The middle panel shows a distribution map (gvisGeoMap) at the country level. The bottom panel shows a distribution map (gvisGeoMap) at the region level.
}
\usage{
gTrendsMap(gTrendsData)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gTrendsData}{
%%     ~~Describe \code{gTrendsData} here~~
A list of data returned by gTrends.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Philippe Massicotte \email{philippe.massicotte@uqtr.ca} with contributions from Raphaël Proulx and Marc Pépino.
}
\note{
%%  ~~further notes~~
Note that the \code{gTrendsMap} produces a \emph{html} file which will be automatically opened in your browser.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
usr = "yourname@gmail.com"
psw = "yourpassword"
ch = gConnect(usr, psw)

location = "CA"
query = "NHL"

data = gTrends(ch, geo = location, query = query)

gTrendsMap(data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

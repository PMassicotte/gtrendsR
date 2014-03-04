\encoding{UTF-8}
\name{gTrends}
\alias{gTrends}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Performs Google Trends queries
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
A Google Trends query is made using the \code{gTrends} function. The user
provides three parameters. First, a valid Google service connection \code{ch}
returned by the function \code{gConnect}. The second parameter \code{geo} is the geographic
location where the query will be performed. This argument should be a string
of two characters. To obtain a list of valid country codes supported by Google
Trends, the user can type \code{data(countries)} followed by \code{countries}. The third parameter is \code{query}, the keyword string to search for.
}
\usage{
gTrends(ch, geo = "all", category = "0", "query")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ch}{
A valid handle returned by \emph{gConnect}.
}
  \item{geo}{
A two characters string containing the geographical localisation for which the query will be performed. A list of country codes supported by Google Trends can be obtained by typing \code{data(countries)}.
}
  \item{query}{
A keyword string. Should be separated by a comma for multiple keywords query (ex.: \code{query = "keyword1, keyword2"}).
}

  \item{category}{
A string used to specify the category in which to search. \code{category = "0"} by default. List of supported categories can be obtained by typing \code{data(categories)}.
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
An object (list) of class \code{gTrendsData} with the following information:

\item{SearchInfo}{Generic information about the keyword, the localisation and the period for which the query was performed.}
\item{WeeklyHits}{A data frame containing raw data of the search-hits over time.}
\item{MonthlyHits}{A data frame containing data aggregated on a monthly basis. The normalized search-hits of the query re-scaled by dividing the number of search hits obtained for a given country, or region, by the maximum number of hits obtained over the specified period. This temporal series matches the visual display provided by the Google Trends service.}
\item{Regions}{A data frame containing search-hits by region.}
\item{Cities}{A data frame containing search-hits by cities.}
\item{TopSearches}{A data frame containing other related keyword(s) most closely associated to the query.}

}
\references{
%% ~put references to the literature/web site here ~
Add link to the paper.
}
\author{
Philippe Massicotte \email{philippe.massicotte@uqtr.ca} with contributions from Raphaël Proulx and Marc Pépino.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#-----------------------------------------
## Example 1 (single keyword)
#-----------------------------------------
location = "CA"
query = "NHL"

data = gTrends(ch, geo = location, query = query)

## Display maps
gTrendsMap(data)

#-----------------------------------------
## Example 2 (multiple keywords)
#-----------------------------------------
query = c("nhl,pool,khl") ## two keywords search
data = gTrends(ch, geo = location, query = query)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{gtrends}
\alias{as.zoo.gtrends}
\alias{gtrends}
\alias{gtrends.default}
\alias{plot.gtrends}
\alias{summary.gtrends}
\title{Google Trends Query}
\usage{
gtrends(ch, query, geo = "all", cat = "0", ...)

\method{gtrends}{default}(ch, query, geo = "all", cat = "0", ...)

\method{summary}{gtrends}(object, ...)

\method{plot}{gtrends}(x, type = c("trend", "regions", "topmetros", "cities"),
  region = "world", resolution = c("countries", "provinces", "metros"),
  displaymode = c("auto", "regions", "markers"), ind = 1L, ...)

\method{as.zoo}{gtrends}(x, ...)
}
\arguments{
  \item{ch}{A valid handle which can be created via
  \code{\link{gconnect}}.}

  \item{query}{A character variable with the actual Google
  Trends query keywords.}

  \item{geo}{A character variable denoting a geographic
  region for the query, default to \dQuote{all} for global
  queries.}

  \item{cat}{A character denoting the category, defaults to
  \dQuote{0}.}

  \item{...}{Additional parameters passed on in method
  dispatch.}

  \item{object}{A \code{\link{gtrends}} object}

  \item{x}{A \code{\link{gtrends}} object}

  \item{type}{A character variable selecting the type of
  plot; permissible values are \sQuote{trends} (which is
  also the default), \sQuote{regions} and \sQuote{cities}.}

  \item{region}{A character variable with default
  \sQuote{world}. Oher permissible value are country codes
  like \sQuote{CA} or \sQuote{GB}, a US Metro code such as
  \sQuote{US-IL} or a three-digit code for a continent or
  sub-continent; see the help for
  \link[googleVis]{gvisGeoChart} for details.}

  \item{resolution}{A character variable selecting the
  granularity of the plot; permissble values are
  \sQuote{countries}, \sQuote{provinces} or
  \sQuote{metros}.}

  \item{displaymode}{A character variable indicating the
  mode of display, with values \sQuote{auto},
  \sQuote{regions} or \sQuote{markers} with latter
  preferable for cities.}

  \item{ind}{A integer selecting the result set in case of
  multiple search terms.}
}
\value{
An object of class \sQuote{gtrends} which is list with six
elements containing the results.
}
\description{
Perform a Google Trends query
}
\details{
The \code{gtrends} default method performs a Google Trends
query for the \sQuote{query} argument and handle
\sQuote{ch}. Optional arguments for geolocation and
category can also be supplied.

This function is based on the \sQuote{GTrendsR} package by
Philippe Massicotte which can be found at
\url{https://bitbucket.org/persican/gtrends}.
}
\author{
Philippe Massicotte and Dirk Eddelbuettel
}
\seealso{
The original GTrendsR repository at
\url{https://bitbucket.org/persican/gtrends}
}


\name{gtrends}
\alias{as.xts.gtrends}
\alias{gtrends}
\alias{gtrends.default}
\alias{plot.gtrends}
\title{Google Trends Query}
\usage{
gtrends(ch, query, geo = "all", cat = "0", ...)

\method{gtrends}{default}(ch, query, geo = "all", cat = "0", ...)

\method{plot}{gtrends}(x, ...)

\method{as.xts}{gtrends}(x)
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

  \item{x}{A \code{\link{gtrends}} object}
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
Dirk Eddelbuettel
}


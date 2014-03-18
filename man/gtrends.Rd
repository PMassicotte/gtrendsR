\name{gtrends}
\alias{as.xts.gtrends}
\alias{gtrends}
\alias{gtrends.default}
\alias{plot.gtrends}
\alias{summary.gtrends}
\title{Google Trends Query}
\usage{
gtrends(ch, query, geo = "all", cat = "0", ...)

\method{gtrends}{default}(ch, query, geo = "all", cat = "0", ...)

\method{summary}{gtrends}(object, ...)

\method{plot}{gtrends}(x, type = c("trend", "regions", "cities"), ...)

\method{as.xts}{gtrends}(x, ...)
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
Dirk Eddelbuettel based on the package by Philippe
Massicotte which can be found at
\url{https://bitbucket.org/persican/gtrends}
}
\seealso{
The original GTrendsR repository at
\url{https://bitbucket.org/persican/gtrends}
}


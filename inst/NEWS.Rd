\name{NEWS}
\title{News for Package 'GTrendsR'}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\section{Changes in GTrendsR version 1.3.1 (2014-03-24)}{
  \itemize{
    \item Fixed a bug with the top search results.
  }
}

\section{Changes in GTrendsR version 1.3 (2014-03-23)}{
  \itemize{
    \item The resulting object is now a generic S3 class of type \code{gtrends}.
    \item The default method executes the Google Trends query, and
    returns the result objects.
    \item A plot method has been added which defaults to showing the
    actual trend series as a \cpkg{zoo} object.
    \item The plot method also has options to display the regions and
    cities result via \cpkg{googleVis}.
    \item The \code{gconnect} function can now consult two environment
    variables, and R options, for default user and password settings.
    \item Dirk Eddelbuettel joined as a co-author.
  }
}

\section{Changes in GTrendsR version 1.2 (2014-03-21)}{
  \itemize{
    \item Now allows to search for multiple keywords. See \code{?gtrends}.
  }
}

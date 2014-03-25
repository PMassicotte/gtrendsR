\name{NEWS}
\title{News for Package 'GTrendsR'}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\section{Changes in [UNRELEASED/UNFINISHED] GTrendsR version 1.3 (2014-03-25)}{
  \itemize{
    \item Searches now allow for multiple keywords, see
    \code{?gtrends}. 
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

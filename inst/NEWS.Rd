\name{NEWS}
\title{News for Package 'GTrendsR'}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\section{Changes in RGoogleTrends version 0.1.0 (2014-03-17)}{
  \itemize{
    \item This package was created by Dirk Eddelbuettel as a fork of the
    GTrendsR project, version 1.2.0, by Philippe Massicotte with
    contributions from Raphael Proulx and Marc Pepino which can be found at
    \url{https://bitbucket.org/persican/gtrendsr}. 
    \item The resulting object is now a generic S3 class of type \code{gtrends}
    \item The default method executes the Google Trends query, and
    returns the result objects.
    \item A plot method has been added which defaults to showing the
    actual trend series as a weekly \cpkg{xts} object.
    \item The plot method also has options to display the regions and
    cities result via \cpkg{googleVis}.
  }
}

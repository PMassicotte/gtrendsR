#' Google Trends Search
#'
#' Search Google Trends to retrieve relative hits and popularity. This supports comparison of up to 5 search terms. Anything more than that will be searched individually.
#'
#' @param search_terms A character vector containing the search terms of interest
#' @param from The beginning date of the query
#' @param to The end date of the query
#' @param ... arguments passed to \code{gtrends()}. See ?gtrends for more information including geography, language, and time-zone.
#'
#' @export
#' @importFrom purrr map map_chr pluck
#' @importFrom dplyr group_by summarise
#' @importFrom crayon bold
#' @importFrom magrittr %>%
#' @return An object of class `gtrends`
#'
#' @examples
#' \dontrun{
#' query_trends("RStudio 1.2")
#' }
#'

query_trends <- function(search_terms, from = NA, to = NA, ...) {
  
  if (!all(is.na(from), is.na(to))) {
    time <- paste(from, to, sep = " ")
  } else {
    time <- "today+5-y"
  }
  
  
  if (length(search_terms) <= 5) {
    message(paste("Querying", length(search_terms), "search terms."))
    searched_trends <- gtrends(search_terms, time = time, ...) %>%
      list()
  } else {
    message(paste("Querying", length(search_terms), "search terms individually."))
    searched_trends <- search_terms %>%
      map(gtrends, time = time, ...)
  }
  
  
  
  structure(
    searched_trends,
    class = c("gtrends", "list")
  )
  
}


#' Plot Google Trends interest over time
#'
#' @param x A \code{\link{gtrends}} object.
#' @param ... Additional parameters passed on in method dispatch. Currently not
#'   used.
#'
#' @import ggplot2
#'
#' @return A ggplot2 object is returned silently.
#' @export
#'
#' @examples
#' \dontrun{
#' res <- query_trends("nhl", geo = c("CA", "US"))
#' plot(res)
#' }
plot.gtrends <- function(x, ...) {
  df <- get_interest(x)
  
  
  df$legend <- paste(df$keyword, " (", df$geo, ")", sep = "")
  
  p <- ggplot(df, aes_string(x = "date", y = "hits", color = "legend")) +
    geom_line() +
    labs(x = "",
         y = "Relative Search Interest",
         title = "Interest Over Time")
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = "bottom")
  
  print(p)
  invisible(p)
}


#'  Print gtrends object
#' 
#' @param x gtrends object
#' @param ... Additional parameters passed on in method dispatch. Currently not
#'   used.
#' @importFrom dplyr pull group_by summarise
#' @export
print.gtrends <- function(x, ...) {
  cat((crayon::bold("~ Google Trends Results ~\n")))
  cat("\nSearch Terms: ")
  cat(paste(get_interest(x) %>%
              dplyr::pull(keyword) %>%
              unique(),
            sep = " "), sep = ", ")
  cat("\n\n~~~~~~~~~~~~~~~~~~~~~~~~~ summary ~~~~~~~~~~~~~~~~~~~~~~~~\n")
  print(x %>%
          get_interest() %>%
          group_by(keyword) %>%
          summarise(max_hits = max(hits),
                    min_hits = min(hits),
                    from = as.Date(min(date)),
                    to = as.Date(max(date))),
        width = 70)
  invisible(x)
}


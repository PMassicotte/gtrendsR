#' Google Trends Search
#'
#' Search Google Trends to retrieve relative hits and popularity. This supports comparison of up to 5 search terms. Anything more than that will be searched individually.
#'
#' @param search_terms A character vector containing the search terms of interest
#' @param from The beginning date of the query
#' @param to The beginning date of the query
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
    searched_trends <- gtrends(search_terms, time = time, ...) %>%
      list()
  } else {
    searched_trends <- search_terms %>%
      map(gtrends, time = time, ...)
  }
  
  
  
  structure(
    searched_trends,
    class = "gtrends"
  )
  
}

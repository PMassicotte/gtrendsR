if(getRversion() >= "2.15.1")  {
  
  utils::globalVariables(c("category", "hits", "id", "keyword", "name", "pull", "value"))
}

#' Retrieve related queries
#'
#' Extract tibble of related queries from gtrends object
#' @param gtrends An object of class gtrends created via \code{query_trends()}
#'
#' @export
#' @importFrom purrr map_dfr pluck
#' @importFrom dplyr mutate rename select left_join
#' @importFrom stringr str_extract
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' ob <- query_trends("obama")
#' get_related_queries(ob)
#' }
#'
#' @return A tibble containing related search terms
get_related_queries <- function(gtrends) {
  
  if (is.null(pluck(gtrends[[1]], "related_queries"))) {
    stop("No related queries")
  }
  
  map_dfr(gtrends, pluck("related_queries")) %>%
    left_join(mutate(categories, id = as.integer(id)),
              by = c("category" = "id")) %>%
    select(-category) %>%
    rename(category = name,
           search_term = value) %>%
    as_tibble() %>%
    return()
}




#' Retrieve related topics
#'
#' Extract a tibble of related topics from gtrends object
#' @param gtrends An object of class gtrends created via \code{query_trends()}
#'
#' @export
#' @importFrom purrr map_dfr pluck
#' @importFrom dplyr mutate rename select left_join
#' @importFrom stringr str_extract
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' ob <- query_trends("obama")
#' get_related_topics(ob)
#' }
#'
get_related_topics <- function(gtrends) {
  
  if (is.null(pluck(gtrends[[1]], "related_topics"))) {
    stop("No related topics")
  }
  
  map_dfr(gtrends, pluck("related_topics")) %>%
    left_join(mutate(categories, id = as.integer(id)),
              by = c("category" = "id")) %>%
    select(-category) %>%
    rename(category = name) %>%
    as_tibble() %>%
    return()
}



#' Retrieve interest over time
#'
#' Extract a tibble of interest over time
#' @param gtrends An object of class gtrends created via \code{query_trends()}
#'
#' @export
#' @importFrom purrr map_dfr pluck
#' @importFrom dplyr mutate rename select left_join
#' @importFrom stringr str_extract
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' ob <- query_trends("obama")
#' get_interest(ob)
#' }
#'
get_interest <- function(gtrends) {
  
  if (is.null(pluck(gtrends[[1]], "interest_over_time"))) {
    stop("No interest over time data")
  }
  
  map_dfr(.x = gtrends, ~pluck(.x, "interest_over_time") %>%
            mutate(hits = str_extract(hits, "[0-9]+"))) %>%
    left_join(mutate(categories, id = as.integer(id)),
              by = c("category" = "id")) %>%
    select(-category) %>%
    rename(category = name) %>%
    as_tibble() %>%
    mutate(hits = as.integer(hits),
           date = as.Date(date))
}


#' Retrieve interest by city
#'
#' Extract a tibble of interest by city from gtrends object
#' @param gtrends An object of class gtrends created via \code{query_trends()}
#'
#' @export
#' @importFrom purrr map_dfr pluck
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' ob <- query_trends("obama")
#' get_interest_city(ob)
#' }
#'
get_interest_city <- function(gtrends) {
  
  if (is.null(pluck(gtrends[[1]], "interest_by_city"))) {
    stop("No interest by city data")
  }
  
  map_dfr(gtrends, pluck("interest_by_city")) %>%
    as_tibble() %>%
    mutate(hits = as.integer(hits))
}

#' Retrieve interest by country
#'
#' Extract a tibble of interest by country from gtrends object
#' @param gtrends An object of class gtrends created via \code{query_trends()}
#'
#' @export
#' @importFrom purrr map_dfr pluck
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' ob <- query_trends("obama")
#' get_interest_country(ob)
#' }
#'
get_interest_country <- function(gtrends) {
  
  if (is.null(pluck(gtrends[[1]], "interest_by_country"))) {
    stop("No interest by country data")
  }
  
  map_dfr(gtrends, pluck("interest_by_country")) %>%
    as_tibble() %>%
    mutate(hits = as.integer(hits))
}


#' Retrieve interest by DMA
#'
#' Extract a tibble of interest by DMA from gtrends object
#' @param gtrends An object of class gtrends created via \code{query_trends()}
#'
#' @export
#' @importFrom purrr map_dfr pluck
#' @importFrom dplyr mutate
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' ob <- query_trends("obama")
#' get_interest_dma(ob)
#' }
#'
get_interest_dma <- function(gtrends) {
  
  if (is.null(pluck(gtrends[[1]], "interest_by_dma"))) {
    stop("No interest by DMA data")
  }
  
  map_dfr(gtrends, pluck("interest_by_dma")) %>%
    as_tibble() %>%
    mutate(hits = as.integer(hits))
}


#' Retrieve interest by region
#'
#' Extract a tibble of interest by region from gtrends object
#' @param gtrends An object of class gtrends created via \code{query_trends()}
#'
#' @export
#' @importFrom purrr map_dfr pluck
#' @importFrom dplyr mutate
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' ob <- query_trends("obama")
#' get_interest_region(ob)
#' }

get_interest_region <- function(gtrends) {
  
  if (is.null(pluck(gtrends[[1]], "interest_by_region"))) {
    stop("No interest by region data")
  }
  
  map_dfr(gtrends, pluck("interest_by_region")) %>%
    as_tibble() %>%
    mutate(hits = as.integer(hits))
}

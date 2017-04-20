#' kgraph
#' 
#' Obtains Google Knowledge Graph Search entities for a given keyword.
#' 
#' \code{kgraph} can be used to obtain Google Knowledge Graph Search entities
#' (\url{https://www.google.com/intl/es419/insidesearch/features/search/knowledge.html})
#' for any given keyword. The obtained \dQuote{kg-id} can then be used together
#' with the \code{gtrends} function to obtain Google Trends information based on
#' the Topic Search. This is a set of combined search queries providing the 
#' overall search interest on the topic. For example the topic search query for 
#' \dQuote{London the capital of England}, will not only cover searches for the
#' keyword London but also for \dQuote{Hotel in London}  obtaining  to be used
#' as \code{gtrends} Google Topic search function in Google Trends.
#'
#' @param keyword A character vector with the actual Google Knowledge Graph
#'   Search query keyword. Note that only one keyword allowed a time.
#'   
#' @param token A character string containing your Google API access token.
#'   Details about how to obtain your access token can be found here
#'   (\url{https://developers.google.com/knowledge-graph/how-tos/authorizing}).
#'   
#' @param ids A vector of entity IDs to search for in the Knowledge Graph.
#'   
#' @param hl A string specifying the ISO 639 language code (ex.: \dQuote{en} or 
#'   \dQuote{fr}).
#'   
#' @param types A vector of character strings which restricts the returned 
#'   entities. For example, you can specify Person to restrict the results to 
#'   entities representing people. If multiple types are specified, returned 
#'   entities will contain one or more of these types. Full list of schemas
#'   types is availabe (\url{http://schema.org/docs/full.html})
#'   
#' @param prefix If set to \dQuote{TRUE} prefix (initial substring) match
#'   against names and aliases of entities is allowed. For example, a prefix
#'   Jung will match entities and aliases such as Jung, Jungle, and Jung-ho
#'   Kang. Default is \dQuote{FALSE}.
#'   
#' @param limit Sets the maximum of returned entities per call. Default is
#'   \dQuote{10}, maximum allowed is \dQuote{20}
#'   
#' @return Returns an object of class \sQuote{kgraph}. This is a list containing
#'   the entities returned from the call sorted in an ascending order relative
#'   to the relevance score.
#''
#' @examples
#' kg <- kgraph("Call of Duty 2", "YOUR TOKEN", types = "VideoGame")
#' # get google trends for the first entity
#' topicsearch <- gtrends(kgs$entities[[1]]$id, time = "all")
#'         
#' @author Oliver Schaer, \email{info@@oliverschaer.ch}
#' 
#' @export

kgraph <- function(keyword, token, ids = "", hl = "",
                   types = "", prefix = FALSE, limit = 10){
  
  # Create ids string. Need to be in the form of ?ids=A&ids=B
  if (length(ids) > 1) {
    ids <- paste(ids, collapse = "&ids=")
  }
  
  # Create ids string. Need to be in the form of ?ids=A&ids=B
  if (length(types) > 1) {
    ids <- paste(types, collapse = "&types=")
  }
  
  if (limit > 20) {
    warning("Setting limit to 20 maximum allowed by Google")
    limit <- 20
  }
  
  # overview of query parameters available here:
  # https://developers.google.com/knowledge-graph/reference/rest/v1/
  # This needs to be better done -- problems with "" characters in ids and types
  if (ids == "" & types =="") {
    url <- paste0("https://kgsearch.googleapis.com/v1/entities:search?",
                  "&query=", keyword, "&key=", token, "&languages=", hl,
                  "&prefix=", prefix, "&limit=", limit, "&indent=", FALSE)
  } else if (ids == "") {
    url <- paste0("https://kgsearch.googleapis.com/v1/entities:search?",
                  "&query=", keyword, "&key=", token, 
                  "&languages=", hl, "&types=", types, "&prefix=", prefix,
                  "&limit=", limit, "&indent=", FALSE)
  } else if (types == "") {
    url <- paste0("https://kgsearch.googleapis.com/v1/entities:search?",
                  "&query=", keyword, "&key=", token, "&ids=", ids, 
                  "&languages=", hl, "&prefix=", prefix,
                  "&limit=", limit, "&indent=", FALSE)
  } else {
    url <- paste0("https://kgsearch.googleapis.com/v1/entities:search?",
                  "&query=", keyword, "&key=", token, "&ids=", ids, 
                  "&languages=", hl, "&types=", types, "&prefix=", prefix,
                  "&limit=", limit, "&indent=", FALSE)
  }
  
  
  curlReturn <- curl::curl_fetch_memory(URLencode(url))
  
  
  # error handling
  stopifnot(curlReturn$status_code == 200)
  
  # Prepare output
  callUrl <- curlReturn$url
  
  # tyding up the returns
  content <- jsonlite::fromJSON(rawToChar(curlReturn$content), simplifyVector = F)
  
  entities <- list()
  
  # make sure result is returned
  if (length(content$itemListElement) != 0) {
   
    ecount <- 1
    
    for (entity in content[[3]]) {
      
      id <- strsplit(entity[[2]][[1]], "kg:")[[1]][2] # ID
      name <- entity[[2]][[2]] # Name
      type <- entity[[2]][[3]] # type(s)
      description <- entity[[2]][[4]] # description
      detailedDescription <- entity[[2]][[5]][[1]] # only article body used
      score <- entity[[3]] # score 
      
      entities[[ecount]] <-
        list("id" = id, "name" = name, "type" = type, "description" = description,
             "detailedDescription" = detailedDescription, "score" = score)
      
      ecount <- ecount + 1
    }
  }
  return(structure(
    list("type" = "kgraph", "call" = sys.call(), "callUrl" = callUrl,
      "entities" = entities), class = "kgraph"))
}
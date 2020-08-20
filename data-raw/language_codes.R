get_language_codes <- function() {
  url <- "http://www.lingoes.net/en/translator/langcode.htm"
  
  webpage <- read_html(url)
  language_codes <- html_nodes(webpage, "table")
  language_codes <- html_table(language_codes, header = TRUE)[[1]]
  
  names(language_codes) <- tolower(names(language_codes))
  
  return(language_codes)
}
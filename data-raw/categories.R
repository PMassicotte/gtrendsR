# Process categories data
get_categories <- function() {
  
  file <- system.file("extdata", "categories.json", package = "gtrendsR")
  
  res <- fromJSON(file, simplifyDataFrame = FALSE)
  
  res <- as.Node(res)
  
  categories <- ToDataFrameTree(res, "name", "id")
  categories <- na.omit(categories)
  categories <- categories[, c("name", "id")]
  categories$name <- iconv(categories$name, to = "ASCII//TRANSLIT")
  categories$id <- as.character(categories$id)
  
  return(categories)
}
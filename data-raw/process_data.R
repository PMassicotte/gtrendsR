
# Process categories data
get_categories <- function() {

  library(jsonlite)
  library(data.tree)
  
    
  file <- system.file("extdata", "categories.json", package = "gtrendsR")
  
  res <- fromJSON(file, simplifyDataFrame =  FALSE)
  
  res <- as.Node(res)
  
  categories <- ToDataFrameTree(res, "name", "id")
  categories <- na.omit(categories)
  categories <- categories[, c("name", "id")]
  categories$name <- iconv(categories$name, to = "ASCII//TRANSLIT")
  categories$id <- as.character(categories$id)
  
  return(categories)
}

# Process countries
get_countries <- function() {
  
  # source: http://www.unece.org/cefact/codesfortrade/codes_index.html
  
  dir <- tempdir()
  destfile <- paste0(dir, "/isocodes.zip")
  
  ret <- download.file(
    "http://www.unece.org/fileadmin/DAM/cefact/locode/loc161csv.zip",
    destfile
  )
  
  # Was the file found?
  stopifnot(ret == 0)
  
  file <- unzip(destfile, exdir = dir)
  file <- file[grepl("part\\d.csv", file, ignore.case = TRUE)]
  
  # df <- read.table(file[3], sep = ",", header = FALSE, fileEncoding = "latin1")
  
  countries <-
    lapply(
      file,
      read.table,
      header = FALSE,
      stringsAsFactors = FALSE,
      sep = ",",
      fileEncoding = "latin1"
    )
  
  # Bind everything
  countries <- do.call(rbind, countries)
  countries <- countries[, c(2, 4, 6)]
  countries <- unique(countries)
  names(countries) <- c("country_code", "description", "sub_code")
  
  # Remove entries without sub_code and format the data
  index <- which(grepl("^\\.", countries$description) | countries$sub_code != "")
  countries <- countries[index, ]
  countries$sub_code <- ifelse(countries$sub_code != "",
                               paste(countries$country_code, countries$sub_code, sep = "-"),
                               "")
  
  return(countries)
  
}


countries <- get_countries()
categories <- get_categories()

# Save the data
save(countries, file = "data/countries.rda") # for data("countries")
save(categories, file = "data/categories.rda") # for data("countries")
save(countries, categories, file = "R/sysdata.rda") # for internal use

# Compress data
tools::resaveRdaFiles("data/countries.rda")
tools::resaveRdaFiles("data/categories.rda")
tools::resaveRdaFiles("R/sysdata.rda")

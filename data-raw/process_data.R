library(rvest)
library(jsonlite)
library(data.tree)

# Process categories data
get_categories <- function() {

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
  
  destfile <- tempfile(fileext = ".csv")
  
  ret <- download.file(
    "https://raw.githubusercontent.com/olahol/iso-3166-2.json/master/data/eQuest.csv",
    destfile
  )
  
  # Was the file found?
  stopifnot(ret == 0)

  # , col.names = c("country_code", "sub_code", "country")
  df <- read.csv(destfile, sep = ",", header = FALSE, fileEncoding = "utf8", stringsAsFactors = FALSE)
  df <- tidyr::unite(df, "sub_code", V2, V3, sep = "")
  names(df) <- c("country_code", "sub_code", "country")
  
  
  # *************************************************************************
  # USA metro codes
  # *************************************************************************
  
  dir <- tempdir()
  destfile <- paste0(dir, "/dma.xlsx")
  
  file <- download.file("www.google.com/help/hc/downloads/ds3/Location-Language-Codes-AdWords.xlsx", 
                        destfile = destfile)
  
  usa <- readxl::read_excel(destfile, skip = 1)
  # usa <- na.omit(usa[, c(8, 10, 11)])
  
  usa <- data.frame(
    country_code = "US",
    description = usa$Metro,
    sub_code = paste(
      "US",
      lapply(regmatches(usa$Metro, regexec(", (\\S{2})", usa$Metro)), "[", 2),
      usa$`Metro code`,
      sep = "-"
    ), stringsAsFactors = FALSE
  )
  
  usa <- na.omit(usa)
  
  # *************************************************************************
  # Merge together
  # *************************************************************************
  countries <- dplyr::bind_rows(df, usa)

  # *************************************************************************
  # Final format
  # *************************************************************************
  countries$country_code <- ifelse(countries$country_code == "", NA, countries$country_code)
  countries <- tidyr::fill(countries, country_code)
  
  # Fix the encoding
  countries <- data.frame(sapply(countries, iconv, to = "ASCII//TRANSLIT"))
  
  return(countries)
  
}

get_language_codes <- function() {
  url <- "http://www.lingoes.net/en/translator/langcode.htm"
  
  webpage <- read_html(url)
  language_codes <- html_nodes(webpage, 'table')
  language_codes <- html_table(language_codes, header = TRUE)[[1]]
  
  names(language_codes) <- tolower(names(language_codes))
  
  return(language_codes)
  
}

countries <- get_countries()
categories <- get_categories()
language_codes <- get_language_codes()

# Save the data
save(countries, file = "data/countries.rda") # for data("countries")
save(categories, file = "data/categories.rda") # for data("countries")
save(countries, categories, language_codes, file = "R/sysdata.rda") # for internal use

# Compress data
tools::resaveRdaFiles("data/countries.rda")
tools::resaveRdaFiles("data/categories.rda")
tools::resaveRdaFiles("R/sysdata.rda")


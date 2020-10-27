library(rvest)
library(jsonlite)
library(data.tree)

source("data-raw/categories.R")
source("data-raw/language_codes.R")
source("data-raw/country_codes.R")

# Extract and save the data -----------------------------------------------

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

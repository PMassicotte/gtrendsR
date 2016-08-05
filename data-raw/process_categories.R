library(jsonlite)
library(data.tree)

rm(list = ls())

file <- system.file("extdata", "categories.json", package = "gtrendsR")

res <- fromJSON(file, simplifyDataFrame =  FALSE)

res <- as.Node(res)

categories <- ToDataFrameTree(res, "name", "id")
categories <- na.omit(categories)
categories <- categories[, c("name", "id")]
categories$name <- iconv(categories$name, to = "ASCII//TRANSLIT")

devtools::use_data(categories, overwrite = TRUE)
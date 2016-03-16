categories <- jsonlite::fromJSON(txt = "data/categories.json", flatten = TRUE)

categories[["humor"]]
categories$children$name

df <- do.call("rbind", categories)

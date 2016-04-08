categories <- jsonlite::fromJSON(txt = "data/categories.json", flatten = TRUE)

categories[["humor"]]
categories$children$name

df <- do.call("rbind", categories)


#http://r4ds.had.co.nz/hierarchy.html#extracting-deeply-nested-elements
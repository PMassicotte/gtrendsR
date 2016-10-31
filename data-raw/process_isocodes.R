# Script to download ISO3166-2 country codes

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

# Save the data
save(countries, file = "data/countries.rda") # for data("countries")
save(countries, file = "R/sysdata.rda") # for internal use

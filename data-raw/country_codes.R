# Process countries
get_countries <- function() {
  destfile <- tempfile(fileext = ".csv")

  ret <-
    download.file(
      "https://raw.githubusercontent.com/CharlotteWoolley/Comprehensive_ISO_Location_Codes/master/ISO_codes.csv",
      destfile = destfile
    )

  # Was the file found?
  stopifnot(ret == 0)

  # , col.names = c("country_code", "sub_code", "country")
  countries <- read.csv(destfile, na.strings = "")

  # Fix the encoding
  countries <-
    data.frame(sapply(countries, iconv, to = "ASCII//TRANSLIT"))

  # *************************************************************************
  # USA metro codes
  # *************************************************************************

  dir <- tempdir()
  destfile <- paste0(dir, "/dma.xlsx")

  file <- download.file(
    "www.google.com/help/hc/downloads/ds3/Location-Language-Codes-AdWords.xlsx",
    destfile = destfile
  )

  usa <- readxl::read_excel(destfile, skip = 1, .name_repair = "minimal")[, c(
    11:14
  )]

  usa <- data.frame(
    country_code = "US",
    name = usa$Metro,
    sub_code = paste(
      "US",
      lapply(
        regmatches(
          usa$Metro,
          regexec(", (\\S{2})", usa$Metro)
        ),
        "[",
        2
      ),
      sep = "-"
    )
  )

  usa <- na.omit(usa)
  # usa <- usa[, c(1, 3, 2)]
  # names(usa) <- names(countries)

  # *************************************************************************
  # More country codes from Google
  # *************************************************************************

  url <- "https://trends.google.com/trends/api/explore/pickers/geo"
  obj <- curl::curl_fetch_memory(url)
  ## Fix encoding issue for keywords like österreich"
  temp <- rawToChar(obj$content)
  Encoding(temp) <- "UTF-8"

  df <- jsonlite::fromJSON(substring(temp, first = 6))

  countrie_names <- data.frame(
    country_name = df$children[[2]],
    country_code = df$children[[3]],
    stringsAsFactors = FALSE
  )

  i <- which(unlist(lapply(df$children[[1]], function(x) !is.null(x))))

  res <- df$children$children[i]

  # do.call(rbind, res[183])

  extract_df <- function(l) {
    if (length(names(l)) == 2) {
      return(l)
    } else {
      (return(do.call(rbind, l$children)))
    }
  }

  rr <- lapply(res, extract_df)
  names(rr) <- df$children$name[i]

  countries2 <- data.table::rbindlist(rr, idcol = "country_name")
  names(countries2) <- c("country_name", "name", "code")

  countries2 <- merge(countrie_names, countries2, by = "country_name")
  countries2$sub_code <- paste(
    countries2$country_code,
    countries2$code,
    sep = "-"
  )

  countries2 <- countries2[, c("country_code", "sub_code", "name")]
  countries2$name <- toupper(countries2$name)

  # *************************************************************************
  # Merge together
  # *************************************************************************
  countries <- rbind(countries, usa, countries2)
  countries <- countries[!duplicated(countries), ]

  return(countries)
}

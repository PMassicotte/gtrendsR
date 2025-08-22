#' Extract top and rising searches
#'
#' @param data Data returned by a search query.
#'
#' @return A list with TOP and RISING data (if such data was found).
extract_top_rising <- function(data) {
  start_top <- which(grepl("TOP", data))[1]
  start_rising <- which(grepl("RISING", data))[1]

  if (length(start_top) == 0 & length(start_rising) == 0) {
    return(NULL) ## No data returned
  }

  if (is.na(start_top) & is.na(start_rising)) {
    return(NULL) ## No data returned
  }

  new_res <- NULL

  if (length(start_top) > 0) {
    end_top <- ifelse(length(start_rising) == 0, length(data), start_rising - 2)

    # Make sure there are "RISING" data. If not, use the length of the vector
    # and the end index.

    if (is.na(end_top)) {
      end_top <- length(data)
    }

    top <- read.csv(
      textConnection(data[start_top:end_top]),
      row.names = NULL,
      encoding = "UTF-8"
    )
    top$subject <- rownames(top)
    rownames(top) <- NULL
    top <- top[, c(2, 1)]
    names(top) <- c("subject", "top")

    top <- reshape(
      top,
      varying = "top",
      v.names = "value",
      direction = "long",
      timevar = "related_topics",
      times = "top"
    )

    new_res <- rbind(new_res, top)
  }

  # Make sure there are "RISING" data.

  if (length(start_rising) > 0 & !is.na(start_rising)) {
    rising <- read.csv(
      textConnection(data[start_rising:length(data)]),
      row.names = NULL,
      encoding = "UTF-8"
    )
    rising$subject <- rownames(rising)
    rownames(rising) <- NULL
    rising <- rising[, c(2, 1)]
    names(rising) <- c("subject", "rising")

    rising <- reshape(
      rising,
      varying = "rising",
      v.names = "value",
      direction = "long",
      timevar = "related_topics",
      times = "rising"
    )

    new_res <- rbind(new_res, rising)
  }

  return(new_res)
}

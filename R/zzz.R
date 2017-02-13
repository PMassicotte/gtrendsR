check_time <- function(time) {
  
  stopifnot(is.character(time))
  
  fixed_format <- c(
    "now 1-H",    # last hour
    "now 4-H",    # last four hours
    "now 1-d",    # last day
    "now 7-d",    # last seven days
    "today 1-m",  # past 30 days
    "today 3-m",  # past 90 days
    "today 12-m", # past 12 months
    "today+5-y",  # last 5 years (default)
    "all"        # Since begening of Google Trends (2004)
  )
  
  ## Return TRUE if one of the basic date formats is used
  if (time %in% fixed_format) {
    return(TRUE)
  }
  
  ## The other possible format is by using time range
  
  time <- unlist(strsplit(time, " "))
  
  ## Need to be a vector of two
  if (length(time) != 2) {
    return(FALSE)
  }
  
  start_date <- anytime::anydate(time[1])
  end_date <- anytime::anydate(time[2])
  
  if (is.na(start_date) | is.na(end_date)) {
    return(FALSE)
  }
  
  ## Start date can't be after end date
  if (start_date >= end_date) {
    return(FALSE)
  }
  
  ## Start date can't be before 204-01-01
  if (start_date < as.Date("2004-01-01")) {
    return(FALSE)
  }
  
  ## End date can't be after today
  if (end_date > Sys.Date()) {
    return(FALSE)
  }
  
  return(TRUE)
  
}
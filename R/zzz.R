# create environment in which to put cookie_handler
.pkgenv <- new.env(parent=emptyenv())

# alternative url is "http://apis.google.com/Cookies/OTZ"
# function to create cookie_handler, which is necessary to run get_widget()
get_api_cookies <- function(cookie_url) {
  # create new handler
  cookie_handler <- curl::new_handle()
  # VY. set options for the proxy
  curl::handle_setopt(handle=cookie_handler,.list=list(ssl_verifypeer=0L,proxyuserpwd=paste(.pkgenv[["handle_domain"]],"\\",.pkgenv[["handle_user"]],":",.pkgenv[["handle_password"]],sep=""),proxyauth=.pkgenv[["handle_proxyauth"]],proxy=.pkgenv[["handle_proxyhost"]],proxyport=.pkgenv[["handle_proxyport"]]))
  # fetch API cookies
  cookie_req <- curl::curl_fetch_memory(cookie_url, handle = cookie_handler)
  curl::handle_cookies(cookie_handler)
  # assign handler to .pkgenv environment
  .pkgenv[["cookie_handler"]] <- cookie_handler
  return(NULL)
}

check_time <- function(time_ranges) {
  stopifnot(is.character(time_ranges))

  fixed_format <- c(
    "now 1-H", # last hour
    "now 4-H", # last four hours
    "now 1-d", # last day
    "now 7-d", # last seven days
    "today 1-m", # past 30 days
    "today 3-m", # past 90 days
    "today 12-m", # past 12 months
    "today+5-y", # last 5 years (default)
    "all" # Since begening of Google Trends (2004)
  )
  
  for (tr in time_ranges){

    ## Return TRUE if one of the basic date formats is used
    if (tr %in% fixed_format) {
      return(TRUE)
    }

    ## The other possible format is by using time range
    time <- unlist(strsplit(tr, " "))
    
    ## Need to be a vector of two
    if (length(time) != 2) {
      return(FALSE)
    }
    
    if(!grepl("T",time[1])){
      start_date <- as.POSIXct(anytime::anydate(time[1]))
      end_date <- as.POSIXct(anytime::anydate(time[2]))
    }else{
      start_date <- anytime::anytime(time[1])
      end_date <- anytime::anytime(time[2])
    }
    
    if (is.na(start_date) | is.na(end_date)) {
      return(FALSE)
    }
    
    ## Start date can't be after end date
    if (start_date >= end_date) {
      return(FALSE)
    }
    
    ## Start date can't be before 2004-01-01
    if (start_date < as.POSIXct("2004-01-01")) {
      return(FALSE)
    }
    
    ## End date can't be after today
    if (end_date > as.POSIXct(Sys.time())) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}


get_widget <- function(comparison_item, category, gprop, hl, cookie_url, tz) {
  token_payload <- list()
  token_payload$comparisonItem <- comparison_item
  token_payload$category <- category
  token_payload$property <- gprop

  # token_payload$comparisonItem$keyword <- curl::curl_escape(token_payload$comparisonItem$keyword)
  
  url <- URLencode(paste0(
    "https://www.google.com/trends/api/explore?property=&req=",
    jsonlite::toJSON(token_payload, auto_unbox = TRUE),
    "&tz=",tz,"&hl=", hl
  ))
  
  url <- encode_keyword(url)
  
  # if cookie_handler hasn't been set up, get the requisite cookies from Google's API
  if(!exists("cookie_handler", envir = .pkgenv)){ get_api_cookies(cookie_url) }
  # get the tokens etc., using the URL and the cookie_handler
  widget <- curl::curl_fetch_memory(url, handle = .pkgenv[["cookie_handler"]])

  stopifnot(widget$status_code == 200)

  ## Fix encoding issue for keywords like österreich"
  temp <- rawToChar(widget$content)
  Encoding(temp) <- "UTF-8"

  myjs <- jsonlite::fromJSON(substring(temp, first = 6))

  widget <- myjs$widgets
}

interest_over_time <- function(widget, comparison_item,tz) {
  payload2 <- list()
  # if there is a mix of search and topic terms requests are all shifted by one
  # for some reason. Maybe there is a better fix for this. I don't understand
  # precisely the structure of the widget.
  # try this example:
  # topicKeys <- c("/m/05s_khw", "Assassins Creed Brotherhood", "/m/0gmg6lv")
  # vs.
  # topicKeys <- c("Assassins Creed", "Assassins Creed Brotherhood", "Assassins Creed Rogue")
  # gtrends(topicKeys, time = "all")
  if((length(widget$request$comparisonItem[[2]]$time)!=1)&
     (length(unique(widget$request$comparisonItem[[2]]$time))!=1)&
     (!is.null(widget$request$comparisonItem[[2]]) )
     ){
    payload2$resolution <- widget$request$resolution[1]
    payload2$locale <- widget$request$locale[2]
    payload2$comparisonItem <- widget$request$comparisonItem[[2]]
    payload2$comparisonItem$geo <- widget$request$comparisonItem[[1]]$geo
    payload2$requestOptions$property <- widget$request$requestOptions$property[2]
    payload2$requestOptions$backend <- widget$request$requestOptions$backend[2]
    payload2$requestOptions$category <- widget$request$requestOptions$category[2]
    token_payload2 <- widget$token[which(widget$id == "TIMESERIES")]

    url <- URLencode(paste0(
      "https://www.google.com/trends/api/widgetdata/multirange/csv?req=",
      jsonlite::toJSON(payload2, auto_unbox = T,null="list"),
      "&token=", token_payload2,
      "&tz=",tz
    ))
  }else{
    if(!is.na(widget$request$locale[1])|(length(unique(unlist(widget$request$comparisonItem[[1]]$geo)))>1)){
      payload2$locale <- widget$request$locale[1]
      payload2$comparisonItem <- widget$request$comparisonItem[[1]]
      payload2$resolution <- widget$request$resolution[1]
      payload2$requestOptions$category <- widget$request$requestOptions$category[1]
      payload2$requestOptions$backend <- widget$request$requestOptions$backend[1]
      payload2$time <- widget$request$time[1]
      payload2$requestOptions$property <- widget$request$requestOptions$property[1]
      token_payload2 <- widget$token[1]
    } else {
      payload2$locale <- widget$request$locale[2]
      payload2$comparisonItem <- widget$request$comparisonItem[[1]]
      payload2$resolution <- widget$request$resolution[2]
      payload2$requestOptions$category <- widget$request$requestOptions$category[2]
      payload2$requestOptions$backend <- widget$request$requestOptions$backend[2]
      payload2$time <- widget$request$time[2]
      payload2$requestOptions$property <- widget$request$requestOptions$property[2]
      token_payload2 <- widget$token[2]
    }
    url <- URLencode(paste0(
      "https://www.google.com/trends/api/widgetdata/multiline/csv?req=",
      jsonlite::toJSON(payload2, auto_unbox = T),
      "&token=", token_payload2,
      "&tz=",tz
    ))
  }

  # ****************************************************************************
  # Downoad the results
  # ****************************************************************************
  url <- encode_keyword(url)
  
  # VY. use the handler with proxy options.
  res <- curl::curl_fetch_memory(url, handle = .pkgenv[["cookie_handler"]])

  # Something went wrong
  if (res$status_code != 200) {
    stop("Status code was not 200. Returned status code:", res$status_code)
  }

  # ****************************************************************************
  # Format the results in a nice way
  # ****************************************************************************
  con <- textConnection(rawToChar(res$content))
  df <- read.csv(con, skip = 1, stringsAsFactors = FALSE)
  close(con)

  if (nrow(df) < 1) {
    return(NULL) ## No data
  }
  
  if((length(widget$request$comparisonItem[[2]]$time)==1)|
     (length(unique(widget$request$comparisonItem[[2]]$time))==1)|
     (is.null(widget$request$comparisonItem[[2]]) )
     ){
    n <- nrow(df) # used to reshape the data
    
    df <- reshape(
      df,
      varying = names(df)[2:ncol(df)],
      v.names = "hits",
      direction = "long",
      timevar = "temp",
      times = names(df)[2:ncol(df)]
    )
    
    df$temp <- NULL
    
    df <- cbind(
      df,
      comparison_item[rep(seq_len(nrow(comparison_item)), each = n), 1:3],
      row.names = NULL
    )
    
    df$geo <- ifelse(df$geo == "", "world", df$geo)
    df$gprop <- ifelse(widget$request$requestOptions$property[1] == "", "web", widget$request$requestOptions$property[1])
    df$category <- widget$request$requestOptions$category[1]
    names(df)[1] <- "date"
    df$id <- NULL
    
    # Format the returned date
    if(all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",df$date))){
      df$date <- as.POSIXct(df$date,format="%Y-%m-%d",tz=paste0("GMT",ifelse(tz>=0,"+","-"),(abs(tz)/60)),asUTC=T)
    }else if(all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}$",df$date))){
      df$date <- as.POSIXct(df$date,format="%Y-%m-%dT%H",tz=paste0("GMT",ifelse(tz>=0,"+","-"),(abs(tz)/60)),asUTC=T)
    }else if(all(grepl("^[0-9]{4}-[0-9]{2}$",df$date))){
      df$date <- df$date <- as.POSIXct(paste0(df$date,"-01"), 
                                       format = "%Y-%m-%d", tz = paste0("GMT", ifelse(tz >= 0, "+", "-"), 
                                                   (abs(tz)/60)), asUTC = T)
    }else{
      df$date <- gsub("^([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}).*$","\\1",df$date)
      df$date <- as.POSIXct(df$date,format="%Y-%m-%dT%H:%M:%S",tz=paste0("GMT",ifelse(tz>=0,"+","-"),(abs(tz)/60)),asUTC=T)
    }

  }else{
    n <- nrow(df) # used to reshape the data
    kw <- payload2$comparisonItem$complexKeywordsRestriction[[1]][[1]]$value
    kw <- gsub("[[:blank:]-]",".",kw)
    dates <- df[,which(!grepl(kw,names(df)))]
    
    if(all(sapply(lapply(dates,function(x) grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",x)),function(y) all(y)))){
      dates <- data.frame(lapply(dates,
                                 function(x) as.POSIXct(x,
                                                format="%Y-%m-%d",
                                                tz=paste0("GMT",ifelse(tz>=0,"+","-"),(abs(tz)/60)))))
    }else if(all(sapply(lapply(dates,function(x) grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}$",x)),function(y) all(y)))){
      dates <- data.frame(lapply(dates,
                                 function(x) as.POSIXct(x,
                                                        format="%Y-%m-%dT%H",
                                                        tz=paste0("GMT",ifelse(tz>=0,"+","-"),(abs(tz)/60)))))
    }else{
      dates <- data.frame(lapply(dates,
                                 function(x)
                                   gsub("^([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}).*$","\\1",x)))
      dates <- data.frame(lapply(dates,
                                 function(x) as.POSIXct(x,
                                              format="%Y-%m-%dT%H:%M:%S",
                                              tz=paste0("GMT",ifelse(tz>=0,"+","-"),(abs(tz)/60)))))
    }
    
    # dates <- data.frame(lapply(dates,function(x) anytime::anytime(x,tz=paste0("GMT",ifelse(tz>=0,"+","-"),(abs(tz)/60)),asUTC=T)))
    
    hits <- df[,which(grepl(kw,names(df)))]
    
    for(jj in 1:NCOL(dates)){
      df_tmp <- data.frame(dates[jj],hits[jj])
      df_tmp2 <- comparison_item[rep(jj,n), 1:3]
      
      df_tmp2[,1] <- ifelse(df_tmp2[,1] == "", "world", df_tmp2[,1])
      df_tmp2[,3] <- ifelse(widget$request$requestOptions$property[1] == "", "web", widget$request$requestOptions$property[1])
      df_tmp2[,4] <- widget$request$requestOptions$category[1]
      if(jj==1){
        df_res <- cbind(df_tmp,df_tmp2)
        names(df_res) <- c("date","hits","geo","time","gprop","category")
      }else{
        df_tmp3 <- cbind(df_tmp,df_tmp2)
        names(df_tmp3) <- c("date","hits","geo","time","gprop","category")
        df_res <- rbind(df_res,df_tmp3)
      }
    }
    df <- df_res
  }

  return(df)
}


interest_by_region <- function(widget, comparison_item, low_search_volume,tz) {
  i <- which(grepl("Interest by", widget$title) == TRUE)

  if (length(i) == 0) {
    return(list(NULL))
  }
  
  ## Interest by region need to be retreived individually

  # resolution <- sub(".* (\\w+)$", "\\1", widget$title[i])
  # resolution[resolution == "subregion"] <- "region"
  # resolution[resolution == "metro"] <- "dma"

  # resolution <- c(resolution, rep(c("city", "dma"), each = length(resolution)))

  ##
  resolution <-
    expand.grid(i, c(ifelse(
      grepl("world", na.omit(widget$geo)), "country", "region"
    ), "city", "dma"), stringsAsFactors = FALSE)

  resolution <- unique(resolution)

  i <- resolution$Var1
  resolution <- resolution$Var2

  ## If it is not US metro, then also search for "city"
  # if (!all(grepl("dma", resolution))) {
  #   resolution <- c(resolution, rep("city", length(resolution)))
  # }
  #

  ## If no country is specified, resolution should be "COUNTRY"
  # resolution[grepl("world", na.omit(widget$geo))] <- "country"
  resolution <- toupper(resolution)

  res <-
    mapply(
      create_geo_payload,
      i,
      resolution,
      MoreArgs = list(widget = widget, low_search_volume = low_search_volume, tz = tz),
      SIMPLIFY = FALSE
    )

  ## Remove duplicated
  ii <- !duplicated(res)
  res <- res[ii]
  resolution <- resolution[ii]

  ## Remove NA
  ii <- !unlist(lapply(res, is.null))
  res <- res[ii]
  resolution <- resolution[ii]


  res <- setNames(res, tolower(resolution))

  return(res)
}


create_geo_payload <- function(i, widget, resolution, low_search_volume,tz) {
  payload2 <- list()
  payload2$locale <- unique(na.omit(widget$request$locale))
  payload2$comparisonItem <- widget$request$comparisonItem[[i]]
  payload2$resolution <- resolution
  payload2$requestOptions$backend <- widget$request$requestOptions$backend[i]
  payload2$requestOptions$property <- widget$request$requestOptions$property[i]
  payload2$requestOptions$category <- widget$request$requestOptions$category[i]
  payload2$geo <- as.list((widget$request$geo[i, , drop = FALSE]))
  payload2$includeLowSearchVolumeGeos <- low_search_volume


  url <- URLencode(paste0(
    "https://www.google.com/trends/api/widgetdata/comparedgeo/csv?req=",
    jsonlite::toJSON(payload2, auto_unbox = T,null="list"),
    "&token=", widget$token[i],
    "&tz=",tz,"&hl=en-US"
  ))

  url <- encode_keyword(url)
  # VY. use the handler with proxy options.
  res <- curl::curl_fetch_memory(url, handle = .pkgenv[["cookie_handler"]])

  if (res$status_code != 200) {
    return(NULL)
  }

  con <- textConnection(rawToChar(res$content))
  df <- read.csv(con, skip = 1, stringsAsFactors = FALSE)
  close(con)

  if (nrow(df) == 0) {
    return(NULL)
  }

  n <- nrow(df) # used to reshape the data

  df <- reshape(
    df,
    varying = names(df)[2:ncol(df)],
    v.names = "hits",
    direction = "long",
    timevar = "temp",
    times = names(df)[2:ncol(df)]
  )


  kw <- do.call(rbind, widget$request$comparisonItem[[i]]$complexKeywordsRestriction$keyword)

  df <- cbind(
    df,
    kw[rep(seq_len(nrow(kw)), each = n), 2],
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  df$temp <- NULL
  # df$geo <- widget$geo[i]
  df$geo <- suppressWarnings(na.omit(unlist(widget$request$geo[i, ])))

  df$geo <- ifelse(is.null(df$geo), "world", df$geo)
  df$gprop <- ifelse(widget$request$requestOptions$property[i] == "", "web", widget$request$requestOptions$property[i])

  df$id <- NULL
  rownames(df) <- NULL

  names(df) <- c("location", "hits", "keyword", "geo", "gprop")

  return(df)
}

## Remove NA from list
na.omit.list <- function(y) {
  return(y[!sapply(y, function(x)
    all(is.na(x)))])
}

## Replace special characters in keywords like P&500 -> P%26500 
encode_keyword <- function(url) {
  url <- gsub("(?:\\G(?!^)|\\[\\s*)[^][\\s]*\\K\\&(?!])(?=[^][]*])", "%26", url, perl = TRUE)
  url <- gsub("(?:\\G(?!^)|\\[\\s*)[^][\\s]*\\K\\&(?!])(?=[^][]*])", "%26", url, perl = TRUE)
  url <- gsub("(?:\\G(?!^)|\\[\\s*)[^][\\s]*\\K\\&(?!])(?=[^][]*])", "%26", url, perl = TRUE)
  url <- gsub("(?:\\G(?!^)|\\[\\s*)[^][\\s]*\\K\\&(?!])(?=[^][]*])", "%26", url, perl = TRUE)
  gsub("(?:\\G(?!^)|\\[\\s*)[^][\\s]*\\K\\&(?!])(?=[^][]*])", "%26", url, perl = TRUE)
}


map_tz2min <- function(timezone){
  round((unclass(as.POSIXct(format(Sys.time(),"%Y-%m-%d %H:%M:%S",tz="UTC")))-unclass(as.POSIXct(format(Sys.time(),"%Y-%m-%d %H:%M:%S",tz=timezone))))/60)
}

map_min2tz <- function(min){
    
}

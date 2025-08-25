# create environment in which to put cookie_handler
.pkgenv <- new.env(parent = emptyenv())

# alternative url is "http://apis.google.com/Cookies/OTZ"
# function to create cookie_handler, which is necessary to run get_widget()
get_api_cookies <- function(cookie_url) {
  # create new handler
  cookie_handler <- curl::new_handle()

  # set options for the proxy
  proxy_domain <- ifelse(
    is.null(.pkgenv[["handle_domain"]]),
    "",
    paste0(.pkgenv[["handle_domain"]], "\\")
  )
  proxy_user_pwd <- paste0(
    proxy_domain,
    .pkgenv[["handle_user"]],
    ":",
    .pkgenv[["handle_password"]]
  )
  curl_opts <- list(
    ssl_verifypeer = 0L,
    proxyuserpwd = proxy_user_pwd,
    proxyauth = .pkgenv[["handle_proxyauth"]],
    proxy = .pkgenv[["handle_proxyhost"]],
    proxyport = .pkgenv[["handle_proxyport"]]
  )
  ## add extra curl options
  curl_opts <- append(curl_opts, .pkgenv[["handle_extra_curl_opts"]])
  curl::handle_setopt(handle = cookie_handler, .list = curl_opts)

  # fetch API cookies
  cookie_req <- curl::curl_fetch_memory(cookie_url, handle = cookie_handler)
  curl::handle_cookies(cookie_handler)
  # assign handler to .pkgenv environment
  .pkgenv[["cookie_handler"]] <- cookie_handler
  return(NULL)
}

get_widget <- function(comparison_item, category, gprop, hl, cookie_url, tz) {
  # Initialize cookies if needed
  if (!exists("cookie_handler", envir = .pkgenv)) {
    tryCatch(
      {
        get_api_cookies(cookie_url)
      },
      error = function(e) {
        stop(
          "Failed to initialize Google Trends session during cookie acquisition:\n",
          "Could not obtain required authentication cookies from Google.\n",
          "\nPossible causes:\n",
          "  - Network connectivity issues\n",
          "  - Proxy server configuration problems\n",
          "  - Firewall blocking Google Trends access\n",
          "  - Invalid cookie URL: ",
          cookie_url,
          "\n",
          "\nOriginal error: ",
          e$message,
          call. = FALSE
        )
      }
    )
  }

  # Build API URL
  url <- build_explore_url(comparison_item, category, gprop, hl, tz)

  # Make API request
  response <- make_api_request(url, "widget initialization")

  # Parse response
  parsed_response <- parse_api_response(response)

  # Fix geographic encoding issues
  parsed_response <- fix_geo_encoding(parsed_response, comparison_item)

  return(parsed_response$widgets)
}
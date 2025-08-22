#' If \code{gtrends} should be used behind a proxy, especially with NTLM
#' authentication mode, you need to set the proxy parameters and credentials
#' using "setHandleParameters" function
#'
#' @param user A string specifying your username
#' @param password A string specifying your password
#' @param domain A string specifying the authentication domain
#' @param proxyhost A string specifying the Proxy host DNS or IP address
#' @param proxyport A numeric specifying the Proxy Port : 8080 (default)
#' @param proxyauth A numeric specifying the Proxy Authentication Method : 0 for
#'   NONE 1 for BASIC 2 for DIGEST 4 for NEGOTIATE 8 for NTLM 15 for ANY
#'   (default)
#' @param extra_curl_opts A list of additional named options to pass into
#'   curl::handle_setopt(), e.g. list(timeout=60)
#'
#' @examples
#' \dontrun{
#' library(gtrendsR)
#'
#' setHandleParameters(
#'   user = "xxxx",
#'   password = "*******",
#'   domain = "mydomain",
#'   proxyhost = "10.111.124.113"
#' )
#'
#' res <- gtrends(c("nhl", "nba"), geo = c("CA", "US"))
#'
#' # include additional curl options
#' setHandleParameters(
#'   user = "xxxx",
#'   password = "*******",
#'   domain = "mydomain",
#'   proxyhost = "10.111.124.113",
#'   extra_curl_opts = list(timeout = 60)
#' )
#' }
#'
#' @export

setHandleParameters <- function(
  user = NULL,
  password = NULL,
  domain = NULL,
  proxyhost = NULL,
  proxyport = 8080,
  proxyauth = 15,
  extra_curl_opts = list()
) {
  .pkgenv[["handle_user"]] <- user
  .pkgenv[["handle_password"]] <- password
  .pkgenv[["handle_domain"]] <- domain
  .pkgenv[["handle_proxyhost"]] <- proxyhost
  .pkgenv[["handle_proxyport"]] <- proxyport
  .pkgenv[["handle_proxyauth"]] <- as.integer(proxyauth)
  if (!is.list(extra_curl_opts)) {
    stop(
      "extra_curl_opts must be a list of name-value pairs for passing to curl::handle_setopt()"
    )
  }
  .pkgenv[["handle_extra_curl_opts"]] <- extra_curl_opts
}

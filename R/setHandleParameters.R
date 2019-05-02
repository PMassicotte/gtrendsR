#' If \code{gtrends} should be used behind a proxy,
#' especially with NTLM authentication mode,
#' you need to set the proxy parameters and credentials using "setHandleParameters" function
#'
#' @param user A string specifying your username
#' @param password A string specifying your password
#' @param domain A string specifying the authentication domain
#' @param proxyhost A string specifying the Proxy host DNS or IP address
#' @param proxyport A numeric specifying the Proxy Port : 8080 (default)
#' @param proxyauth A numeric specifying the Proxy Authentication Method :
#'   0 for NONE
#'   1 for BASIC
#'   2 for DIGEST
#'   4 for NEGOTIATE
#'   8 for NTLM
#'  15 for ANY (default)
#'
#' @examples
#'
#' \dontrun{
#' library(gtrendsR)
#'
#' setHandleParameters(user="xxxx",password="*******",domain="mydomain",proxyhost = "10.111.124.113")
#' res <- gtrends(c("nhl", "nba"), geo = c("CA", "US"))
#' }
#' @export

setHandleParameters <- function(user = NULL, password = NULL, domain = NULL, proxyhost = NULL, proxyport = 8080, proxyauth = 15) {
  .pkgenv[["handle_user"]] <- user
  .pkgenv[["handle_password"]] <- password
  .pkgenv[["handle_domain"]] <- domain
  .pkgenv[["handle_proxyhost"]] <- proxyhost
  .pkgenv[["handle_proxyport"]] <- proxyport
  .pkgenv[["handle_proxyauth"]] <- as.integer(proxyauth)
}

.pkgenv <- new.env(parent=emptyenv())

.onAttach <- function(libname, pkgname) {
    ## check the R option, as well as an environment variable
    if (getOption("google.autoconnect", FALSE) ||
        tolower(Sys.getenv("GOOGLE_AUTOCONNECT")) == "true") {
        ## get connection, gconnect will deal with user and password
        ch <- gconnect(verbose=FALSE)   # flip to TRUE to see message
    } else {
        ## store a fallback value
        ch <- NULL
    }

    ## assign connection here for retrieval later
    assign("ch", ch, envir=.pkgenv)
}

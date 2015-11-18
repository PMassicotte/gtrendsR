.pkgenv <- new.env(parent=emptyenv())

.onAttach <- function(libname, pkgname) {
    ## we could auto-connect here just like Rblpapi
    ## we already respect two env.vars and options
    ## so why not add another pair for autoconecct?

    ## store a fallback value
    assign("ch", NULL, envir=.pkgenv)

}

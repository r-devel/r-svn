upstream <- function() "upstream"

.onLoad <- function(libname, pkgname) {
    do_downstream <- function(...) testIsLoadedDownstream::downstream()

    ## isNamespaceLoaded() and the `%in% loadedNamespaces()` idiom are
    ## documented as equivalent and must agree
    loaded <- isNamespaceLoaded("testIsLoadedDownstream")
    if (loaded != ("testIsLoadedDownstream" %in% loadedNamespaces()))
        stop("isNamespaceLoaded() and loadedNamespaces() disagree for ",
             "testIsLoadedDownstream")

    ## If testIsLoadedDownstream is already loaded we use its exports now,
    ## otherwise we defer until it finishes loading.  The deferral branch is
    ## the one the test exercises.
    if (loaded) {
        do_downstream()
    } else {
        setHook(
            packageEvent("testIsLoadedDownstream", "onLoad"),
            do_downstream
        )
    }
}

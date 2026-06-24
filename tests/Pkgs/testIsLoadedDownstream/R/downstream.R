downstream <- function() "downstream"

.onLoad <- function(libname, pkgname) {
    ## Load testIsLoadedUpstream from within our own load, so its .onLoad
    ## probes isNamespaceLoaded("testIsLoadedDownstream") while we are still
    ## loading.  testIsLoadedUpstream is reached via `::` rather than a
    ## NAMESPACE import so that this happens in our .onLoad, after we are
    ## registered but before our exports are sealed, rather than earlier
    ## during import processing.
    testIsLoadedUpstream::upstream()
}

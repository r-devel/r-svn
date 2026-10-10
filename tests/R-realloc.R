## Tests of the R_realloc() C API; run with make test-Realloc.
## Needs a C compiler: an installation without one skips the tests.

## Some tests provoke errors that R_ToplevelExec() catches in C, whose
## messages would clutter the output. A genuine failure is re-raised.
callQuietly <- function(name, ...) {
    old <- options(show.error.messages = FALSE)
    res <- tryCatch(.Call(name, ..., PACKAGE = "realloc"), error = identity)
    options(old)
    if (inherits(res, "error"))
        stop(res)
}

local({
    src <- normalizePath(file.path(Sys.getenv("SRCDIR", "."), "R-realloc.c"))
    tmp <- tempfile("R-realloc-")
    dir.create(tmp)
    oldwd <- setwd(tmp)
    on.exit({setwd(oldwd); unlink(tmp, recursive = TRUE)})
    stopifnot(file.copy(src, "realloc.c"))
    r <- file.path(R.home("bin"), "R")

    status <- system2(
        r,
        c("CMD", "SHLIB", "realloc.c"),
        stdout = "build.log",
        stderr = "build.log"
    )
    if (status != 0L) {
        ## Distinguish a missing toolchain from a genuine build failure.
        writeLines("void realloc_probe(void) {}", "probe.c")
        probe <- system2(
            r,
            c("CMD", "SHLIB", "probe.c"),
            stdout = FALSE,
            stderr = FALSE
        )
        if (probe != 0L) {
            message("no working C compiler found: skipping R_realloc() tests")
            return(invisible())
        }
        stop(paste(readLines("build.log"), collapse = "\n"))
    }
    dll <- dyn.load(paste0("realloc", .Platform$dynlib.ext))
    on.exit(dyn.unload(dll[["path"]]), add = TRUE, after = FALSE)

    ## Force a genuine allocation failure without exhausting system memory.
    oldlimit <- mem.maxVSize()
    limit <- ceiling(gc()[2L, 3L] * 8 / 1024^2) + 16
    mem.maxVSize(limit)
    on.exit(mem.maxVSize(oldlimit), add = TRUE)

    .Call("test_basic", PACKAGE = "realloc")
    callQuietly("test_contexts")
    callQuietly("test_errors", 2 * limit * 1024^2)
    msg <- tryCatch(.Call("test_stale", PACKAGE = "realloc"),
                    error = conditionMessage)
    stopifnot(identical(msg,
        "'R_realloc' called on a pointer to a block it has already resized"))

    gctorture(TRUE)
    on.exit(gctorture(FALSE), add = TRUE)
    .Call("test_basic", PACKAGE = "realloc")
    callQuietly("test_contexts")
    callQuietly("test_errors", 2 * limit * 1024^2)
    gctorture(FALSE)

    ## The reclaim measurements hold up to 24 Mb of buffers at once.
    mem.maxVSize(oldlimit)

    ## gc() reports Vcells in units of eight bytes. Allow for R's own
    ## allocations. A replaced or released block is reclaimed at once.
    used <- .Call("test_reclaim", function() gc()[2L, 1L], PACKAGE = "realloc")
    used <- (used - used[1L]) / 1024^2
    stopifnot(all(abs(used - c(0, 1, 2, 0, 0)) < 4096 / 1024^2))
})

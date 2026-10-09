## Tests of the R_realloc() C API; run with make test-Realloc.
## Needs a C compiler: an installation without one skips the tests.
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

    old <- options(show.error.messages = FALSE)
    on.exit(options(old), add = TRUE)
    ## Force a genuine allocation failure without exhausting system memory.
    oldlimit <- mem.maxVSize()
    limit <- ceiling(gc()[2L, 3L] * 8 / 1024^2) + 16
    mem.maxVSize(limit)
    on.exit(mem.maxVSize(oldlimit), add = TRUE)
    run <- function() {
        .Call("test_basic", PACKAGE = "realloc")
        .Call("test_contexts", PACKAGE = "realloc")
        .Call("test_errors", 2 * limit * 1024^2, PACKAGE = "realloc")
    }
    run()
    gctorture(TRUE)
    tryCatch(run(), finally = gctorture(FALSE))
    ## The reclaim measurements hold up to 24 Mb of buffers at once.
    mem.maxVSize(oldlimit)

    ## gc() reports Vcells in units of eight bytes. Allow for R's own
    ## allocations. A block from R_realloc(NULL) is released as soon as it
    ## is replaced; one from R_alloc() stays until the stack unwinds.
    reclaim <- function(resizable) {
        used <- .Call("test_reclaim", function() gc()[2L, 1L], resizable,
                      PACKAGE = "realloc")
        (used - used[1L]) / 1024^2
    }
    stopifnot(
        all(abs(reclaim(TRUE) - c(0, 1, 2, 0, 0)) < 4096 / 1024^2),
        all(abs(reclaim(FALSE) - c(0, 1, 3, 1, 0)) < 4096 / 1024^2)
    )
})

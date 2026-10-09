## Standalone C API tests; run with make R-realloc.Rout or make test-Misc-dev.
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
    if (status != 0L) stop(paste(readLines("build.log"), collapse = "\n"))
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

    ## gc() reports Vcells in units of eight bytes. Allow for R's own allocations.
    used <- .Call("test_reclaim", function() gc()[2L, 1L], PACKAGE = "realloc")
    expected <- c(0, 1, 2, 0, 0) * 1024^2
    stopifnot(all(abs((used - used[1L]) - expected) < 4096))
})

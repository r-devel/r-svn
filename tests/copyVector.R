## Rf_copyVector() with overlapping source and destination ranges
stopifnot(file.copy(file.path(Sys.getenv("SRCDIR", "."), "copyVector.c"), tempdir()))
setwd(tempdir())
stopifnot(system2(file.path(R.home("bin"), "R"), c("CMD", "SHLIB", "copyVector.c")) == 0L)
dll <- dyn.load(paste0("copyVector", .Platform$dynlib.ext))
copy_views <- getNativeSymbolInfo("copy_views", dll)

check <- function(from, m, to, n) {
    x <- as.raw(seq_len(max(from + m, to + n) + 1L) %% 251L)
    want <- x
    want[to + seq_len(n)] <- x[from + (seq_len(n) - 1L) %% m + 1L]
    got <- .Call(copy_views, x, as.integer(c(from, m)), as.integer(c(to, n)))
    stopifnot(identical(got, want))
}

check(3, 2, 4, 9)                  # alternating pair and odd tail
check(3, 5, 4, 21)                 # short period doubled in the destination
check(2, 8193, 3, 17001)           # first copy overwrites part of the source
check(3, 8193, 2, 17001)           # overlap in the other direction
check(19000, 8193, 1, 40001)       # only later repetitions overwrite the source
check(2, 1000004, 3, 1000002)      # truncated first copy spans two chunks
check(3, 1000003, 2, 2000011)      # chunked first copy followed by recycling
check(2, 1000003, 2, 1000003)      # exactly the same range
check(2, 1000003, 2, 2000011)      # same start, different lengths
## Overlapping copies could overwrite source values in R <= 4.6.2.

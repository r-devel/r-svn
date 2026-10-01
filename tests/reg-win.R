### Windows-only regression tests

## closing a graphics window could segfault in Windows
windows(record = TRUE)
plot(1)
dev.off()
gc()
## segfaulted in 2.0.0


## Using a closed progress bar (PR#13709)
bar = winProgressBar(min = 0, max = 100, width = 300)
setWinProgressBar(bar, 25)
close(bar)
try(setWinProgressBar(bar, 50))
## segfaulted in 2.9.0


## trio peculiarity with %a, and incorrect fix
x <- sprintf("%a", 1:8)
y <- c("0x1p+0", "0x1p+1", "0x1.8p+1", "0x1p+2", "0x1.4p+2", "0x1.8p+2",
       "0x1.cp+2", "0x1p+3")
stopifnot(identical(x, y))


## binary mode in download.file(,method="wininet") (PR#17715)
src <- file.path(tempdir(), "source.bin")
dst <- file.path(tempdir(), "target.bin")
url <- file.path("file://", src)
d <- as.raw(0x1a)
writeBin(d, src)
download.file(url, dst, method = "wininet", mode = "wb")
dstbin <- readBin(dst, "raw")
stopifnot(identical(d, dstbin))


## file.copy(copy.date = TRUE) opened the files for exclusive access when
## copying the file time, so the time was not copied if the source file was
## open elsewhere and concurrent attempts to open the source file could fail (PR#19187)
src <- tempfile("source")
dir <- tempfile("target")
writeLines("hello", src)
dir.create(dir)
Sys.setFileTime(src, as.POSIXct("2020-01-01 12:00:00", tz = "UTC"))
con <- file(src, "rb")
## copy into an existing directory, so that the internal copy code is used
ok <- file.copy(src, dir, copy.date = TRUE)
close(con)
dst <- file.path(dir, basename(src))
stopifnot(ok, file.mtime(src) == print(file.mtime(dst)))
## the file time was not copied in R <= 4.6.1


## Starting a new profile must finish the previous sampling thread.
if (capabilities("Rprof")) local({
    files <- c(tempfile("Rprof-old"), tempfile("Rprof-new"))
    on.exit({ Rprof(NULL); unlink(files) })
    Rprof(NULL)
    for (i in 1:5) {
        Rprof(files[1L], memory.profiling = TRUE)
        out <- lapply(1:10000, rnorm, n = 512)
        Rprof(files[2L], memory.profiling = TRUE)
        old <- readLines(files[1L])
        out <- lapply(1:10000, rnorm, n = 512)
        Rprof(NULL)
        Rprof(NULL) # stopping an inactive profiler must be harmless
        stopifnot(identical(old, readLines(files[1L])))
        for (file in files) {
            s <- summaryRprof(file, memory = "tseries", chunksize = 1L)
            stopifnot(is.data.frame(s), nrow(s) > 0L)
        }
    }
})
## the old thread could still write after Rprof(NULL), or into a new profile

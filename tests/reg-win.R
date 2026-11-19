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


## formatted output from the UCRT rather than trio
stopifnot(exprs = {
    identical(sprintf("%.0f", 2^70), "1180591620717411303424")
    identical(format(2^70, scientific = FALSE), "1180591620717411303424")
    identical(sprintf("%.0f", c(0.5, 1.5, 2.5)), c("0", "2", "2"))
    identical(sprintf("%.3f", 18266.3125), "18266.312")
    identical(sprintf("%.15g", -128710289995254.5), "-128710289995254")
    identical(sprintf(c("%a", "%A", "%.3a", "%10a|"), c(0.1, 255.5, 1/3, 1)),
              c("0x1.999999999999ap-4", "0X1.FFP+7", "0x1.555p-2", "    0x1p+0|"))
    identical(deparse(c(1, 2^-1074, 2^1023), control = "hexNumeric"),
              "c(0x1p+0, 0x0.0000000000001p-1022, 0x1p+1023)")
    grepl("^<environment: 0x[0-9a-f]{16}>$", format(new.env()))
    identical(format(new("externalptr")), "<pointer: (nil)>")
})
## wrong digits beyond about 20 significant ones, and some ties, in R <= 4.6.x


## positional arguments in translated C-level messages, in a child process
## as these tests run in the C locale, which has no translations
if (capabilities("NLS") && !is.na(.popath)) {
    lc <- Sys.getenv(c("LC_ALL", "LC_CTYPE"), unset = NA)
    Sys.unsetenv(names(lc))
    out <- system2(file.path(R.home("bin"), "Rscript"),
                   c("--vanilla", "-e",
                     shQuote("Sys.setLanguage('ja'); sprintf('%s', 1, 2, 3)")),
                   stdout = TRUE, stderr = TRUE)
    if (any(!is.na(lc)))
        do.call(Sys.setenv, as.list(lc[!is.na(lc)]))
    stopifnot(is.null(attr(out, "status")))
    ## unless the Japanese catalog could not be used
    if (!any(grepl("arguments not used", out, fixed = TRUE)))
        stopifnot(any(grepl("'%s'[^0-9$]*2[^0-9]", out, useBytes = TRUE)),
                  !any(grepl("$", out, fixed = TRUE)))
}
## a plain UCRT printf gives "$s" for "%2$s"; trio was fine in R <= 4.6.x


## %ls arguments the locale cannot represent are shown as <U+xxxx>
old <- Sys.getlocale("LC_CTYPE")
Sys.setlocale("LC_CTYPE", "C")
w <- tryCatch(file(file.path(tempdir(), "\u20ac\u0101.txt"), "r"),
              warning = conditionMessage)
Sys.setlocale("LC_CTYPE", old)
stopifnot(grepl("<U+20AC><U+0101>.txt", w, fixed = TRUE))
## a plain UCRT printf loses the message; trio was fine in R <= 4.6.x

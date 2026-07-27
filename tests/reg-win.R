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


## dyn.load() failing because a dependent DLL cannot be found should
## name the missing module (needs a toolchain, as when building R)
if (nzchar(Sys.which("make")) && nzchar(Sys.which("gcc"))) {
    owd <- setwd(tempdir())
    dir.create("dll-deps-test")
    setwd("dll-deps-test")
    rcmd <- file.path(R.home("bin"), "Rcmd.exe")

    ## a helper DLL, moved into a directory not on the DLL search path
    writeLines("void dep_fn(void) {}", "fakedep.c")
    stopifnot(system2(rcmd, c("SHLIB", "-o", "fakedep.dll",
                              "fakedep.c")) == 0L)
    dir.create("deps")
    stopifnot(file.rename("fakedep.dll", "deps/fakedep.dll"))

    ## a DLL linked against the helper
    writeLines(c("extern void dep_fn(void);",
                 "void use_dep(void) { dep_fn(); }"),
               "needsdep.c")
    stopifnot(system2(rcmd, c("SHLIB", "-o", "needsdep.dll", "needsdep.c",
                              "-Ldeps", "-lfakedep")) == 0L)

    ## the dependency cannot be found, so loading must fail, and the
    ## error message should identify the missing module
    msg <- tryCatch(dyn.load("needsdep.dll"), error = conditionMessage)
    stopifnot(is.character(msg),
              grepl("missing module 'fakedep.dll' required by 'needsdep.dll'",
                    msg, fixed = TRUE))

    ## sanity check: with DLLpath the dependency is found and loading works
    dll <- dyn.load("needsdep.dll", DLLpath = file.path(getwd(), "deps"))
    dyn.unload(dll[["path"]])

    setwd(owd)
}

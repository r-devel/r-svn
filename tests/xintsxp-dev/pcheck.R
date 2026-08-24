## Text conversion checked against exact arithmetic computed
## independently of the implementation.
##
## The round-trip test in the gauntlet only shows that as.xinteger() and
## as.character() invert each other, which two mirrored bugs would also
## pass.  This checks both directions against an outside reference:
## the decimal text a value has, and the native-order bytes it is
## stored in, both derived in bignum.R from decimal digit vectors --
## which share nothing with the byte kernels under test.
##
## Self-contained, no external tools:
##   build/bin/Rscript tests/xintsxp-dev/pcheck.R

.xintsxpDir <- local({
    a <- commandArgs(FALSE)
    hit <- startsWith(a, "--file=")
    f <- if (any(hit)) sub("^--file=", "", a[hit][1L])
         else { i <- match("-f", a, nomatch = 0L); if (i) a[i + 1L] else "" }
    if (nzchar(f)) dirname(f) else "."
})
source(file.path(.xintsxpDir, "bignum.R"))
bnSelfTest()

set.seed(7)
fails <- 0L
combos <- list(list(1L,"signed"), list(2L,"unsigned"), list(4L,"signed"),
               list(8L,"unsigned"), list(8L,"signed"), list(16L,"unsigned"),
               list(16L,"signed"))

for (spec in combos) {
    w <- spec[[1L]]; k <- spec[[2L]]
    txt <- bnRandomValues(w, k, 250L)

    ## parse the decimal text
    got <- as.xinteger(txt, w, k)

    ## the same values as their native-order bytes, computed here and
    ## taken verbatim -- the ingest path, with no transform
    want <- as.xinteger(as.raw(unlist(lapply(txt, bnToBytes, width = w, kind = k))),
                     w, k)

    payloadOK <- identical(xintegerRaw(got), xintegerRaw(want))
    textOK <- identical(as.character(got), txt)
    ## and the inverse: the bytes read back as the text they name
    backOK <- identical(vapply(seq_along(txt), function(i)
        bnFromBytes(xintegerRaw(got[i]), w, k), ""), txt)
    if (!payloadOK || !textOK || !backOK) fails <- fails + 1L
    cat(sprintf("%2d,%-9s n=%-4d payload %s  text %s  inverse %s\n", w, k,
                length(txt),
                if (payloadOK) "ok  " else "FAIL",
                if (textOK) "ok  " else "FAIL",
                if (backOK) "ok  " else "FAIL"))
}

cat(sprintf("\n%d failure(s)\n", fails))
if (fails) quit(status = 1L)

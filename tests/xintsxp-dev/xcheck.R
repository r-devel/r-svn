## Decimal rendering and the identity operations, against exact
## arithmetic computed independently of the implementation.
##
## R has no arbitrary-precision integer type, so a 128-bit value's
## decimal text cannot be checked against anything R computes directly,
## and a round-trip test would pass with two mirrored bugs.  bignum.R
## supplies the reference: decimal digit vectors and schoolbook base-10
## algorithms, sharing nothing with the byte kernels under test.
##
## Self-contained, no external tools:
##   build/bin/Rscript tests/xintsxp-dev/xcheck.R

.xintsxpDir <- local({
    a <- commandArgs(FALSE)
    hit <- startsWith(a, "--file=")
    f <- if (any(hit)) sub("^--file=", "", a[hit][1L])
         else { i <- match("-f", a, nomatch = 0L); if (i) a[i + 1L] else "" }
    if (nzchar(f)) dirname(f) else "."
})
source(file.path(.xintsxpDir, "bignum.R"))
bnSelfTest()

set.seed(11)
fails <- 0L
chk <- function(l, c) { if (!isTRUE(c)) fails <<- fails + 1L
                        cat(sprintf("%-42s %s\n", l, if (isTRUE(c)) "ok" else "FAIL")) }

for (spec in list(list(8L, "unsigned"), list(8L, "signed"), list(16L, "signed"),
                  list(4L, "unsigned"), list(16L, "unsigned"),
                  list(2L, "unsigned"), list(16L, "signed"))) {
    w <- spec[[1L]]; k <- spec[[2L]]

    ref <- bnRandomValues(w, k, 120L)
    ref <- sample(ref)                     # so order() has work to do
    ord <- order(bnKey(ref))               # the reference permutation

    ## the ingest path: build each element's payload independently, read
    ## the bytes back, reinterpret.  No transform.
    raw <- as.raw(unlist(lapply(ref, bnToBytes, width = w, kind = k)))
    x <- as.xinteger(raw, w, k)

    cat(sprintf("\n-- width %d, %s, n = %d --\n", w, k, length(x)))
    chk("length",                    length(x) == length(ref))
    chk("decimal matches reference", identical(as.character(x), ref))
    chk("order matches reference",   identical(order(x), ord))
    chk("sort matches reference",    identical(as.character(sort(x)), ref[ord]))
    chk("no spurious NA",            !anyNA(x))
    chk("round-trip to raw",         identical(xintegerRaw(x), raw))
    chk("unique count",              length(unique(x)) == length(unique(ref)))
    chk("match is identity",         identical(match(x, x), match(ref, ref)))
    chk("== is reflexive",           all(x == x))
    chk("is.unsorted agrees",        is.unsorted(x) == is.unsorted(order(x)))
    chk("sorted is sorted",          !is.unsorted(sort(x)))
    ## rank with ties.method="first" is the inverse of the order permutation
    chk("rank inverts order",        identical(rank(x, ties.method = "first"),
                                               as.integer(order(ord))))
    ## text is also the way back in, at every width the type allows
    chk("text parses back to x",     identical(as.xinteger(ref, w, k), x))
    ## and the payload the implementation stores is the one computed here
    chk("payload matches reference",
        identical(xintegerRaw(x[1L]), bnToBytes(ref[1L], w, k)))
    ## min and max pick the ends of that ordering
    chk("min is the first sorted",   identical(as.character(min(x)), ref[ord[1L]]))
    chk("max is the last sorted",
        identical(as.character(max(x)), ref[ord[length(ord)]]))
}
cat(sprintf("\n%d failure(s)\n", fails))
if (fails) quit(status = 1L)

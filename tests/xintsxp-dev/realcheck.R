## as.numeric() checked against a correctly-rounded reference computed
## independently of the implementation.
##
## A double is the nearest representable value or it is wrong, and
## "close enough" is not a standard a fixed-width integer type should be
## held to.  Accumulating the element byte by byte (d = d * 256 + byte)
## rounds again at every step once the running total passes 2^53, and
## the errors compound to as much as a whole ulp.
##
## The reference is bignum.R's bnToDouble(), written from the IEEE rule
## -- the top 53 significant bits, then a round bit and a sticky -- over
## decimal digit vectors.  The implementation takes the top eight
## significant XINTEGER payload into a uint64_t plus a sticky and lets the hardware
## convert; the two arrive at the same answer by different routes, which
## is what makes this a check rather than a restatement.
##
## Note this does NOT go through R's string -> double parser: Rstrtod is
## not correctly rounded for long digit strings either, so building the
## reference with as.numeric(text) would measure that instead and report
## mismatches where there are none.
##
## Self-contained, no external tools:
##   build/bin/Rscript tests/xintsxp-dev/realcheck.R

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
big <- function(n, digits) vapply(seq_len(n), function(i)
    paste0(sample(1:9, 1), paste(sample(0:9, digits - 1, TRUE), collapse = "")), "")

total <- 0L; bad <- 0L
for (spec in list(list(8L, "unsigned", 19), list(8L, "signed", 18),
                  list(16L, "unsigned", 38), list(16L, "signed", 37),
                  list(4L, "unsigned", 9), list(2L, "unsigned", 5),
                  list(1L, "unsigned", 3), list(1L, "signed", 3))) {
    w <- spec[[1L]]; k <- spec[[2L]]; nd <- spec[[3L]]
    ## the long strings exercise the rounding; the short ones must stay
    ## exact, and a mistake in the sticky bit would show up in both
    txt <- unique(c(big(250, nd), big(60, max(1, nd - 4)), "0", "1", "12345",
                    if (k == "signed") paste0("-", big(60, nd - 1))))
    v <- suppressWarnings(as.xinteger(txt, w, k))
    keep <- !is.na(v)			# out of range for this width
    txt <- txt[keep]; v <- v[keep]

    got <- suppressWarnings(as.numeric(v))
    want <- unname(vapply(txt, bnToDouble, 0))

    n <- length(txt); total <- total + n
    ## identical() on doubles, so a difference of one ulp counts
    off <- which(!vapply(seq_len(n), function(i)
        identical(got[i], want[i]), NA))
    bad <- bad + length(off)
    cat(sprintf("%2d,%-9s n=%4d  %s\n", w, k, n,
                if (length(off))
                    sprintf("MISMATCH %d, e.g. %s (%.17g vs %.17g)",
                            length(off), txt[off[1L]], got[off[1L]], want[off[1L]])
                else "ok"))
}

cat(sprintf("\n%d values, %d not the nearest double\n", total, bad))
if (bad) stop("as.numeric() is not correctly rounded")

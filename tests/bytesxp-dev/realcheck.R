## as.numeric() checked against Python's correctly-rounded float().
##
## A double is the nearest representable value or it is wrong, and
## "close enough" is not a standard a fixed-width integer type should be
## held to.  Accumulating the element byte by byte (d = d * 256 + byte)
## rounds again at every step once the running total passes 2^53, and
## the errors compound to as much as a whole ulp.
##
## Compared as BIT PATTERNS, not as printed numbers: R's own string ->
## double parser is not correctly rounded for long digit strings either,
## so reading the reference back with as.numeric() would measure that
## instead and report mismatches where there are none.
##
## Self-contained -- it generates its own reference with python3:
##   build/bin/Rscript tests/bytesxp-dev/realcheck.R

set.seed(11)
big <- function(n, digits) vapply(seq_len(n), function(i)
    paste0(sample(1:9, 1), paste(sample(0:9, digits - 1, TRUE), collapse = "")), "")
hexd <- function(d) vapply(d, function(z)
    paste(sprintf("%02x", as.integer(writeBin(z, raw(), endian = "little"))),
          collapse = ""), "")
PY <- r"---(
import sys, struct
for l in sys.stdin.read().split():
    print(struct.pack('<d', float(int(l))).hex())
)---"

total <- 0L; bad <- 0L
for (spec in list(list(8L, "unsigned", 19), list(8L, "signed", 18),
                  list(16L, "unsigned", 38), list(16L, "signed", 37),
                  list(4L, "unsigned", 9), list(21L, "unsigned", 50),
                  list(7L, "unsigned", 16), list(9L, "signed", 20))) {
    w <- spec[[1L]]; k <- spec[[2L]]; nd <- spec[[3L]]
    ## the long strings exercise the rounding; the short ones must stay
    ## exact, and a mistake in the sticky bit would show up in both
    txt <- unique(c(big(1500, nd), big(300, max(1, nd - 4)), "0", "1", "12345",
                    if (k == "signed") paste0("-", big(300, nd - 1))))
    v <- suppressWarnings(as.bytes(txt, w, k))
    keep <- !is.na(v)			# out of range for this width
    txt <- txt[keep]; v <- v[keep]

    got <- hexd(suppressWarnings(as.numeric(v)))
    want <- system2("python3", c("-c", shQuote(PY)),
                    input = paste(txt, collapse = "\n"), stdout = TRUE)

    n <- length(txt); total <- total + n
    off <- which(got != want)
    bad <- bad + length(off)
    cat(sprintf("%2d,%-9s n=%4d  %s\n", w, k, n,
                if (length(off))
                    sprintf("MISMATCH %d, e.g. %s (%s vs %s)",
                            length(off), txt[off[1L]], got[off[1L]], want[off[1L]])
                else "ok"))
}

cat(sprintf("\n%d values, %d not the nearest double\n", total, bad))
if (bad) stop("as.numeric() is not correctly rounded")

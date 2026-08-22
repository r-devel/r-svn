## Decimal rendering and the identity operations, against Python's
## exact integers.
##
## The point is an outside authority.  R has no arbitrary-precision
## integer type, so a 128-bit value's decimal text cannot be checked
## against anything R computes -- and a round-trip test would pass with
## two mirrored bugs.  Python's ints are exact at every width here.
##
## Self-contained -- it generates its own reference with python3:
##   build/bin/Rscript tests/bytesxp-dev/xcheck.R
##
## The reference is written in the machine's OWN byte order
## (sys.byteorder), which is what as.bytes(<raw>) reinterprets, so this
## is correct on a big-endian machine too.  See endcheck.R for the
## checks that hold on any platform without needing python3 at all.

REF <- tempfile(fileext = ".tsv")
PY <- r"---(
import random, sys
random.seed(11)
rows = []
combos = [(8,"unsigned"), (8,"signed"), (16,"signed"), (4,"unsigned"),
          (16,"unsigned"), (3,"unsigned"), (9,"signed")]
for w, kind in combos:
    bits = 8 * w
    if kind == "unsigned":
        lo, hi = 0, 2**bits - 1
    else:
        lo, hi = -(2**(bits-1)), 2**(bits-1) - 1
    reserved = hi if kind == "unsigned" else lo    # the value NA takes
    vals = set(v for v in (lo, lo+1, -1, 0, 1, hi-1, hi) if lo <= v <= hi)
    while len(vals) < 300:                         # weighted to the edges
        r = random.random()
        if r < .3:   v = random.randint(lo, min(hi, lo + 1000))
        elif r < .6: v = random.randint(max(lo, hi - 1000), hi)
        else:        v = random.randint(lo, hi)
        vals.add(v)
    vals = [v for v in vals if v != reserved]
    random.shuffle(vals)                           # so order() has work to do
    # 1-based stable order permutation, ties broken by position as R does
    perm = sorted(range(len(vals)), key = lambda i: (vals[i], i))
    payload = b"".join(v.to_bytes(w, sys.byteorder,
                                  signed = (kind == "signed")) for v in vals)
    rows.append("\t".join([
        "%d,%s" % (w, kind),
        payload.hex(),
        ",".join(str(v) for v in vals),
        ",".join(str(i + 1) for i in perm)]))
open(sys.argv[1], "w").write("\n".join(rows) + "\n")
)---"
system2("python3", c("-c", shQuote(PY), shQuote(REF)))

fails <- 0L
chk <- function(l, c) { if (!isTRUE(c)) fails <<- fails + 1L
                        cat(sprintf("%-42s %s\n", l, if (isTRUE(c)) "ok" else "FAIL")) }

for (line in readLines(REF)) {
    f <- strsplit(line, "\t", fixed = TRUE)[[1L]]
    w <- as.integer(sub(",.*", "", f[1L])); k <- sub(".*,", "", f[1L])
    hex <- f[2L]
    ref <- strsplit(f[3L], ",", fixed = TRUE)[[1L]]
    ord <- as.integer(strsplit(f[4L], ",", fixed = TRUE)[[1L]])

    ## the ingest path: read the bytes, reinterpret, no transform
    raw <- as.raw(strtoi(substring(hex, seq(1, nchar(hex) - 1, 2),
                                   seq(2, nchar(hex), 2)), 16L))
    x <- as.bytes(raw, w, k)

    cat(sprintf("\n-- width %d, %s, n = %d --\n", w, k, length(x)))
    chk("length",                    length(x) == length(ref))
    chk("decimal matches reference", identical(as.character(x), ref))
    chk("order matches reference",   identical(order(x), ord))
    chk("sort matches reference",    identical(as.character(sort(x)), ref[ord]))
    chk("no spurious NA",            !anyNA(x))
    chk("round-trip to raw",         identical(bytesRaw(x), raw))
    chk("unique count",              length(unique(x)) == length(unique(ref)))
    chk("match is identity",         identical(match(x, x), match(ref, ref)))
    chk("== is reflexive",           all(x == x))
    chk("is.unsorted agrees",        is.unsorted(x) == is.unsorted(order(x)))
    chk("sorted is sorted",          !is.unsorted(sort(x)))
    ## rank with ties.method="first" is the inverse of the order permutation
    chk("rank inverts order",        identical(rank(x, ties.method = "first"),
                                               as.integer(order(ord))))
    ## text is also the way back in, at every width the type allows
    chk("text parses back to x",     identical(as.bytes(ref, w, k), x))
}
cat(sprintf("\n%d failure(s)\n", fails))
if (fails) quit(status = 1L)

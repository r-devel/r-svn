## The byte radix sort, against Python.
##
## sort() and order() take an LSD counting sort for the numeric kinds
## and, since the opaque kind's lexicographic order is exactly what a
## byte radix produces, for that one too.  R's comparison path breaks
## ties by index, so the radix has to be STABLE and has to agree with
## it -- which is why the values here are drawn from a SMALL POOL.  Ties
## everywhere is where a stability bug hides; distinct values would not
## find one.
##
## Decreasing order is its own case: the implementation complements the
## keys, which reverses the order while KEEPING the ascending index
## tiebreak that R gives.  Getting that backwards is invisible without
## ties, so it is checked separately.
##
## Self-contained -- it generates its own reference with python3:
##   build/bin/Rscript tests/bytesxp-dev/rxcheck.R
##
## The payload is written in the machine's own byte order, so this is
## correct on a big-endian machine too.

REF <- tempfile(fileext = ".tsv")
PY <- r"---(
import random, sys
random.seed(23)
rows = []
combos = [(8,"unsigned"), (8,"signed"), (16,"signed"), (4,"unsigned"),
          (1,"signed"), (2,"unsigned"), (16,"opaque"), (5,"opaque")]
N = 400
for w, kind in combos:
    bits = 8 * w
    if kind == "opaque":
        # byte strings, ordered lexicographically; a small pool of them
        pool = [bytes(random.randrange(256) for _ in range(w))
                for _ in range(max(4, N // 40))]
        vals = [random.choice(pool) for _ in range(N)]
        if any(v == b"\xff" * w for v in vals):      # the reserved NA
            vals = [v for v in vals if v != b"\xff" * w]
        keys = list(vals)                            # compare as bytes
        payload = b"".join(vals)
    else:
        if kind == "unsigned":
            lo, hi = 0, 2**bits - 1
        else:
            lo, hi = -(2**(bits-1)), 2**(bits-1) - 1
        reserved = hi if kind == "unsigned" else lo
        pool = [v for v in
                (random.randint(lo, hi) for _ in range(max(4, N // 40)))
                if v != reserved]
        if not pool: pool = [0]
        vals = [random.choice(pool) for _ in range(N)]
        keys = list(vals)                            # compare by value
        payload = b"".join(v.to_bytes(w, sys.byteorder,
                                      signed = (kind == "signed"))
                           for v in vals)
    n = len(keys)
    # R's order(): ties keep ascending index, in BOTH directions
    asc  = sorted(range(n), key = lambda i: (keys[i], i))
    desc = sorted(range(n), key = lambda i: i)
    desc = sorted(desc, key = lambda i: keys[i], reverse = True)
    rows.append("\t".join([
        "%d,%s" % (w, kind),
        payload.hex(),
        ",".join(str(i + 1) for i in asc),
        ",".join(str(i + 1) for i in desc)]))
open(sys.argv[1], "w").write("\n".join(rows) + "\n")
)---"
system2("python3", c("-c", shQuote(PY), shQuote(REF)))

fails <- 0L
chk <- function(l, c) { if (!isTRUE(c)) fails <<- fails + 1L
                        cat(sprintf("%-38s %s\n", l, if (isTRUE(c)) "ok" else "FAIL")) }

for (line in readLines(REF)) {
    f <- strsplit(line, "\t", fixed = TRUE)[[1L]]
    w <- as.integer(sub(",.*", "", f[1L])); k <- sub(".*,", "", f[1L])
    hex <- f[2L]
    asc  <- as.integer(strsplit(f[3L], ",", fixed = TRUE)[[1L]])
    desc <- as.integer(strsplit(f[4L], ",", fixed = TRUE)[[1L]])

    raw <- as.raw(strtoi(substring(hex, seq(1, nchar(hex) - 1, 2),
                                   seq(2, nchar(hex), 2)), 16L))
    x <- as.bytes(raw, w, k)

    cat(sprintf("\n-- width %d, %s, n = %d, heavy ties --\n", w, k, length(x)))
    chk("order ascending (stable ties)",  identical(order(x), asc))
    chk("order decreasing (stable ties)", identical(order(x, decreasing = TRUE), desc))
    chk("sort matches order",             identical(sort(x), x[asc]))
    chk("sort decreasing matches order",  identical(sort(x, decreasing = TRUE), x[desc]))
    chk("sorted really is sorted",        !is.unsorted(sort(x)))
    chk("ties really are present",        length(unique(x)) < length(x))
    ## NAs must still land per na.last, on both sides
    half <- length(x) %/% 2L
    xn <- c(x[seq_len(half)], rep(as.bytes(NA, w, k), 3L),
            x[(half + 1L):length(x)])
    chk("NA last by default",
        all(is.na(xn[order(xn)][(length(x) + 1L):(length(x) + 3L)])))
    chk("NA first when asked",
        all(is.na(xn[order(xn, na.last = FALSE)][1:3])))
    chk("sort drops NA",                  length(sort(xn)) == length(x))
    ## the comparison path and the radix must agree; xtfrm/rank go
    ## through order(), so this pins them to the same permutation
    chk("rank agrees with order",
        identical(rank(x, ties.method = "first"), as.integer(order(asc))))
}
cat(sprintf("\n%d failure(s)\n", fails))
if (fails) quit(status = 1L)

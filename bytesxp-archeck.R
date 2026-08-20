## Arithmetic checked against Python's exact integers.
##
## This is the oracle for the arithmetic kernels: every operation, at
## every width and kind, on operand pairs weighted to the range edges so
## that overflow is hit hard, and to magnitudes that put a product just
## either side of the boundary.  Python's // and % are already floor
## division and divisor-signed modulo, which is what R's %/% and %% mean
## for integers and therefore what these mean too.
##
## Self-contained -- it generates its own reference with python3:
##   ../build/bin/Rscript bytesxp-archeck.R

REF <- tempfile(fileext = ".tsv")
PY <- r"---(
import random, sys
random.seed(23)

COMBOS = [(1,"signed"),(1,"unsigned"),(2,"unsigned"),(4,"signed"),
          (8,"unsigned"),(8,"signed"),(16,"unsigned"),(16,"signed")]

def limits(w, kind):
    bits = 8 * w
    if kind == "unsigned":
        lo, hi = 0, 2**bits - 1
        reserved = hi                      # all bits set
    else:
        lo, hi = -(2**(bits-1)), 2**(bits-1) - 1
        reserved = lo                      # the most negative value
    return lo, hi, reserved

def mag(k):
    return 0 if k == 0 else (1 << (k - 1)) | random.getrandbits(k - 1)

def draw(lo, hi, k=None):
    if k is not None:                      # a magnitude of exactly k bits
        v = mag(k)
        return -v if lo < 0 and random.random() < .5 else v
    r = random.random()
    if r < .25:   return random.randint(lo, min(hi, lo + 3))
    if r < .50:   return random.randint(max(lo, hi - 3), hi)
    if r < .60:   return 0
    if r < .70:   return random.choice([1, -1] if lo < 0 else [1])
    return random.randint(lo, hi)

rows = []
for w, kind in COMBOS:
    lo, hi, reserved = limits(w, kind)

    def fits(v):
        # a result equal to the reserved value is reported as overflow:
        # it is not representable, and saying so beats a silent NA
        return "NA" if (v is None or v < lo or v > hi or v == reserved) else str(v)

    mbits = 8 * w - (kind == "signed")     # bits a magnitude can use

    n = 0
    while n < 400:
        if random.random() < .3:
            # magnitudes whose bit widths add up to about the width, so
            # that a product lands on either side of the overflow
            # boundary rather than always far past it -- multiply is
            # checked before the fact and this is where it is decided
            k = random.randint(1, mbits - 1)
            a = draw(lo, hi, k)
            b = draw(lo, hi, min(mbits, max(0, mbits - k + random.randint(-1, 1))))
        else:
            a, b = draw(lo, hi), draw(lo, hi)
        if a == reserved or b == reserved:
            continue                       # an operand that is really NA
        q = None if b == 0 else a // b
        r = None if b == 0 else a % b
        neg = -a if kind == "signed" else None
        rows.append("\t".join([
            "%d,%s" % (w, kind), str(a), str(b),
            fits(a + b), fits(a - b), fits(a * b), fits(q), fits(r),
            fits(neg) if neg is not None else ""]))
        n += 1

open(sys.argv[1], "w").write("\n".join(rows) + "\n")
)---"
system2("python3", c("-c", shQuote(PY), shQuote(REF)))

ref <- read.delim(REF, header = FALSE, colClasses = "character", sep = "\t",
                  na.strings = character())
names(ref) <- c("meta", "a", "b", "add", "sub", "mul", "idiv", "mod", "neg")

## our NA prints as NA; compare as character so exact 128-bit values survive
str_ <- function(v) { s <- as.character(v); s[is.na(v)] <- "NA"; s }

fails <- 0L
chk <- function(l, c) {
    if (!isTRUE(c)) fails <<- fails + 1L
    cat(sprintf("  %-10s %s\n", l, if (isTRUE(c)) "ok" else "FAIL"))
}

for (m in unique(ref$meta)) {
    s <- ref[ref$meta == m, ]
    w <- as.integer(sub(",.*", "", m))
    k <- sub(".*,", "", m)
    a <- as.bytes(s$a, w, k)
    b <- as.bytes(s$b, w, k)

    cat(sprintf("\n-- %s, %d pairs --\n", m, nrow(s)))
    got <- suppressWarnings(list(a + b, a - b, a * b, a %/% b, a %% b))
    for (j in seq_along(got))
        chk(c("+", "-", "*", "%/%", "%%")[j],
            identical(str_(got[[j]]), s[[c("add","sub","mul","idiv","mod")[j]]]))
    if (k == "signed")
        chk("unary -", identical(str_(suppressWarnings(-a)), s$neg))

    ## the reductions run their own accumulator loop, so they are not
    ## covered by the element-wise checks above
    tot <- suppressWarnings(sum(a))
    py <- sum(as.numeric(s$a))
    lo <- if (k == "unsigned") 0 else -(2^(8*w-1))
    hi <- if (k == "unsigned") 2^(8*w) - 1 else 2^(8*w-1) - 1
    chk("sum", if (py > lo && py < hi) !is.na(tot) else TRUE)
    chk("min/max", identical(str_(suppressWarnings(c(min(a), max(a)))),
                             c(min(s$a[order(a)][1]), s$a[order(a)][nrow(s)])))
}

## The two implementations against each other.  The Python reference
## above is authoritative but sampled; this is cheap enough to run over
## far more values, and it is the check that keeps the general kernels
## honest once a native type covers every arithmetic width and they stop
## being reachable in an ordinary session.
AB <- r"---(
set.seed(99)
str_ <- function(v) { s <- as.character(v); s[is.na(v)] <- "NA"; s }
for (spec in list(c(1,"signed"), c(1,"unsigned"), c(2,"unsigned"), c(4,"signed"),
                  c(8,"unsigned"), c(8,"signed"), c(16,"unsigned"), c(16,"signed"))) {
    w <- as.integer(spec[1]); k <- spec[2]
    n <- 20000
    ## operands include the reserved pattern often enough to exercise NA
    ## propagation, and the range edges often enough to exercise overflow
    a <- suppressWarnings(as.bytes(as.raw(sample(c(0:8, 247:255, 0:255), n*w, TRUE)), w, k))
    b <- suppressWarnings(as.bytes(as.raw(sample(c(0:3, 252:255, 0:255), n*w, TRUE)), w, k))
    r <- suppressWarnings(list(a+b, a-b, a*b, a %/% b, a %% b,
                               if (k == "signed") -a else a+a))
    cat(unlist(lapply(r, str_)), sep = "\n")
    cat("\n")
}
## So that this comparison cannot quietly become general-against-general
## -- which it was, at first, because an empty R_BYTES_GENERIC_ARITH
## still reads as set.  The two paths differ by ~100x on division, so
## the factor asserted below has an enormous margin.
d <- as.bytes(as.character(rep(1:100, length.out = 3e5)), 8L, "unsigned")
e <- as.bytes(as.character(rep(3:7, length.out = 3e5)), 8L, "unsigned")
cat("ELAPSED", min(replicate(3, system.time(d %/% e)[["elapsed"]])), "\n")
)---"
tmp <- tempfile(fileext = ".R")
writeLines(AB, tmp)
RS <- file.path(R.home("bin"), "Rscript")
nat <- system2(RS, shQuote(tmp), stdout = TRUE,
               env = "R_BYTES_GENERIC_ARITH=")
gen <- system2(RS, shQuote(tmp), stdout = TRUE,
               env = "R_BYTES_GENERIC_ARITH=1")

el <- function(x) as.numeric(sub("ELAPSED ", "", grep("^ELAPSED", x, value = TRUE)))
natT <- el(nat); genT <- el(gen)
nat <- grep("^ELAPSED", nat, value = TRUE, invert = TRUE)
gen <- grep("^ELAPSED", gen, value = TRUE, invert = TRUE)

cat("\n-- the two implementations against each other --\n")
chk(sprintf("%d results", length(nat)), length(nat) > 0 && identical(nat, gen))
chk(sprintf("paths differ (%.0fx)", genT / natT), genT > 2 * natT)
if (!identical(nat, gen)) {
    i <- which(nat != gen)[1:min(5, sum(nat != gen))]
    cat("  first differences at ", paste(i, collapse = ", "), "\n",
        "  native:  ", paste(nat[i], collapse = " "), "\n",
        "  general: ", paste(gen[i], collapse = " "), "\n", sep = "")
}

cat(sprintf("\n%d failure(s)\n", fails))
if (fails) quit(status = 1L)

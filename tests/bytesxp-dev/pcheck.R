## Text conversion checked against Python's exact integers.
##
## The round-trip test in the gauntlet only shows that as.bytes() and
## as.character() invert each other, which two mirrored bugs would also
## pass.  This checks both directions against an outside authority: the
## decimal text Python writes for a value, and the native-order bytes
## Python stores it in.
##
## Self-contained -- it generates its own reference with python3:
##   build/bin/Rscript tests/bytesxp-dev/pcheck.R

REF <- tempfile(fileext = ".tsv")
PY <- r"---(
import random, sys
random.seed(7)
rows = []
combos = [(1,"signed"),(2,"unsigned"),(4,"signed"),(8,"unsigned"),
          (8,"signed"),(16,"unsigned"),(16,"signed"),(21,"signed")]
for w, kind in combos:
    bits = 8 * w
    if kind == "unsigned":
        lo, hi = 0, 2**bits - 1
    else:
        lo, hi = -(2**(bits-1)), 2**(bits-1) - 1
    reserved = hi if kind == "unsigned" else lo   # the value NA takes
    vals = set(v for v in (lo, lo+1, -1, 0, 1, hi-1, hi) if lo <= v <= hi)
    while len(vals) < 250:                        # weighted to the edges,
        r = random.random()                       # where the bugs are
        if r < .3:   v = random.randint(lo, min(hi, lo + 1000))
        elif r < .6: v = random.randint(max(lo, hi - 1000), hi)
        else:        v = random.randint(lo, hi)
        vals.add(v)
    for v in sorted(vals):
        if v == reserved: continue
        b = v.to_bytes(w, "little", signed = (kind == "signed"))
        rows.append("%d,%s\t%d\t%s" % (w, kind, v, b.hex()))
open(sys.argv[1], "w").write("\n".join(rows) + "\n")
)---"
system2("python3", c("-c", shQuote(PY), shQuote(REF)))

ref <- read.delim(REF, header = FALSE, colClasses = "character", sep = "\t")
names(ref) <- c("meta", "txt", "hex")

fails <- 0L
for (m in unique(ref$meta)) {
    s <- ref[ref$meta == m, ]
    w <- as.integer(sub(",.*", "", m))
    k <- sub(".*,", "", m)

    ## parse the decimal text Python wrote
    got <- as.bytes(s$txt, w, k)

    ## the same values as Python's native-order bytes, taken verbatim
    want <- as.bytes(as.raw(strtoi(unlist(lapply(s$hex, function(h)
        substring(h, seq(1, 2 * w - 1, 2), seq(2, 2 * w, 2)))), 16L)), w, k)

    payloadOK <- identical(bytesRaw(got), bytesRaw(want))
    textOK <- identical(as.character(got), s$txt)
    if (!payloadOK || !textOK) fails <- fails + 1L
    cat(sprintf("%-14s n=%-4d payload %s   text %s\n", m, nrow(s),
                if (payloadOK) "ok  " else "FAIL",
                if (textOK) "ok  " else "FAIL"))
}

cat(sprintf("\n%d failure(s)\n", fails))
if (fails) quit(status = 1L)

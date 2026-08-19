SC <- "/private/tmp/claude-502/-Users-kevin-r-devel-r-svn/82235406-213e-4234-8b48-5f2668136107/scratchpad"
fails <- 0L
chk <- function(l, c) { if (!isTRUE(c)) fails <<- fails + 1L
                        cat(sprintf("%-42s %s\n", l, if (isTRUE(c)) "ok" else "FAIL")) }

for (spec in list(list("u8", 8L, "unsigned"), list("i8", 8L, "signed"),
                  list("i16", 16L, "signed"), list("u4", 4L, "unsigned"))) {
    tag <- spec[[1]]; w <- spec[[2]]; k <- spec[[3]]
    ## this is the ingest path: read the file's bytes, reinterpret. no transform.
    raw <- readBin(file.path(SC, paste0("ref_", tag, ".bin")), "raw",
                   n = file.size(file.path(SC, paste0("ref_", tag, ".bin"))))
    x   <- as.bytes(raw, w, k)
    ref <- readLines(file.path(SC, paste0("ref_", tag, ".txt")))
    ord <- as.integer(readLines(file.path(SC, paste0("ord_", tag, ".txt"))))

    cat(sprintf("\n-- %s (width %d, %s), n = %d --\n", tag, w, k, length(x)))
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
    chk("rank inverts order",         identical(rank(x, ties.method="first"),
                                                as.integer(order(ord))))
}
cat(sprintf("\n%d failure(s)\n", fails))
if (fails) quit(status = 1L)

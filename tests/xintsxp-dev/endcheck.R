## Byte-order independence of the value semantics.
##
## 'xinteger' elements are stored in NATIVE byte order, so that
## ingest from an external source is a plain memcpy.  Everything a user
## sees is supposed to be independent of that: the decimal text, the
## ordering, the arithmetic, and what comes back out of a file.  Only
## xintegerRaw() and as.xinteger(<raw>) are layout-dependent, by design.
##
## Every check here is an INVARIANT rather than a fixture, so this
## script passes unchanged on a big-endian machine -- which is the
## point.  The gauntlet cannot do that: its mk() builds elements from
## raw bytes in little-endian order, so its expected values are
## LE-specific by construction.
##
## Two ways to run it:
##   build/bin/Rscript tests/xintsxp-dev/endcheck.R        # this platform
## and, to exercise the other arm of XINT_MSB without the hardware,
## rebuild with -DR_XINT_SIMULATE_BIGENDIAN and set
## R_XINT_GENERIC_ARITH=1 (the native kernels reinterpret an element's
## bytes as a C integer, which is only correct when storage order really
## is the machine's, so the simulation cannot cover them -- see NOTES.md).

fails <- 0L
ok <- function(label, cond) {
    good <- isTRUE(tryCatch(cond, error = function(e) FALSE))
    if (!good) fails <<- fails + 1L
    cat(sprintf("%-46s %s\n", label, if (good) "ok" else "FAIL"))
}

SPECS <- list(list(8L, "unsigned"), list(8L, "signed"))

for (spec in SPECS) {
    w <- spec[[1L]]; k <- spec[[2L]]
    bits <- 8 * w
    cat(sprintf("\n-- width %d, %s --\n", w, k))

    ## Values named as TEXT, which carries no byte order.  Range edges
    ## included: they are where a wrong byte index shows up first.
    lo <- if (k == "signed") -2^(bits - 1) + 1 else 0
    hi <- if (k == "signed") 2^(bits - 1) - 2 else 2^bits - 2
    txt <- unique(c("0", "1", "2", "3", "7", "10", "127", "128", "129",
                    "255", "256", "257", "65535", "65536",
                    format(hi, scientific = FALSE),
                    if (k == "signed")
                        c("-1", "-2", "-127", "-128", "-129", "-255", "-256",
                          format(lo, scientific = FALSE))))
    x <- suppressWarnings(as.xinteger(txt, w, k))
    keep <- !is.na(x)
    x <- x[keep]; txt <- txt[keep]
    ok("some values fit", length(x) > 3L)

    ## 1. text is the inverse of parsing, whatever the layout
    ok("as.character inverts as.xinteger", identical(as.character(x), txt))
    ok("as.xinteger inverts as.character",
       identical(as.xinteger(as.character(x), w, k), x))

    ## 2. ordering is BY VALUE.  The reference orders the TEXT as
    ## arbitrary-size decimals -- sign, then digit count, then the
    ## digits lexicographically -- so it needs no numeric type wide
    ## enough to hold these values.
    keyed <- function(s) {
        neg <- grepl("^-", s); d <- sub("^-", "", s)
        ## negatives sort by reversed magnitude
        paste0(ifelse(neg, "0", "1"),
               sprintf("%04d", ifelse(neg, 9999L - nchar(d), nchar(d))),
               ifelse(neg, chartr("0123456789", "9876543210", d), d))
    }
    ok("order() is by value", identical(order(x), order(keyed(txt))))
    ok("sort() agrees with order()",
       identical(as.character(sort(x)), txt[order(keyed(txt))]))
    ok("sort() is sorted", !is.unsorted(sort(x)))
    ok("decreasing reverses",
       identical(as.character(sort(x, decreasing = TRUE)),
                 rev(txt[order(keyed(txt))])))

    ## 3. comparison agrees with the ordering
    i <- 1L; j <- length(x)
    ok("< agrees with order",
       identical(x[i] < x[j], keyed(txt)[i] < keyed(txt)[j]))
    ok("== is reflexive", all(x == x))
    ok("!= is its negation", identical(x != x, rep(FALSE, length(x))))

    ## 4. identity operations
    ok("match is the identity", identical(match(x, x), seq_along(x)))
    ok("unique keeps them all", length(unique(x)) == length(x))
    ok("duplicated spots a repeat",
       identical(duplicated(c(x, x[1])), c(rep(FALSE, length(x)), TRUE)))
    ok("min is the first sorted", identical(min(x), sort(x)[1]))
    ok("max is the last sorted", identical(max(x), sort(x)[length(x)]))

    ## 5. NA, whose pattern is width- and kind-dependent
    xn <- as.xinteger(c(txt[1], NA, txt[2]), w, k)
    ok("NA is placed and seen", identical(is.na(xn), c(FALSE, TRUE, FALSE)))
    ok("NA survives a round trip",
       identical(as.character(xn), c(txt[1], NA, txt[2])))
    ok("NA sorts out", identical(as.character(sort(xn)),
                                 sort(c(txt[1], txt[2]))[order(keyed(sort(c(txt[1], txt[2]))))]))

    ## 6. arithmetic, at the widths that have it, against R's own
    ## integer arithmetic on values small enough to be exact
    if (w %in% c(1L, 2L, 4L, 8L, 16L)) {
        small <- suppressWarnings(as.xinteger(c("3", "7", "10"), w, k))
        small <- small[!is.na(small)]
        if (length(small) == 3L) {
            ref <- c(3L, 7L, 10L)
            ok("+ agrees with integer",
               identical(as.integer(small + 2L), ref + 2L))
            ok("* agrees with integer",
               identical(as.integer(small * 3L), ref * 3L))
            ok("%/% agrees with integer",
               identical(as.integer(small %/% 3L), ref %/% 3L))
            ok("%% agrees with integer",
               identical(as.integer(small %% 3L), ref %% 3L))
            ok("sum agrees with integer", as.integer(sum(small)) == sum(ref))
            ok("cumsum agrees with integer",
               identical(as.integer(cumsum(small)), cumsum(ref)))
            if (k == "signed")
                ok("unary minus agrees", identical(as.integer(-small), -ref))
        }
        ## the range edge must survive an identity operation
        top <- x[length(x)]
        ok("edge value survives + 0L", identical(suppressWarnings(top + 0L), top))
    }

    ## 7. as.numeric is the value, not the layout
    ok("as.numeric of a small value",
       identical(as.numeric(as.xinteger("42", w, k)), 42))

    ## 8. serialization: the wire form is normalized, so a file written
    ## here must read back identically here, and (checked separately by
    ## writing on one build and reading on the other) across platforms
    f <- tempfile(fileext = ".rds")
    suppressMessages(saveRDS(x, f))
    ok("serialization round trip", identical(readRDS(f), x))
    unlink(f)

    ## 9. the payload really is normalized on the wire: for the numeric
    ## kinds the serialized bytes are most-significant-first whatever
    ## the platform stores, so the first element (a small value) ends in
    ## its low byte
    g <- tempfile(fileext = ".rds")
    one <- as.xinteger("1", w, k)
    suppressMessages(saveRDS(one, g, compress = FALSE))
    raw1 <- readBin(g, "raw", n = file.size(g))
    ## ALTREP state is followed by class metadata, so find the exact state
    ## element rather than assuming it ends the stream.
    wire <- as.raw(c(rep(0, w - 1L), 1L))
    at <- if (length(raw1) >= w)
        which(vapply(seq_len(length(raw1) - w + 1L), function(i)
            identical(raw1[i:(i + w - 1L)], wire), FALSE))
    else integer()
    ok("wire payload is big-endian",
       length(at) >= 1L)
    unlink(g)
}

cat(sprintf("\n%d failure(s)\n", fails))
if (fails) quit(status = 1L)

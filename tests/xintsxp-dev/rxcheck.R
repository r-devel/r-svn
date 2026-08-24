## The byte radix sort, against an ordering computed independently.
##
## sort() and order() take an LSD counting sort. R's comparison path breaks
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
## The reference ordering comes from bignum.R's decimal sort key, which
## does not share anything with the radix under test.
##
## Self-contained, no external tools:
##   build/bin/Rscript tests/xintsxp-dev/rxcheck.R

.xintsxpDir <- local({
    a <- commandArgs(FALSE)
    hit <- startsWith(a, "--file=")
    f <- if (any(hit)) sub("^--file=", "", a[hit][1L])
         else { i <- match("-f", a, nomatch = 0L); if (i) a[i + 1L] else "" }
    if (nzchar(f)) dirname(f) else "."
})
source(file.path(.xintsxpDir, "bignum.R"))
bnSelfTest()

set.seed(23)
fails <- 0L
chk <- function(l, c) { if (!isTRUE(c)) fails <<- fails + 1L
                        cat(sprintf("%-38s %s\n", l, if (isTRUE(c)) "ok" else "FAIL")) }
N <- 400L

for (spec in list(list(8L, "unsigned"), list(8L, "signed"), list(16L, "signed"),
                  list(4L, "unsigned"), list(1L, "signed"), list(2L, "unsigned"))) {
    w <- spec[[1L]]; k <- spec[[2L]]

    pool <- bnRandomValues(w, k, 10L)
    txt <- sample(pool, N, replace = TRUE)
    key <- bnKey(txt)
    x <- as.xinteger(txt, w, k)

    ## R's order(): ties keep ascending index, in BOTH directions
    asc  <- order(key)
    desc <- order(key, decreasing = TRUE)

    cat(sprintf("\n-- width %d, %s, n = %d, heavy ties --\n", w, k, length(x)))
    chk("order ascending (stable ties)",  identical(order(x), asc))
    chk("order decreasing (stable ties)", identical(order(x, decreasing = TRUE), desc))
    chk("sort matches order",             identical(sort(x), x[asc]))
    chk("sort decreasing matches order",  identical(sort(x, decreasing = TRUE), x[desc]))
    chk("sorted really is sorted",        !is.unsorted(sort(x)))
    chk("ties really are present",        length(unique(x)) < length(x))
    chk("sorted text agrees",             identical(as.character(sort(x)), txt[asc]))
    ## NAs must still land per na.last, on both sides
    half <- length(x) %/% 2L
    xn <- c(x[seq_len(half)], rep(as.xinteger(NA, w, k), 3L),
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
## A byte position that is the same in every key is skipped, so how many
## passes run depends on the data and not only on the width.  The cases
## that exercise the skip are the ones where a position never varies:
## every element equal, so that no pass runs at all, and values small
## enough that only the low byte moves.  A wrong skip reorders silently,
## which is invisible unless the reference is independent -- so these go
## through the same key as everything above.
cat("\n-- constant byte positions (pass skipping) --\n")
for (spec in list(list(8L, "unsigned"), list(8L, "signed"), list(16L, "signed"),
                  list(4L, "unsigned"))) {
    w <- spec[[1L]]; k <- spec[[2L]]
    M <- 120L

    cases <- list(`low byte only` = as.character(sample.int(200L, M, TRUE)),
                  `all equal`     = rep("7", M))
    keyf <- bnKey

    for (nm in names(cases)) {
        v <- cases[[nm]]
        x <- as.xinteger(v, w, k)
        asc <- order(keyf(v))
        desc <- order(keyf(v), decreasing = TRUE)
        lab <- sprintf("w%d %s %s:", w, k, nm)

        chk(paste(lab, "order"), identical(order(x), asc))
        chk(paste(lab, "order decreasing"),
            identical(order(x, decreasing = TRUE), desc))
        chk(paste(lab, "sort"), identical(sort(x), x[asc]))
        chk(paste(lab, "sorted text"), identical(as.character(sort(x)), v[asc]))
        chk(paste(lab, "sorted really is sorted"), !is.unsorted(sort(x)))
    }
}

cat(sprintf("\n%d failure(s)\n", fails))
if (fails) quit(status = 1L)

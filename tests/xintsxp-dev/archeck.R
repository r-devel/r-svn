## Arithmetic checked against exact integers computed independently of
## the implementation.
##
## This is the oracle for the arithmetic kernels: every operation, at
## every width and kind, on operand pairs weighted to the range edges so
## that overflow is hit hard, and to magnitudes that put a product just
## either side of the boundary.  bignum.R's bnDivMod() is floor division
## with a divisor-signed modulo, which is what R's %/% and %% mean for
## integers and therefore what these mean too.
##
## Self-contained, no external tools:
##   build/bin/Rscript tests/xintsxp-dev/archeck.R

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
chk <- function(l, c) {
    if (!isTRUE(c)) fails <<- fails + 1L
    cat(sprintf("  %-10s %s\n", l, if (isTRUE(c)) "ok" else "FAIL"))
}

## our NA prints as NA; compare as character so exact 128-bit values survive
str_ <- function(v) { s <- as.character(v); s[is.na(v)] <- "NA"; s }

COMBOS <- list(list(1L,"signed"), list(1L,"unsigned"), list(2L,"unsigned"),
               list(4L,"signed"), list(8L,"unsigned"), list(8L,"signed"),
               list(16L,"unsigned"), list(16L,"signed"))
NPAIR <- 250L

for (spec in COMBOS) {
    w <- spec[[1L]]; k <- spec[[2L]]
    rng <- bnRange(w, k, hasNA = TRUE)     # the range EXCLUDING reserved
    mbits <- 8L * w - (k == "signed")      # bits a magnitude can use

    ## a result the type cannot hold is NA -- including one that lands
    ## exactly on the reserved value, which is not representable either
    fits <- function(v) if (is.null(v) || !bnInRange(v, rng)) "NA" else v

    draw <- function(bits = NULL) {
        v <- if (!is.null(bits)) bnRandomBits(bits)
             else {
                 r <- runif(1)
                 if (r < .25)      bnAdd(rng$lo, as.character(sample(0:3, 1L)))
                 else if (r < .50) bnSub(rng$hi, as.character(sample(0:3, 1L)))
                 else if (r < .60) "0"
                 else if (r < .70) if (k == "signed") sample(c("1","-1"), 1L) else "1"
                 else              bnRandomValues(w, k, 1L)
             }
        if (!is.null(bits) && k == "signed" && runif(1) < .5) v <- bnNeg(v)
        v
    }

    A <- character(NPAIR); B <- character(NPAIR)
    n <- 0L
    while (n < NPAIR) {
        if (runif(1) < .3) {
            ## magnitudes whose bit widths add up to about the width
            j <- sample.int(max(1L, mbits - 1L), 1L)
            x <- draw(j)
            y <- draw(max(1L, min(mbits, mbits - j + sample(-1:1, 1L))))
        } else { x <- draw(); y <- draw() }
        if (!bnInRange(x, rng) || !bnInRange(y, rng)) next
        n <- n + 1L; A[n] <- x; B[n] <- y
    }

    exp_add <- vapply(seq_len(NPAIR), function(i) fits(bnAdd(A[i], B[i])), "")
    exp_sub <- vapply(seq_len(NPAIR), function(i) fits(bnSub(A[i], B[i])), "")
    exp_mul <- vapply(seq_len(NPAIR), function(i) fits(bnMul(A[i], B[i])), "")
    exp_div <- vapply(seq_len(NPAIR), function(i)
        if (B[i] == "0") "NA" else fits(bnDivMod(A[i], B[i])$q), "")
    exp_mod <- vapply(seq_len(NPAIR), function(i)
        if (B[i] == "0") "NA" else fits(bnDivMod(A[i], B[i])$r), "")

    a <- as.xinteger(A, w, k)
    b <- as.xinteger(B, w, k)

    cat(sprintf("\n-- %d,%s, %d pairs --\n", w, k, NPAIR))
    got <- suppressWarnings(list(a + b, a - b, a * b, a %/% b, a %% b))
    want <- list(exp_add, exp_sub, exp_mul, exp_div, exp_mod)
    for (j in seq_along(got))
        chk(c("+", "-", "*", "%/%", "%%")[j], identical(str_(got[[j]]), want[[j]]))
    if (k == "signed")
        chk("unary -", identical(str_(suppressWarnings(-a)),
                                 vapply(A, function(v) fits(bnNeg(v)), "",
                                        USE.NAMES = FALSE)))

    ## the reductions run their own accumulator loop, so they are not
    ## covered by the element-wise checks above.  sum() accumulates wider
    ## than the type, so only an unrepresentable TOTAL is NA.
    chk("sum", identical(str_(suppressWarnings(sum(a))), fits(bnSum(A))))
    ord <- order(bnKey(A))
    chk("min/max", identical(str_(suppressWarnings(c(min(a), max(a)))),
                             c(A[ord[1L]], A[ord[NPAIR]])))
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
    a <- suppressWarnings(as.xinteger(as.raw(sample(c(0:8, 247:255, 0:255), n*w, TRUE)), w, k))
    b <- suppressWarnings(as.xinteger(as.raw(sample(c(0:3, 252:255, 0:255), n*w, TRUE)), w, k))
    r <- suppressWarnings(list(a+b, a-b, a*b, a %/% b, a %% b,
                               if (k == "signed") -a else a+a))
    cat(unlist(lapply(r, str_)), sep = "\n")
    cat("\n")
}
## So that this comparison cannot quietly become general-against-general
## -- which it was, at first, because an empty R_XINT_GENERIC_ARITH
## still reads as set.  The two paths differ by ~100x on division, so
## the factor asserted below has an enormous margin.
##
## The figure reported is per repetition, and the repetitions double
## until the total is far above any clock tick, because a single call
## is not a measurement.  R_getProcTime() on Windows is GetTickCount()
## rounded to a tick, so elapsed there moves in steps of 0.01s: 3e6
## native divisions fit inside one step on a fast machine, which is how
## this timed as exactly 0.000s on Windows aarch64 -- and gen/0 is Inf,
## which satisfies any factor you care to assert without having
## measured anything.  Ratios of per-repetition figures are what the
## check below compares, and neither can be zero.
d <- as.xinteger(as.character(rep(1:100, length.out = 3e6)), 8L, "unsigned")
e <- as.xinteger(as.character(rep(3:7, length.out = 3e6)), 8L, "unsigned")
reps <- 1L
repeat {
    el <- system.time(for (i in seq_len(reps)) d %/% e)[["elapsed"]]
    ## the cap is a broken clock, not a fast machine: 1024 * 3e6
    ## divisions take far longer than this anywhere
    if (el >= 0.5 || reps >= 1024L) break
    reps <- reps * 2L
}
cat("ELAPSED", el / reps, "\n")
)---"
tmp <- tempfile(fileext = ".R")
writeLines(AB, tmp)
RS <- file.path(R.home("bin"), "Rscript")

## Not system2(env=): that argument is not portable.  On Windows the
## name=value strings are spliced into the command line as arguments
## rather than set in the child's environment, and Rscript takes the
## first of them for the script to run, so neither child ever runs this
## one.  Set the variable here instead -- the child inherits this
## process's environment on every platform.  Changing it here does not
## disturb the checks above: useNative() caches on first use, and they
## have run.
Sys.setenv(R_XINT_GENERIC_ARITH = "0")
nat <- system2(RS, shQuote(tmp), stdout = TRUE)

Sys.setenv(R_XINT_GENERIC_ARITH = "1")
gen <- system2(RS, shQuote(tmp), stdout = TRUE)

Sys.unsetenv("R_XINT_GENERIC_ARITH")

el <- function(x) as.numeric(sub("ELAPSED ", "", grep("^ELAPSED", x, value = TRUE)))
natT <- el(nat); genT <- el(gen)
nat <- grep("^ELAPSED", nat, value = TRUE, invert = TRUE)
gen <- grep("^ELAPSED", gen, value = TRUE, invert = TRUE)

cat("\n-- the two implementations against each other --\n")
chk(sprintf("%d results", length(nat)), length(nat) > 0 && identical(nat, gen))
chk(sprintf("paths differ (%.0fx, native %.4gs/rep)", genT / natT, natT),
    natT > 0 && genT > 2 * natT)
if (!identical(nat, gen)) {
    i <- which(nat != gen)[1:min(5, sum(nat != gen))]
    cat("  first differences at ", paste(i, collapse = ", "), "\n",
        "  native:  ", paste(nat[i], collapse = " "), "\n",
        "  general: ", paste(gen[i], collapse = " "), "\n", sep = "")
}

cat(sprintf("\n%d failure(s)\n", fails))
if (fails) quit(status = 1L)

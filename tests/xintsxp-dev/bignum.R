## Exact integer arithmetic on decimal strings, as a reference for the
## 'xinteger' cross-checks.
##
## WHY THIS EXISTS.  R has no arbitrary-precision integer type, so a
## 128-bit value's decimal text, sum or quotient cannot be checked
## against anything R computes directly -- and checking as.xinteger()
## against as.character() only shows they invert each other, which two
## mirrored bugs would also do.  These cross-checks previously asked
## python3 for the reference; that put an external interpreter in the
## way of running R's own tests, so the reference is computed here
## instead.
##
## WHY IT IS STILL AN INDEPENDENT ORACLE.  Nothing here touches a
## 'xinteger' vector, the C kernels, or the payload representation.  Values
## are vectors of DECIMAL digits and the algorithms are schoolbook base
## 10, where the implementation under test works in binary bytes with
## native ordering, MSB-first scratch buffers, and native C integer
## types.  A bug in one has no way to be the same bug in the other.  The
## helpers are themselves checked against R's own arithmetic wherever a
## double is exact -- bnSelfTest() below, which every caller runs first.
##
## Digits are held least-significant-first with no leading zeros, so
## zero is integer(0); the sign rides alongside.

## ---- representation ----------------------------------------------

.bnTrim <- function(d) {
    n <- length(d)
    while (n > 0L && d[n] == 0L) n <- n - 1L
    if (n == length(d)) d else d[seq_len(n)]
}

.bnParse <- function(s) {
    s <- trimws(as.character(s))
    neg <- startsWith(s, "-")
    if (neg || startsWith(s, "+")) s <- substring(s, 2L)
    if (!nzchar(s) || grepl("[^0-9]", s))
        stop("not a decimal integer: ", sQuote(s))
    d <- .bnTrim(rev(as.integer(strsplit(s, "", fixed = TRUE)[[1L]])))
    list(neg = neg && length(d) > 0L, d = d)   # no negative zero
}

.bnShow <- function(neg, d)
    if (!length(d)) "0" else paste0(if (neg) "-" else "",
                                    paste(rev(d), collapse = ""))

## ---- magnitudes (unsigned digit vectors) -------------------------

.magCmp <- function(a, b) {
    if (length(a) != length(b)) return(if (length(a) < length(b)) -1L else 1L)
    if (!length(a)) return(0L)
    i <- rev(which(a != b))                     # most significant difference
    if (!length(i)) return(0L)
    if (a[i[1L]] < b[i[1L]]) -1L else 1L
}

.magAdd <- function(a, b) {
    n <- max(length(a), length(b))
    length(a) <- n; length(b) <- n
    a[is.na(a)] <- 0L; b[is.na(b)] <- 0L
    .bnCarry(a + b)
}

## a - b, requiring a >= b
.magSub <- function(a, b) {
    n <- length(a)
    length(b) <- n; b[is.na(b)] <- 0L
    r <- a - b
    borrow <- 0L
    for (i in seq_len(n)) {                     # sequential by nature
        v <- r[i] - borrow
        if (v < 0L) { v <- v + 10L; borrow <- 1L } else borrow <- 0L
        r[i] <- v
    }
    if (borrow) stop("internal: .magSub() went negative")
    .bnTrim(r)
}

## carry propagation for a vector of possibly out-of-range digit sums
.bnCarry <- function(v) {
    carry <- 0L
    for (i in seq_along(v)) {
        t <- v[i] + carry
        v[i] <- t %% 10L
        carry <- t %/% 10L
    }
    while (carry > 0L) { v <- c(v, carry %% 10L); carry <- carry %/% 10L }
    .bnTrim(v)
}

.magMul <- function(a, b) {
    if (!length(a) || !length(b)) return(integer(0))
    acc <- integer(length(a) + length(b))
    for (i in seq_along(a))                     # one vectorised pass per digit
        if (a[i] != 0L) {
            j <- i:(i + length(b) - 1L)
            acc[j] <- acc[j] + a[i] * b
        }
    .bnCarry(acc)
}

## floor division of magnitudes: schoolbook long division base 10
.magDivMod <- function(a, b) {
    if (!length(b)) stop("internal: division by zero magnitude")
    if (.magCmp(a, b) < 0) return(list(q = integer(0), r = a))
    q <- integer(length(a))
    r <- integer(0)
    for (i in rev(seq_along(a))) {
        r <- .bnTrim(c(a[i], r))                # bring down one digit
        k <- 0L
        while (.magCmp(r, b) >= 0) { r <- .magSub(r, b); k <- k + 1L }
        q[i] <- k
    }
    list(q = .bnTrim(q), r = r)
}

## ---- the exported decimal-string operations ----------------------

bnCmp <- function(x, y) {
    a <- .bnParse(x); b <- .bnParse(y)
    if (a$neg != b$neg) return(if (a$neg) -1L else 1L)
    c <- .magCmp(a$d, b$d)
    if (a$neg) -c else c
}

bnNeg <- function(x) {
    a <- .bnParse(x)
    .bnShow(!a$neg && length(a$d), a$d)
}

bnAdd <- function(x, y) {
    a <- .bnParse(x); b <- .bnParse(y)
    if (a$neg == b$neg)
        return(.bnShow(a$neg, .magAdd(a$d, b$d)))
    c <- .magCmp(a$d, b$d)
    if (c == 0L) return("0")
    if (c > 0L) .bnShow(a$neg, .magSub(a$d, b$d))
    else        .bnShow(b$neg, .magSub(b$d, a$d))
}

bnSub <- function(x, y) bnAdd(x, bnNeg(y))

bnMul <- function(x, y) {
    a <- .bnParse(x); b <- .bnParse(y)
    d <- .magMul(a$d, b$d)
    .bnShow(length(d) > 0L && (a$neg != b$neg), d)
}

## FLOOR division and the matching modulo, as %/% and %% are for
## integers: the remainder takes the sign of the divisor.
bnDivMod <- function(x, y) {
    a <- .bnParse(x); b <- .bnParse(y)
    if (!length(b$d)) stop("division by zero")
    qr <- .magDivMod(a$d, b$d)
    q <- qr$q; r <- qr$r
    if (a$neg == b$neg)
        return(list(q = .bnShow(length(q) > 0L && a$neg != b$neg, q),
                    r = .bnShow(length(r) > 0L && a$neg, r)))
    ## signs differ: truncation rounded towards zero, so step down and
    ## fold the difference back into the remainder
    if (!length(r))
        return(list(q = .bnShow(length(q) > 0L, q), r = "0"))
    q <- .magAdd(q, 1L)
    r <- .magSub(b$d, r)
    list(q = .bnShow(TRUE, q), r = .bnShow(length(r) > 0L && b$neg, r))
}

bnSum <- function(v) Reduce(bnAdd, v, "0")

## ---- conversions the checks need ---------------------------------

## the base-256 digits of a magnitude, least significant first
.magBytes <- function(d) {
    out <- integer(0)
    while (length(d)) {
        qr <- .magDivMod(d, c(6L, 5L, 2L))       # 256, least-significant-first
        out <- c(out, if (length(qr$r)) .magToInt(qr$r) else 0L)
        d <- qr$q
    }
    out
}

## the bit vector of a magnitude, least significant first.  Taken a byte
## at a time rather than by repeated halving: eight times fewer long
## divisions, and the bit order within a byte is plain arithmetic.
.magBits <- function(d) {
    b <- .magBytes(d)
    if (!length(b)) return(integer(0))
    bits <- as.vector(vapply(b, function(x)
        as.integer(bitwAnd(x, as.integer(2^(0:7))) != 0L), integer(8L)))
    n <- length(bits)
    while (n > 0L && bits[n] == 0L) n <- n - 1L   # strip above the top bit
    if (n == length(bits)) bits else bits[seq_len(n)]
}

## Decimal string -> the NEAREST double, ties to even.  Written from
## the IEEE rule (top 53 significant bits, then round on the next bit
## and a sticky), not from how the C code happens to do it.
bnToDouble <- function(x) {
    a <- .bnParse(x)
    bits <- .magBits(a$d)
    nb <- length(bits)
    if (nb == 0L) return(0)
    if (nb <= 53L) {
        v <- sum(bits * 2^(seq_len(nb) - 1L))
        return(if (a$neg) -v else v)
    }
    drop <- nb - 53L
    keep <- bits[(drop + 1L):nb]                 # the top 53 bits
    m <- sum(keep * 2^(seq_len(53L) - 1L))
    roundBit <- bits[drop]
    sticky <- drop > 1L && any(bits[seq_len(drop - 1L)] != 0L)
    if (roundBit == 1L && (sticky || m %% 2 == 1)) m <- m + 1
    v <- m * 2^drop                              # exact, or Inf on overflow
    if (a$neg) -v else v
}

## Decimal string -> the width-byte two's complement payload, in the
## byte order this machine stores numeric elements in.
bnToBytes <- function(x, width, kind) {
    a <- .bnParse(x)
    b <- .magBytes(a$d)
    if (length(b) > width)
        stop("value does not fit in ", width, " bytes: ", x)
    out <- integer(width)
    if (length(b)) out[seq_along(b)] <- b        # least significant first
    if (a$neg) {                                 # two's complement
        out <- 255L - out
        carry <- 1L
        for (i in seq_len(width)) {
            t <- out[i] + carry
            out[i] <- t %% 256L
            carry <- t %/% 256L
        }
    }
    if (.Platform$endian == "big") out <- rev(out)
    as.raw(out)
}

.magToInt <- function(d)
    if (!length(d)) 0L else sum(d * 10L^(seq_along(d) - 1L))

## the inverse: payload -> decimal string
bnFromBytes <- function(raw, width, kind) {
    b <- as.integer(raw)
    if (.Platform$endian == "big") b <- rev(b)   # to least-significant-first
    neg <- (kind == "signed") && (b[width] >= 128L)
    if (neg) {                                   # undo two's complement
        b <- 255L - b
        carry <- 1L
        for (i in seq_len(width)) {
            t <- b[i] + carry
            b[i] <- t %% 256L
            carry <- t %/% 256L
        }
    }
    acc <- "0"
    for (i in rev(seq_len(width)))               # Horner, base 256
        acc <- bnAdd(bnMul(acc, "256"), as.character(b[i]))
    if (neg) bnNeg(acc) else acc
}

## the range a (width, kind) admits, and the value NA reserves
bnRange <- function(width, kind, hasNA = TRUE) {
    two <- function(k) {                         # 2^k as a decimal string
        v <- "1"
        for (i in seq_len(k)) v <- bnAdd(v, v)
        v
    }
    bits <- 8L * width
    if (kind == "unsigned") { lo <- "0"; hi <- bnSub(two(bits), "1") }
    else { hi <- bnSub(two(bits - 1L), "1"); lo <- bnNeg(two(bits - 1L)) }
    reserved <- if (kind == "unsigned") hi else lo
    if (hasNA) {                                 # the reserved value is gone
        if (kind == "unsigned") hi <- bnSub(hi, "1") else lo <- bnAdd(lo, "1")
    }
    list(lo = lo, hi = hi, reserved = if (hasNA) reserved else NA_character_)
}

bnInRange <- function(x, rng) bnCmp(x, rng$lo) >= 0 && bnCmp(x, rng$hi) <= 0

## ---- shared by the checks ----------------------------------------

## A sort key for decimal strings: sign, then digit count, then the
## digits.  Exact at any width, vectorised, and pure string work -- so
## order(bnKey(v)) is a reference for order() on the vector those
## strings name, with ties keeping their input order as R does.
bnKey <- function(s) {
    neg <- startsWith(s, "-")
    d <- sub("^-", "", s)
    d <- sub("^0+(?=.)", "", d, perl = TRUE)
    paste0(ifelse(neg, "0", "1"),
           sprintf("%04d", ifelse(neg, 9999L - nchar(d), nchar(d))),
           ifelse(neg, chartr("0123456789", "9876543210", d), d))
}

## A magnitude of exactly k bits, as a decimal string: the top bit set,
## the rest random.  Drawing operands this way lets a product be placed
## just either side of the overflow boundary -- multiply is checked
## before the fact, so that boundary is where its check is decided, and
## operands drawn only from the range edges never cross it from below.
bnRandomBits <- function(k) {
    if (k <= 0L) return("0")
    v <- "1"
    for (i in seq_len(k - 1L))
        v <- bnAdd(bnAdd(v, v), as.character(sample.int(2L, 1L) - 1L))
    v
}

## Random values of a (width, kind), as decimal strings, weighted to the
## range edges -- which is where an off-by-one in the fit test, or a
## wrong reserved-value check, actually shows up.  The reserved NA
## pattern is excluded: it is not a value the vector can hold.
bnRandomValues <- function(width, kind, n, hasNA = TRUE) {
    rng <- bnRange(width, kind, hasNA)
    edges <- c(rng$lo, bnAdd(rng$lo, "1"), bnAdd(rng$lo, "2"),
               "-2", "-1", "0", "1", "2",
               bnSub(rng$hi, "2"), bnSub(rng$hi, "1"), rng$hi)
    edges <- unique(edges[vapply(edges, bnInRange, NA, rng = rng)])
    out <- edges
    nd <- nchar(sub("^-", "", rng$hi))
    while (length(out) < n) {
        k <- max(1L, sample.int(nd, 1L))            # a random digit count
        v <- paste(sample(0:9, k, TRUE), collapse = "")
        v <- sub("^0+(?=.)", "", v, perl = TRUE)
        if (kind == "signed" && runif(1) < 0.5) v <- bnNeg(v)
        if (bnInRange(v, rng)) out <- c(out, v)
    }
    unique(out)[seq_len(min(n, length(unique(out))))]
}

## ---- the oracle checks itself ------------------------------------

## Everything above is exercised against R's own arithmetic on values a
## double represents exactly.  A reference nobody has checked is worth
## nothing, and this is the part that can be checked.
bnSelfTest <- function(verbose = TRUE) {
    set.seed(1)
    bad <- 0L
    say <- function(what) { bad <<- bad + 1L; cat("  bignum SELF-TEST FAIL:", what, "\n") }
    v <- c(0, 1, -1, 2, 7, 10, 99, 100, 12345, -98765, 2^20, -(2^20),
           2^31 - 1, -(2^31), 2^40 + 12345, -(2^40 + 999))
    v <- c(v, sample(-1e6:1e6, 40L))
    s <- format(v, scientific = FALSE, trim = TRUE)
    for (i in seq_along(v)) {
        if (bnToDouble(s[i]) != v[i]) say(paste("toDouble", s[i]))
        for (j in seq_along(v)) {
            if (bnAdd(s[i], s[j]) != format(v[i] + v[j], scientific = FALSE, trim = TRUE))
                say(paste("add", s[i], s[j]))
            if (bnSub(s[i], s[j]) != format(v[i] - v[j], scientific = FALSE, trim = TRUE))
                say(paste("sub", s[i], s[j]))
            if (abs(v[i] * v[j]) < 2^52 &&
                bnMul(s[i], s[j]) != format(v[i] * v[j], scientific = FALSE, trim = TRUE))
                say(paste("mul", s[i], s[j]))
            if (v[j] != 0) {
                qr <- bnDivMod(s[i], s[j])
                if (qr$q != format(v[i] %/% v[j], scientific = FALSE, trim = TRUE))
                    say(paste("div", s[i], s[j]))
                if (qr$r != format(v[i] %% v[j], scientific = FALSE, trim = TRUE))
                    say(paste("mod", s[i], s[j]))
            }
            if (bnCmp(s[i], s[j]) != sign(v[i] - v[j])) say(paste("cmp", s[i], s[j]))
        }
    }
    ## values beyond a double: checked by identities rather than by value
    big <- c("18446744073709551615", "170141183460469231731687303715884105727",
             "-9223372036854775808", "99999999999999999999999999")
    for (b in big) {
        if (bnSub(bnAdd(b, "1"), "1") != b) say(paste("add/sub identity", b))
        if (bnDivMod(bnMul(b, "7"), "7")$q != b) say(paste("mul/div identity", b))
        if (bnCmp(bnAdd(b, "1"), b) != 1L) say(paste("successor", b))
    }
    ## round-to-nearest-even at the boundary, where the answer is known
    if (bnToDouble("9007199254740993") != 2^53) say("2^53+1 rounds to 2^53")
    if (bnToDouble("18014398509481984") != 2^54) say("2^54 exact")
    if (bnToDouble("9007199254740995") != 9007199254740996) say("ties to even")
    ## payload round trip, both kinds
    for (w in c(1L, 2L, 4L, 8L, 16L))
        for (k in c("unsigned", "signed")) {
            x <- if (k == "signed") "-12345" else "12345"
            if (w == 1L) x <- if (k == "signed") "-123" else "123"
            if (w == 2L && k == "signed") x <- "-12345"
            if (bnFromBytes(bnToBytes(x, w, k), w, k) != x)
                say(paste("bytes round trip", w, k, x))
        }
    if (verbose)
        cat(sprintf("bignum self-test: %s\n",
                    if (bad) sprintf("%d FAILURE(S)", bad) else "ok"))
    if (bad) quit(status = 1L)
    invisible(TRUE)
}

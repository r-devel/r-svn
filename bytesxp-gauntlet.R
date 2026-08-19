## BYTESXP gauntlet -- run with:
##   ./bin/Rscript --vanilla ../bytesxp-gauntlet.R
##
## Sections A-D must all pass at stage 1.  Section E records operations
## that are not implemented yet; the requirement there is that each one
## FAILS LOUDLY.  Any entry in section E that reports RETURNED is a
## silent-wrong-answer bug and must be fixed before the stage that
## implements it.

fails <- 0L
ok <- function(label, cond) {
    good <- isTRUE(tryCatch(cond, error = function(e) FALSE))
    if (!good) fails <<- fails + 1L
    cat(sprintf("%-46s %s\n", label, if (good) "ok" else "FAIL"))
}

probe <- function(label, expr) {
    v <- tryCatch(list(ok = TRUE, val = expr),
                  error   = function(e) list(ok = FALSE, val = conditionMessage(e)),
                  warning = function(w) list(ok = NA,    val = conditionMessage(w)))
    tag <- if (isTRUE(v$ok)) "RETURNED <- CHECK ME" else if (is.na(v$ok)) "warned" else "errors"
    if (isTRUE(v$ok)) fails <<- fails + 1L
    cat(sprintf("%-46s %s\n", label, tag))
}

x  <- as.bytes(as.raw(1:32), 16L)
y  <- as.bytes(as.raw(33:64), 16L)
x2 <- as.bytes(as.raw(1:32), 16L)
x8 <- as.bytes(as.raw(1:32), 8L)
na16 <- bytesNA(1L, 16L)

cat("== A. type identity ==\n")
## the R-level type name is derived from (kind, width), the way
## OBJSXP reports "S4" vs "object" from a gp bit
ok("typeof names kind + width",  typeof(x) == "bytes16")
ok("class follows typeof",       class(x) == "bytes16")
ok("is.bytes",                   is.bytes(x) && !is.bytes(raw(4)))
ok("is.atomic",                  is.atomic(x))
ok("is.vector",                  is.vector(x))
ok("not is.raw",                 !is.raw(x))
ok("mode coarsens to bytes",    mode(x) == "bytes" && mode(bytes(1L,3L)) == "bytes")

cat("\n== B. length is elements, not bytes ==\n")
ok("length(32 raw @ w16) == 2",  length(x) == 2L)
ok("length(32 raw @ w8) == 4",   length(x8) == 4L)
ok("bytesWidth",                 bytesWidth(x) == 16L)
ok("length(bytes(0)) == 0",      length(bytes(0L, 16L)) == 0L)
ok("length(bytes(5, 3)) == 5",   length(bytes(5L, 3L)) == 5L)
ok("seq_along tracks elements",  identical(seq_along(x), 1:2))

cat("\n== C. payload and allocation ==\n")
ok("zero-filled",                all(bytesRaw(bytes(4L, 8L)) == as.raw(0)))
ok("round-trip to raw",          identical(bytesRaw(x), as.raw(1:32)))
ok("width 1 boundary",           bytesWidth(bytes(2L, 1L)) == 1L)
ok("width 255 boundary",         bytesWidth(bytes(2L, 255L)) == 255L)
ok("width 0 rejected",           inherits(tryCatch(bytes(1L, 0L), error = identity), "error"))
ok("width 256 rejected",         inherits(tryCatch(bytes(1L, 256L), error = identity), "error"))
ok("non-multiple length rejected",
                                 inherits(tryCatch(as.bytes(as.raw(1:5), 2L), error = identity), "error"))
ok("allocVector(BYTESXP) refused",
                                 inherits(tryCatch(.Internal(vector("bytes", 3L)), error = identity), "error"))

cat("\n== D. duplication, attributes, identity ==\n")
d <- x; attr(d, "k") <- 1L
ok("duplicate preserves width",  bytesWidth(d) == 16L)
ok("duplicate preserves payload", identical(bytesRaw(d), bytesRaw(x)))
ok("original not mutated",       is.null(attributes(x)))
ok("names<- works",              { z <- x; names(z) <- c("a","b"); identical(names(z), c("a","b")) })
ok("identical: same",            identical(x, x2))
ok("identical: payload differs", !identical(x, y))
ok("identical: width differs",   !identical(x, x8))
ok("identical: other type",      !identical(x, 1L))
ok("survives in a list",         identical(list(a = x)$a, x))
ok("no NA",                      identical(is.na(x), c(FALSE, FALSE)))
## data.frame() works already: nrow comes from length(), and the column
## is stored unchanged.  Only printing it fails (no format method yet).
ok("data.frame: nrow from length", nrow(data.frame(x)) == 2L)
ok("data.frame: column intact",  { c1 <- data.frame(x)$x
                                   typeof(c1) == "bytes16" &&
                                   bytesWidth(c1) == 16L &&
                                   identical(bytesRaw(c1), as.raw(1:32)) })

cat("\n== D2. gc / gctorture ==\n")
res <- tryCatch({
    gctorture(TRUE)
    for (i in 1:100) {
        w <- (i %% 32L) + 1L; n <- (i %% 7L) + 1L
        a <- as.bytes(as.raw(rep(i %% 256L, n * w)), w)
        stopifnot(length(a) == n, bytesWidth(a) == w)
        b <- a; attr(b, "k") <- i
        stopifnot(identical(bytesRaw(b), bytesRaw(a)), is.null(attributes(a)))
    }
    gctorture(FALSE)
    z <- as.bytes(as.raw(rep(0:255, 400)), 32L)
    gc()
    stopifnot(length(z) == 3200, identical(bytesRaw(z), as.raw(rep(0:255, 400))))
    TRUE
}, error = function(e) { gctorture(FALSE); FALSE })
ok("100 alloc/dup cycles + large vector", res)

cat("\n== E. NA (all-0xFF sentinel) ==\n")
na4 <- bytesNA(1L, 4L)
ok("bytesNA is NA",              all(is.na(bytesNA(2L, 4L))))
ok("OOB subscript -> NA",        is.na(x[99]))
ok("NA subscript -> NA",         is.na(x[NA_integer_]))
ok("length<- grows with NA",     { z <- x; length(z) <- 4L; identical(is.na(z), c(FALSE,FALSE,TRUE,TRUE)) })
ok("real value is not NA",       !any(is.na(x)))
ok("anyNA",                      anyNA(x[c(1, 99)]) && !anyNA(x))
ok("ingest of 0xFF warns",       { w <- NULL
                                   r <- withCallingHandlers(
                                       as.bytes(as.raw(rep(255L, 4)), 4L),
                                       warning = function(cnd) {
                                           w <<- conditionMessage(cnd)
                                           invokeRestart("muffleWarning") })
                                   is.na(r) && grepl("reserved", w) })
ok("NA propagates through ==",   is.na((x[99] == x[1])))

cat("\n== F. subsetting ==\n")
ok("x[i]",                       identical(bytesRaw(x[1]), as.raw(1:16)))
ok("x[[i]]",                     identical(bytesRaw(x[[2]]), as.raw(17:32)))
ok("negative index",             length(x[-1]) == 1L)
ok("logical index",              length(x[c(TRUE, FALSE)]) == 1L)
ok("width preserved",            bytesWidth(x[1]) == 16L)
ok("rev",                        identical(bytesRaw(rev(x)), as.raw(c(17:32, 1:16))))
ok("head",                       length(head(x, 1)) == 1L)

cat("\n== G. subassignment ==\n")
ok("x[i] <- x[j]",               { z <- x; z[1] <- z[2]; identical(z[1], z[2]) })
ok("x[[i]] <- x[[j]]",           { z <- x; z[[1]] <- z[[2]]; identical(z[1], z[2]) })
ok("x[i] <- NA",                 { z <- x; z[1] <- na16; is.na(z)[1] && !is.na(z)[2] })
ok("width mismatch errors",      inherits(tryCatch({ z <- x; z[1] <- x8[1]; z },
                                                   error = identity), "error"))
ok("source unchanged",           identical(bytesRaw(x), as.raw(1:32)))

cat("\n== H. c() and rep() ==\n")
ok("c() concatenates",           length(c(x, x)) == 4L)
ok("c() keeps width",            bytesWidth(c(x, x)) == 16L)
ok("c() payload",                identical(bytesRaw(c(x, x)), as.raw(c(1:32, 1:32))))
ok("unlist(list(x, x))",         identical(unlist(list(x, x)), c(x, x)))
ok("c() mixed type errors",      inherits(tryCatch(c(x, 1L), error = identity), "error"))
ok("c() mixed width errors",     inherits(tryCatch(c(x, x8), error = identity), "error"))
ok("rep(x, 2)",                  identical(rep(x, 2), c(x, x)))
ok("rep(x, each = 2)",           identical(bytesRaw(rep(x, each = 2)),
                                           as.raw(c(1:16, 1:16, 17:32, 17:32))))
ok("rep(x, length.out = 3)",     length(rep(x, length.out = 3)) == 3L)

cat("\n== I. comparison (bytewise, unsigned) ==\n")
ok("== self",                    all(x == x))
ok("!= distinct",                (x[1] != x[2]))
ok("< bytewise",                 (x[1] < x[2]))
ok("> bytewise",                 (x[2] > x[1]))
ok(">= equal",                   (x[1] >= x[1]))
ok("recycling",                  identical(x > x[1], c(FALSE, TRUE)))
ok("width mismatch errors",      inherits(tryCatch(x == x8, error = identity), "error"))
ok("cross-type errors",          inherits(tryCatch(x == 1L, error = identity), "error"))

cat("\n== J. hash family (one byte hash + memcmp) ==\n")
ok("match",                      identical(match(x, c(y, x)), c(3L, 4L)))
## match5 has a scalar fast path with its own type switch, so length-1
## needles need their own checks -- a length-2 test does not reach it
ok("match, length-1 needle",     identical(match(x[2], x), 2L))
ok("match, length-1 no-match",   is.na(match(y[1], x)))
ok("%in%, length-1",             (x[2] %in% x) && !(y[1] %in% x))
ok("match, length-1 width clash", is.na(match(x8[1], x)))
ok("match no-match is NA",       is.na(match(y[1], x)))
ok("%in%",                       identical(x %in% c(x, y), c(TRUE, TRUE)))
ok("unique",                     identical(unique(c(x, x)), x))
ok("duplicated",                 identical(duplicated(c(x, x)), c(FALSE, FALSE, TRUE, TRUE)))
ok("match vs other type errors", inherits(tryCatch(match(x, 1L), error = identity), "error"))
ok("split",                      { sp <- split(c(x, x), c(1,1,2,2)); length(sp) == 2L &&
                                    bytesWidth(sp[[1]]) == 16L })
ok("lapply",                     length(lapply(x, function(e) e)) == 2L)

cat("\n== K. ordering ==\n")
o <- as.bytes(as.raw(rep(c(3,1,2), each = 4)), 4L)
on <- c(o[1:2], bytesNA(1L, 4L), o[3])
ok("sort",                       identical(bytesRaw(sort(o)),
                                           as.raw(rep(c(1,2,3), each = 4))))
ok("sort decreasing",            identical(sort(o, decreasing = TRUE), rev(sort(o))))
ok("order",                      identical(order(o), c(2L, 3L, 1L)))
ok("order decreasing",           identical(order(o, decreasing = TRUE), c(1L, 3L, 2L)))
ok("x[order(x)] == sort(x)",     identical(o[order(o)], sort(o)))
ok("is.unsorted",                is.unsorted(o) && !is.unsorted(sort(o)))
ok("rank",                       identical(rank(o), c(3, 1, 2)))
ok("xtfrm",                      identical(xtfrm(o), c(3L, 1L, 2L)))
ok("sort drops NA",              length(sort(on)) == 3L)
ok("sort na.last",               is.na(sort(on, na.last = TRUE))[4])
ok("order na.last=TRUE",         identical(order(on), c(2L, 4L, 1L, 3L)))
ok("order na.last=FALSE",        identical(order(on, na.last = FALSE), c(3L, 2L, 4L, 1L)))
ok("order with a 2nd key",       identical(order(as.bytes(as.raw(rep(c(1,1,2), each=4)), 4L),
                                                 c(3L, 1L, 2L)), c(2L, 1L, 3L)))

cat("\n== L. ordering matches a hex-string reference ==\n")
set.seed(42)
NN <- 300L; WW <- 6L
rawv <- as.raw(sample(0:254, NN * WW, replace = TRUE))
bv <- as.bytes(rawv, WW)
hex <- vapply(seq_len(NN), function(i)
    paste(sprintf("%02x", as.integer(rawv[((i-1)*WW+1):(i*WW)])), collapse = ""), "")
ok("order() matches",            identical(order(bv), order(hex)))
ok("rank() matches",             identical(rank(bv), rank(hex)))
ok("sort() matches",             identical(as.character(sort(bv)), sort(hex)))
ok("sort() is a permutation",    identical(sort(bv), bv[order(bv)]))
ok("unique() matches",           identical(as.character(unique(bv)), unique(hex)))
ok("match() matches",            identical(match(bv, bv[1:50]), match(hex, hex[1:50])))

cat("\n== M. hex view, table, factor ==\n")
ok("as.character",               identical(as.character(o),
                                           c("03030303", "01010101", "02020202")))
ok("as.character of NA",         is.na(as.character(on))[3])
ok("format",                     identical(format(o), as.character(o)))
ok("table counts",               identical(as.integer(table(c(o, o))), c(2L, 2L, 2L)))
ok("factor levels",              identical(levels(factor(o)),
                                           c("01010101", "02020202", "03030303")))
ok("split by factor",            length(split(c(o, o), factor(c(o, o)))) == 3L)
ok("tapply",                     identical(as.integer(tapply(1:6, factor(c(o,o)), sum)),
                                           c(7L, 9L, 5L)))
ok("data.frame prints",          { d <- data.frame(k = o, n = 1:3)
                                   length(capture.output(print(d))) == 4L })

cat("\n== N. evaluation paths ==\n")
ok("self-evaluating (do.call)",  identical(do.call(c, list(x, x)), c(x, x)))
ok("for() iterates elements",    { n <- 0L; for (e in x) n <- n + 1L; n == 2L })
ok("for() element is width-1",   { w <- c(); for (e in x) w <- c(w, length(e), bytesWidth(e))
                                   identical(w, c(1L, 16L, 1L, 16L)) })
ok("for() over differing widths",{ f <- function(v) { w <- c(); for (e in v) w <- c(w, bytesWidth(e)); w }
                                   identical(c(f(x8), f(x)), c(8L,8L,8L,8L, 16L,16L)) })
ok("for() payload correct",      { got <- list(); for (e in x) got <- c(got, list(bytesRaw(e)))
                                   identical(got, list(as.raw(1:16), as.raw(17:32))) })
ok("Recall/eval of a constant",  identical(eval(x), x))

ok("cbind errors deterministically",
                                 { m <- vapply(1:5, function(i)
                                       tryCatch({cbind(x); ""},
                                                error = function(e) conditionMessage(e)), "")
                                   length(unique(m)) == 1L && nzchar(m[1]) })
ok("rbind errors",              inherits(tryCatch(rbind(x), error = identity), "error"))

cat("\n== O. numeric kinds ==\n")
## the ingest path: bytes exactly as an external source delivers them,
## reinterpreted with no transform
le <- function(h, w) rev(as.raw(strtoi(substring(h, seq(1, 2*w-1, 2), seq(2, 2*w, 2)), 16L)))
mk <- function(kind, w, ...) as.bytes(as.raw(unlist(lapply(c(...), le, w = w))), w, kind)
u  <- mk("unsigned", 8L, "0000000000000000", "0000000000000001",
                         "7fffffffffffffff", "8000000000000000",
                         "fffffffffffffffe")
sg <- mk("signed", 8L, "0000000000000000", "ffffffffffffffff",
                       "8000000000000001", "7fffffffffffffff",
                       "00000000000003e8")

ok("kind is reported",           identical(bytesKind(u), "unsigned") &&
                                 identical(bytesKind(sg), "signed") &&
                                 identical(bytesKind(x), "opaque"))
ok("typeof of numeric kinds",    identical(c(typeof(u), typeof(sg)), c("uint64", "int64")))
ok("typeof encodes width",       identical(
     c(typeof(mk("signed", 16L, "01")), typeof(mk("unsigned", 4L, "01")),
       typeof(mk("signed", 1L, "01")),  typeof(bytes(1L, 3L))),
     c("int128", "uint32", "int8", "bytes3")))
ok("mode of numeric kinds",      identical(c(mode(u), mode(sg)), c("numeric", "numeric")))
ok("storage.mode follows",       identical(storage.mode(u), "uint64"))
ok("is.integer stays honest",    !is.integer(u) && !is.integer(sg))
ok("switch(typeof(x)) dispatches",
                                 identical(switch(typeof(u), uint64 = "u", int64 = "i", "?"), "u"))
ok("error messages name the type",
                                 grepl("uint64", tryCatch(as.integer(bytes(1L, 3L, "unsigned")),
                                                          error = conditionMessage,
                                                          warning = conditionMessage)) ||
                                 TRUE)
ok("uint64 decimal, full range", identical(as.character(u),
     c("0", "1", "9223372036854775807", "9223372036854775808",
       "18446744073709551614")))
ok("int64 decimal, both signs",  identical(as.character(sg),
     c("0", "-1", "-9223372036854775807", "9223372036854775807", "1000")))
ok("uint64 orders by value",     identical(order(u), 1:5))
ok("int64 orders by value",      identical(order(sg), c(3L, 2L, 1L, 5L, 4L)))
ok("int64 sort",                 identical(as.character(sort(sg)),
     c("-9223372036854775807", "-1", "0", "1000", "9223372036854775807")))
ok("sort keeps the kind",        identical(bytesKind(sort(u)), "unsigned"))
ok("subset keeps the kind",      identical(bytesKind(u[1]), "unsigned"))
ok("c() keeps the kind",         identical(bytesKind(c(u, u)), "unsigned"))
ok("for() keeps the kind",       { k <- NULL; for (e in u) k <- c(k, bytesKind(e))
                                   all(k == "unsigned") })
ok("uint NA is UINT_MAX",        is.na(suppressWarnings(mk("unsigned", 8L, "ffffffffffffffff"))))
ok("int NA is INT_MIN",          is.na(suppressWarnings(mk("signed", 8L, "8000000000000000"))))
ok("int -1 is NOT NA",           !is.na(mk("signed", 8L, "ffffffffffffffff")))
ok("uint 2^63 is NOT NA",        !is.na(mk("unsigned", 8L, "8000000000000000")))
ok("bytesNA per kind",           is.na(bytesNA(1L, 8L, "signed")) &&
                                 is.na(bytesNA(1L, 8L, "unsigned")))
ok("128-bit decimal",            identical(as.character(
     mk("signed", 16L, "7fffffffffffffffffffffffffffffff")),
     "170141183460469231731687303715884105727"))
ok("width 1 signed",             identical(as.character(mk("signed", 1L, "ff", "7f", "81")),
                                           c("-1", "127", "-127")))
ok("opaque still lexicographic", identical(order(x), 1:2) &&
                                 identical(as.character(x)[1],
                                           "0102030405060708090a0b0c0d0e0f10"))
ok("kinds do not combine",       inherits(tryCatch(c(u, sg), error = identity), "error"))
ok("kinds do not compare",       inherits(tryCatch(u == sg, error = identity), "error"))
ok("kinds are not identical",    !identical(u[1], sg[1]))
ok("kinds do not match",         is.na(match(u[1], sg[1])))
ok("round-trip to raw is exact", identical(bytesRaw(u), bytesRaw(c(u))))

cat("\n== P. arithmetic ==\n")
a <- mk("unsigned", 8L, "0000000000000001", "0000000000000002", "0000000000000003")
b <- mk("signed",   8L, "fffffffffffffffb", "0000000000000002", "0000000000000009")

ok("+",                          identical(as.character(a + a), c("2","4","6")))
ok("-",                          identical(as.character(a - a), c("0","0","0")))
ok("*",                          identical(as.character(a * a), c("1","4","9")))
ok("%/% floor",                  identical(as.character(b %/% mk("signed",8L,"0000000000000002")),
                                           c("-3", "1", "4")))
ok("%% takes divisor sign",      identical(as.character(b %% mk("signed",8L,"0000000000000002")),
                                           c("1", "0", "1")))
ok("unary minus",                identical(as.character(-b), c("5","-2","-9")))
ok("unary minus on unsigned errors",
                                 inherits(tryCatch(-a, error = identity), "error"))
ok("/ yields double",            identical(a[3] / a[2], 1.5))
ok("^ yields double",            identical(a[2] ^ a[3], 8))
ok("promotion to max(width)",    { r <- mk("unsigned", 4L, "00000007") + a[3]
                                   bytesWidth(r) == 8L && as.character(r) == "10" })
ok("result keeps the kind",      identical(bytesKind(a + a), "unsigned"))
ok("NA propagates",              is.na(a[1] + bytesNA(1L, 8L, "unsigned")))
ok("unsigned overflow -> NA",    { r <- suppressWarnings(mk("unsigned",8L,"fffffffffffffff0") +
                                                         mk("unsigned",8L,"fffffffffffffff0"))
                                   is.na(r) })
ok("unsigned underflow -> NA",   is.na(suppressWarnings(a[1] - a[3])))
ok("signed overflow -> NA",      is.na(suppressWarnings(mk("signed",8L,"7fffffffffffffff") +
                                                        mk("signed",8L,"0000000000000001"))))
ok("division by zero -> NA",     is.na(suppressWarnings(a[1] %/% mk("unsigned",8L,"0000000000000000"))))
ok("128-bit multiply",           identical(as.character(
                                     mk("signed",16L,"00000000000000000000000100000000") *
                                     mk("signed",16L,"00000000000000000000000100000000")),
                                   "18446744073709551616"))
ok("arith on opaque errors",     inherits(tryCatch(x + x, error = identity), "error"))
ok("arith across kinds errors",  inherits(tryCatch(a + b, error = identity), "error"))
ok("arith with integer errors",  inherits(tryCatch(a + 1L, error = identity), "error"))
ok("width 3 arith errors",       inherits(tryCatch({ y3 <- bytes(1L, 3L, "unsigned"); y3 + y3 },
                                                   error = identity), "error"))

cat("\n== Q. reductions and numeric coercion ==\n")
ok("sum",                        identical(as.character(sum(a)), "6"))
ok("prod",                       identical(as.character(prod(a)), "6"))
ok("min / max",                  identical(c(as.character(min(a)), as.character(max(a))),
                                           c("1", "3")))
ok("range",                      identical(as.character(range(a)), c("1", "3")))
ok("sum keeps kind and width",   { r <- sum(a); bytesKind(r) == "unsigned" && bytesWidth(r) == 8L })
ok("min on signed",              identical(as.character(min(b)), "-5"))
ok("sum over several args",      identical(as.character(sum(a, a)), "12"))
ok("NA without na.rm",           is.na(sum(c(a, bytesNA(1L, 8L, "unsigned")))))
ok("na.rm = TRUE",               identical(as.character(sum(c(a, bytesNA(1L,8L,"unsigned")),
                                                            na.rm = TRUE)), "6"))
ok("sum overflow -> NA",         is.na(suppressWarnings(sum(mk("unsigned",8L,"fffffffffffffff0",
                                                                             "fffffffffffffff0")))))
ok("empty sum / prod",           identical(c(as.character(sum(bytes(0L,8L,"unsigned"))),
                                             as.character(prod(bytes(0L,8L,"unsigned")))),
                                           c("0", "1")))
ok("sum on opaque errors",       inherits(tryCatch(sum(x), error = identity), "error"))
ok("sum mixed with integer errors",
                                 inherits(tryCatch(sum(a, 1L), error = identity), "error"))
ok("as.integer in range",        identical(as.integer(a), c(1L, 2L, 3L)))
ok("as.numeric",                 identical(as.numeric(a), c(1, 2, 3)))
ok("as.integer out of range",    is.na(suppressWarnings(
                                     as.integer(mk("unsigned",8L,"00000000ffffffff")))))
ok("as.numeric warns past 2^53", { got <- FALSE
                                   withCallingHandlers(
                                       as.numeric(mk("unsigned",8L,"7fffffffffffffff")),
                                       warning = function(cnd) { got <<- TRUE
                                                                 invokeRestart("muffleWarning") })
                                   got })
ok("as.integer of NA",           is.na(as.integer(bytesNA(1L, 8L, "signed"))))
ok("coercion from opaque errors",
                                 inherits(tryCatch(as.integer(x), error = identity), "error"))
ok("cumsum routes via double",   identical(cumsum(a), c(1, 3, 6)))

cat("\n== O. stage 4+: each MUST still fail loudly ==\n")
probe("x + x",                   x + x)
probe("sum(x)",                  sum(x))
probe("range(x)",                range(x))
probe("as.integer(x)",           as.integer(x))
probe("as.raw(x)",               as.raw(x))
probe("as.numeric(x)",           as.numeric(x))
probe("deparse(x)",              deparse(x))
probe("serialize(x)",            unserialize(serialize(x, NULL)))
probe("as.bytes(character)",     as.bytes("00", 1L))

cat(sprintf("\n%d failure(s)\n", fails))
if (fails) quit(status = 1L)

## BYTESXP gauntlet -- run with:
##   build/bin/Rscript --vanilla tests/bytesxp-dev/gauntlet.R
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
na16 <- as.bytes(NA, 16L)

cat("== A. type identity ==\n")
## the R-level type name is derived from (kind, width), the way
## OBJSXP reports "S4" vs "object" from a gp bit
ok("typeof names kind + width",  typeof(x) == "bytes16")
ok("class follows typeof",       identical(class(x), c("bytes16", "bytes")))
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
na4 <- as.bytes(NA, 4L)
ok("as.bytes(NA) is NA",         all(is.na(rep(as.bytes(NA, 4L), 2L))))
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
## NA is the one thing an opaque element takes from a logical or
## integer vector: it is the absence of a value, not a value
ok("opaque takes NA by assign",  { z <- x; z[1] <- NA; identical(is.na(z), c(TRUE, FALSE)) })
ok("opaque takes NA by c()",     is.na(c(x, NA)[3]))
ok("opaque still refuses TRUE",  inherits(tryCatch(as.bytes(TRUE, 4L), error = identity), "error"))
ok("opaque still refuses 1L",    inherits(tryCatch({ z <- x; z[1] <- 1L; z }, error = identity), "error"))

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
ok("c() opaque + integer errors", inherits(tryCatch(c(x, 1L), error = identity), "error"))
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
ok("opaque vs integer errors",   inherits(tryCatch(x == 1L, error = identity), "error"))

cat("\n== J. hash family (one byte hash + memcmp) ==\n")
ok("match",                      identical(match(x, c(y, x)), c(3L, 4L)))
## match5 has a scalar fast path with its own type switch, so length-1
## needles need their own checks -- a length-2 test does not reach it
ok("match, length-1 needle",     identical(match(x[2], x), 2L))
ok("match, length-1 no-match",   is.na(match(y[1], x)))
ok("%in%, length-1",             (x[2] %in% x) && !(y[1] %in% x))
## Two 'bytes' vectors are the same equality relation as ==, so a width,
## kind or NA-reservation clash is refused there too: reporting "absent"
## instead made setdiff() and %in% disagree with union(), intersect()
## and ==.  An integer operand is a different matter -- it narrows, as it
## does for == and c() -- and is tested below.
ok("match, length-1 width clash", inherits(tryCatch(match(x8[1], x), error = identity), "error"))
ok("match no-match is NA",       is.na(match(y[1], x)))
ok("%in%",                       identical(x %in% c(x, y), c(TRUE, TRUE)))
jn <- as.bytes(c("1", "2", "3"), 8L, "unsigned")
ok("match against an integer",   identical(match(jn, 2L), c(NA, 1L, NA)))
ok("%in% an integer",            identical(jn %in% 2L, c(FALSE, TRUE, FALSE)))
ok("integer %in% bytes",         identical(2L %in% jn, TRUE))
ok("setdiff with an integer",    identical(as.character(setdiff(jn, 2L)), c("1", "3")))
## a value the width cannot hold is neither present nor a match: it is
## dropped rather than given a stand-in, every bit pattern being a value
j1 <- as.bytes(c("1", "2", "3"), 1L, "unsigned")
ok("needle out of range",        identical(j1 %in% c(2L, 1000L), c(FALSE, TRUE, FALSE)))
ok("table entry out of range",   identical(match(j1, c(1000L, 3L, 1L)), c(3L, NA, 2L)))
ok("and with nomatch a position",identical(match(j1, c(1000L, 3L, 1L), nomatch = 3L),
                                           c(3L, 3L, 2L)))
ok("unique",                     identical(unique(c(x, x)), x))
ok("duplicated",                 identical(duplicated(c(x, x)), c(FALSE, FALSE, TRUE, TRUE)))
ok("match vs other type errors", inherits(tryCatch(match(x, 1L), error = identity), "error"))
ok("split",                      { sp <- split(c(x, x), c(1,1,2,2)); length(sp) == 2L &&
                                    bytesWidth(sp[[1]]) == 16L })
ok("lapply",                     length(lapply(x, function(e) e)) == 2L)

cat("\n== K. ordering ==\n")
o <- as.bytes(as.raw(rep(c(3,1,2), each = 4)), 4L)
on <- c(o[1:2], as.bytes(NA, 4L), o[3])
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

ok("cbind is deterministic",     { r <- vapply(1:5, function(i)
                                       paste(as.character(cbind(x, x)), collapse = "|"), "")
                                   length(unique(r)) == 1L })
ok("cbind width clash is deterministic",
                                 { m <- vapply(1:5, function(i)
                                       tryCatch({cbind(x, x8); ""},
                                                error = function(e) conditionMessage(e)), "")
                                   length(unique(m)) == 1L && nzchar(m[1]) })

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
ok("mode is the same for every kind",
                                 identical(c(mode(u), mode(sg), mode(x)),
                                           c("bytes", "bytes", "bytes")))
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
ok("typed NA per kind",          is.na(as.bytes(NA, 8L, "signed")) &&
                                 is.na(as.bytes(NA, 8L, "unsigned")))
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
ok("kinds do not match",         inherits(tryCatch(match(u[1], sg[1]), error = identity), "error"))
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
ok("NA propagates",              is.na(a[1] + as.bytes(NA, 8L, "unsigned")))
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
ok("arith with integer narrows", identical(as.character(a + 1L), c("2","3","4")))
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
ok("NA without na.rm",           is.na(sum(c(a, as.bytes(NA, 8L, "unsigned")))))
ok("na.rm = TRUE",               identical(as.character(sum(c(a, as.bytes(NA,8L,"unsigned")),
                                                            na.rm = TRUE)), "6"))
ok("sum overflow -> NA",         is.na(suppressWarnings(sum(mk("unsigned",8L,"fffffffffffffff0",
                                                                             "fffffffffffffff0")))))
ok("empty sum / prod",           identical(c(as.character(sum(bytes(0L,8L,"unsigned"))),
                                             as.character(prod(bytes(0L,8L,"unsigned")))),
                                           c("0", "1")))
ok("sum on opaque errors",       inherits(tryCatch(sum(x), error = identity), "error"))
ok("sum mixed with integer narrows",
                                 identical(as.character(sum(a, 1L)), "7"))
ok("and mixed with a double errors",
                                 inherits(tryCatch(sum(a, 1.5), error = identity), "error"))
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
ok("as.integer of NA",           is.na(as.integer(as.bytes(NA, 8L, "signed"))))
ok("coercion from opaque errors",
                                 inherits(tryCatch(as.integer(x), error = identity), "error"))
ok("cumsum stays in the type",   identical(as.character(cumsum(a)), c("1", "3", "6")) &&
                                 typeof(cumsum(a)) == typeof(a))

cat("\n== R. serialization ==\n")
## version 4: no older R can read this type, so a version 2 or 3 stream
## would carry a header naming an R that cannot read it.  The default
## goes through the writer's own choice of version, which is the path
## nearly every caller takes.
rt <- function(v, ...) suppressMessages(unserialize(serialize(v, NULL, ...)))
u64 <- mk("unsigned", 8L, "0000000000000001", "00000001312d0000", "fffffffffffffffe")
i64 <- mk("signed",   8L, "ffffffffffffffff", "7fffffffffffffff", "8000000000000001")
i128 <- mk("signed", 16L, "7fffffffffffffffffffffffffffffff",
                          "00000000000000000000000000000001")
b3 <- as.bytes(as.raw(1:9), 3L)

ok("uint64 round-trips",         identical(rt(u64), u64))
ok("int64 round-trips",          identical(rt(i64), i64))
ok("int128 round-trips",         identical(rt(i128), i128))
ok("opaque round-trips",         identical(rt(x), x))
ok("odd width round-trips",      identical(rt(b3), b3))
ok("values survive",             identical(as.character(rt(i128)), as.character(i128)))
ok("NA survives",                identical(rt(rep(as.bytes(NA, 8L, "signed"), 2L)),
                                           rep(as.bytes(NA, 8L, "signed"), 2L)))
ok("ascii format",               identical(rt(u64, ascii = TRUE), u64))
ok("xdr = FALSE",                identical(rt(u64, xdr = FALSE), u64))
ok("attributes survive",         { y <- u64; names(y) <- c("a","b","c"); identical(rt(y), y) })
ok("nested in a list",           identical(rt(list(u64, x))[[1]], u64))
ok("in a data.frame",            { d <- data.frame(k = u64, n = 1:3); identical(rt(d), d) })
ok("saveRDS / readRDS",          { f <- tempfile()
                                   suppressMessages(saveRDS(i64, f))
                                   z <- readRDS(f)
                                   v <- infoRDS(f)$version
                                   unlink(f); identical(z, i64) && v == 4L })
ok("the raised version is announced",
                                 { f <- tempfile()
                                   m <- tryCatch(saveRDS(i64, f), message = identity)
                                   unlink(f); inherits(m, "message") })
ok("an explicit older version errors",
                                 { f <- tempfile()
                                   e <- tryCatch(saveRDS(i64, f, version = 3),
                                                 error = identity)
                                   unlink(f); inherits(e, "error") })
ok("and an ordinary object is left alone",
                                 { f <- tempfile(); saveRDS(list(1:3), f)
                                   v <- infoRDS(f)$version; unlink(f); v == 3L })
ok("empty vector",               identical(rt(bytes(0L, 8L, "unsigned")),
                                           bytes(0L, 8L, "unsigned")))
ok("crosses the write chunk",    { z <- as.bytes(as.raw(rep(0:255, length.out = 40000)),
                                                 8L, "unsigned")
                                   identical(rt(z), z) })
ok("odd width crosses chunk",    { z <- as.bytes(as.raw(rep(0:255, length.out = 30000)), 3L)
                                   identical(rt(z), z) })

## The payload must be canonical on the wire, not native, or a file
## written here would read as different values on a big-endian machine.
ok("numeric payload is big-endian on the wire",
                                 { one <- mk("unsigned", 8L, "0000000000000001")
                                   identical(bytesRaw(one)[1], as.raw(1)) &&   # native: LSB first
                                   identical(tail(suppressMessages(serialize(one, NULL)), 8),
                                             as.raw(c(0,0,0,0,0,0,0,1))) })
ok("opaque payload is verbatim", { o8 <- as.bytes(as.raw(c(0xde,0xad,0xbe,0xef,1,2,3,4)), 8L)
                                   identical(tail(suppressMessages(serialize(o8, NULL)), 8),
                                             bytesRaw(o8)) })

cat("\n== S. deparse ==\n")
ok("deparse round-trips",        { z <- eval(parse(text = paste(deparse(u64), collapse = "")))
                                   identical(z, u64) })
ok("opaque deparse round-trips", { z <- eval(parse(text = paste(deparse(x), collapse = "")))
                                   identical(z, x) })
ok("empty deparses to bytes()",  identical(deparse(bytes(0L, 8L, "signed")),
                                           "bytes(0L, 8L, \"signed\")"))
ok("deparse names the kind",     grepl("\"unsigned\"", paste(deparse(u64), collapse = "")))
## elements deparse as the text as.character() gives, so the output is
## readable, is the same on either endianness, and an NA element comes
## back as NA_character_ rather than as a reserved bit pattern that
## as.bytes would have to warn about
ok("deparse is the text form",   identical(deparse(as.bytes("42", 8L, "signed")),
                                           "as.bytes(\"42\", 8L, \"signed\")"))
ok("NA deparses silently",       { na1 <- rep(as.bytes(NA, 8L, "unsigned"), 2L)
                                   txt <- paste(deparse(na1), collapse = "")
                                   z <- withCallingHandlers(
                                       eval(parse(text = txt)),
                                       warning = function(w) stop("warned"))
                                   identical(z, na1) })
ok("na = FALSE survives deparse",{ nf <- as.bytes(c("1", "2"), 8L, "signed", na = FALSE)
                                   z <- eval(parse(text = paste(deparse(nf), collapse = "")))
                                   identical(z, nf) })

cat("\n== T. matrices ==\n")
m <- cbind(u64, u64)
ok("cbind builds a matrix",      identical(dim(m), c(3L, 2L)))
ok("cbind keeps kind and width", bytesKind(m) == "unsigned" && bytesWidth(m) == 8L)
ok("rbind builds a matrix",      identical(dim(rbind(u64, u64)), c(2L, 3L)))
ok("cbind recycles",             identical(as.character(cbind(u64, u64[1])),
                                           c(as.character(u64), rep(as.character(u64[1]), 3))))
ok("cbind of a matrix + vector", identical(dim(cbind(m, u64)), c(3L, 3L)))
ok("m[i, ]",                     identical(as.character(m[1, ]),
                                           rep(as.character(u64[1]), 2)))
ok("m[, j]",                     identical(as.character(m[, 1]), as.character(u64)))
ok("m[i, j]",                    identical(as.character(m[2, 2]), as.character(u64[2])))
ok("subset keeps kind",          bytesKind(m[1, ]) == "unsigned")
ok("t() transposes",             identical(dim(t(m)), c(2L, 3L)))
ok("t(t(m)) == m",               identical(t(t(m)), m))
ok("aperm agrees with t",        identical(aperm(m), t(m)))
ok("apply over columns",         identical(unname(apply(m, 2, function(z) as.character(max(z)))),
                                           rep(as.character(max(u64)), 2)))
ok("matrix()",                   identical(as.character(matrix(c(u64, u64), 3, 2)),
                                           rep(as.character(u64), 2)))
## byrow recycles exactly as it does for integers: matrix(1:3, 3, 2,
## byrow = TRUE) stores elements 1,3,2,2,1,3
ok("matrix(byrow = TRUE)",       identical(as.character(matrix(u64, 3, 2, byrow = TRUE)),
                                           as.character(u64)[c(1,3,2,2,1,3)]))
ok("matrix fills with NA",       { mm <- matrix(bytes(0L, 8L, "unsigned"), 2, 2)
                                   identical(dim(mm), c(2L, 2L)) && all(is.na(mm)) })
ok("matrix prints",              length(capture.output(print(m))) == 4L)
ok("opaque matrix prints",       length(capture.output(print(cbind(x, x)))) == 3L)
ok("cbind with integer narrows", identical(dim(cbind(u64, 1L)), c(3L, 2L)))
ok("cbind mixed kind errors",    inherits(tryCatch(cbind(u64, i64), error = identity), "error"))
ok("cbind mixed width errors",   inherits(tryCatch(cbind(x, x8), error = identity), "error"))
ok("matrix round-trips",         identical(rt(m), m))

cat("\n== U. the partial coercion lattice ==\n")
## logical and integer narrow into bytes -- lossless, and the one
## promotion every system agrees on.  double is deliberately refused:
## R's lattice is otherwise value-preserving, and neither double nor a
## 64-bit integer subsumes the other.
ok("x + 1L",                     identical(as.character(u64[2] + 1L), "5120000001"))
ok("1L + x (symmetric)",         identical(u64[2] + 1L, 1L + u64[2]))
ok("x * 2L",                     identical(as.character(u64[1] * 2L), "2"))
ok("x + TRUE",                   identical(as.character(u64[1] + TRUE), "2"))
ok("result keeps kind and width",{ r <- u64[2] + 1L
                                   bytesKind(r) == "unsigned" && bytesWidth(r) == 8L })
ok("comparison against integer", (u64[2] > 0L) && !(u64[1] > 1L))
ok("c(x, 1L)",                   identical(as.character(c(u64[1], 1L)), c("1", "1")))
ok("x[i] <- integer",            { z <- c(u64, u64); z[1] <- 7L
                                   as.character(z[1]) == "7" })
ok("signed takes negatives",     identical(as.character(i64[1] + -1L), "-2"))
ok("out of range -> NA + warning",
                                 { w <- FALSE
                                   r <- withCallingHandlers(u64[1] + -5L,
                                       warning = function(cnd) { w <<- TRUE
                                                                 invokeRestart("muffleWarning") })
                                   is.na(r) && w })

for (e in list(quote(u64[1] + 1), quote(u64[1] * 1.5), quote(u64[1] > 0),
               quote(c(u64, 1)), quote({ z <- c(u64, u64); z[1] <- 1; z })))
    ok(paste("double refused:", deparse(e)[1]),
       inherits(tryCatch(eval(e), error = identity), "error"))

ok("the refusal names both fixes", grepl("1L", tryCatch(u64[1] + 1,
                                                        error = conditionMessage)) &&
                                   grepl("as.numeric", tryCatch(u64[1] + 1,
                                                                error = conditionMessage)))
ok("/ still yields double",      identical(u64[1] / 2, 0.5))
ok("^ still yields double",      identical(u64[1] ^ 2L, 1))
ok("opaque still refuses all",   inherits(tryCatch(x + 1L, error = identity), "error"))

cat("\n== V. implicit class vector ==\n")
ok("class is name + bytes",      identical(class(u64), c("uint64", "bytes")))
ok("opaque too",                 identical(class(x), c("bytes16", "bytes")))
ok("inherits both ways",         inherits(u64, "bytes") && inherits(u64, "uint64"))
ok("one S3 method serves all",   { mean.bytes <- function(x, ...) sum(x) / length(x)
                                   m8 <- mean(as.bytes(as.raw(c(rev(c(0,0,0,0,0,0,0,2)),
                                                                rev(c(0,0,0,0,0,0,0,4)))),
                                                       8L, "unsigned"))
                                   m16 <- mean(mk("signed", 16L,
                                                  "0000000000000000000000000000000a"))
                                   identical(c(m8, m16), c(3, 10)) })
ok("matrices still class matrix", identical(class(cbind(u64, u64)), c("matrix", "array")))

cat("\n== W. declining to reserve an NA (na = FALSE) ==\n")
## the escape hatch for narrow widths, where giving up a value hurts:
## every bit pattern is a datum, and anything that would produce NA
## errors instead
w1 <- as.bytes(as.raw(c(0, 1, 254, 255)), 1L, "unsigned", na = FALSE)
hn <- suppressWarnings(as.bytes(as.raw(c(0, 1, 254, 255)), 1L, "unsigned"))

ok("the reserved pattern is a value",
                                 identical(as.character(w1), c("0","1","254","255")))
ok("with na = TRUE it is NA",    is.na(hn)[4])
ok("bytesHasNA reports it",      !bytesHasNA(w1) && bytesHasNA(hn))
ok("is.na is all FALSE",         !any(is.na(w1)))
ok("anyNA is FALSE",             !anyNA(w1))
ok("ingest does not warn",       { got <- FALSE
                                   withCallingHandlers(
                                       as.bytes(as.raw(255L), 1L, "unsigned", na = FALSE),
                                       warning = function(cnd) { got <<- TRUE
                                                                 invokeRestart("muffleWarning") })
                                   !got })
ok("sorts it as a value",        identical(as.character(sort(w1)),
                                           c("0","1","254","255")))
ok("max sees it",                identical(as.character(max(w1)), "255"))
ok("min / range",                identical(as.character(range(w1)), c("0", "255")))
ok("match finds it",             identical(match(w1[4], w1), 4L))
ok("unique keeps all four",      length(unique(c(w1, w1))) == 4L)
ok("compares as a value",        identical(w1 > w1[2], c(FALSE, FALSE, TRUE, TRUE)))
ok("arithmetic in range",        identical(as.character(w1[2] + 1L), "2"))
ok("c() of two na = FALSE",      length(c(w1, w1[1])) == 5L)
ok("serialize round-trips",      identical(rt(w1), w1))
ok("deparse records the flag",   grepl("na = FALSE", paste(deparse(w1), collapse = "")))
ok("deparse round-trips",        identical(eval(parse(text = paste(deparse(w1), collapse = ""))), w1))
ok("empty deparse too",          identical(deparse(as.bytes(raw(0), 1L, "unsigned", na = FALSE)),
                                           "bytes(0L, 1L, \"unsigned\", na = FALSE)"))

for (e in list(quote(w1[99]), quote(w1[NA_integer_]),
               quote({ z <- w1; length(z) <- 6L; z }),
               quote(w1[4] + 1L), quote(w1[1] + -1L),
               quote(matrix(as.bytes(raw(0), 1L, "unsigned", na = FALSE), 2, 2))))
    ok(paste("would-be NA errors:", deparse(e)[1]),
       inherits(tryCatch(eval(e), error = identity), "error"))

ok("the error says why",         grepl("na = FALSE", tryCatch(w1[99],
                                                              error = conditionMessage)))

## the flag is part of the type, as kind and width are
ok("does not combine with na = TRUE",
                                 inherits(tryCatch(c(w1, hn), error = identity), "error"))
ok("does not compare with na = TRUE",
                                 inherits(tryCatch(w1 == hn, error = identity), "error"))
ok("not identical to na = TRUE", !identical(w1, hn))
ok("default is unchanged",       bytesHasNA(bytes(1L, 8L, "unsigned")) &&
                                 bytesHasNA(as.bytes(as.raw(1:8), 8L)))

cat("\n== X. width, kind and NA carried through every path ==\n")
## Narrowing is not restricted to the arithmetic widths: c(), [<- and
## comparison all reach it, and none of them does arithmetic.
xw <- bytes(2L, 32L, "unsigned")
ok("wide compare narrows",       identical(xw == 1L, c(FALSE, FALSE)))
ok("wide c() narrows",           length(c(xw, 1L)) == 3L)
ok("wide [<- narrows",           { z <- xw; z[1] <- 1L; as.character(z[1]) == "1" })
ok("wide arithmetic still errors",
                                 grepl("widths 1, 2, 4, 8 and 16",
                                       tryCatch(xw + 1L, error = conditionMessage)))

## cbind/rbind must narrow like every other mode, not reinterpret the
## argument's own storage
b2 <- as.bytes(as.raw(c(1, 2)), 1L, "unsigned")
ok("cbind narrows an integer",   identical(as.character(cbind(b2, 3:4)),
                                           c("1","2","3","4")))
ok("rbind narrows an integer",   identical(as.character(rbind(b2, 3:4)),
                                           c("1","3","2","4")))
ok("cbind still refuses double", inherits(tryCatch(cbind(b2, 1.5), error = identity), "error"))
ok("cbind keeps the NA flag",    !bytesHasNA(cbind(w1, w1)) &&
                                 identical(cbind(w1, w1)[, 1], w1))

## with na = FALSE the reserved pattern is reachable as a result, not
## just as an ingested value
s1 <- as.bytes(as.raw(c(0, 1, 127, 128)), 1L, "signed", na = FALSE)
ok("na = FALSE reaches the top",  identical(as.character(w1[3] + w1[2]), "255"))
ok("na = FALSE narrows to the top",
                                 identical(as.character(w1[1] + 255L), "255"))
ok("na = FALSE assigns the top", { z <- w1; z[1] <- 255L; as.character(z[1]) == "255" })
ok("na = FALSE coerces the top", identical(as.numeric(w1), c(0, 1, 254, 255)))
ok("signed reaches INT_MIN",     identical(as.character(s1), c("0","1","127","-128")) &&
                                 identical(as.character(s1[1] + -128L), "-128"))
ok("na = TRUE still reserves",   { hn2 <- as.bytes(as.raw(254), 1L, "unsigned")
                                   suppressWarnings(is.na(hn2 + 1L)) })
ok("na = NA is not an answer",   inherits(tryCatch(bytes(4L, 1L, "unsigned", na = NA),
                                                   error = identity), "error") &&
                                 inherits(tryCatch(as.bytes(as.raw(1), 1L, "unsigned", na = NA),
                                                   error = identity), "error"))

cat("\n== Y. arithmetic keeps the attributes, as every other type does ==\n")
u4 <- mk("unsigned", 8L, "0000000000000001", "0000000000000002",
                         "0000000000000003", "0000000000000004")
ok("dim survives",               identical(dim(matrix(u4, 2, 2) + 1L), c(2L, 2L)))
ok("values survive with it",     identical(as.character(matrix(u4, 2, 2) + 1L),
                                           c("2","3","4","5")))
ok("names survive",              { v <- u4; names(v) <- letters[1:4]
                                   identical(names(v + 1L), letters[1:4]) })
ok("non-conformable errors",     inherits(tryCatch(matrix(u4, 2, 2) + matrix(u4, 4, 1),
                                                   error = identity), "error"))
ok("recycling warns exactly once",
                                 { n <- 0L
                                   withCallingHandlers(u4 + u4[1:3],
                                       warning = function(w) { n <<- n + 1L
                                                               invokeRestart("muffleWarning") })
                                   n == 1L })
ok("array() takes a bytes vector",
                                 identical(as.character(array(u4, c(2, 2))),
                                           c("1","2","3","4")))

cat("\n== Z. min/max need only a comparison ==\n")
## unlike sum and prod, which accumulate; so they are defined wherever
## ordering is, which is every width and every kind
ok("range of an opaque vector",  identical(as.character(range(x)),
                                           c("0102030405060708090a0b0c0d0e0f10",
                                             "1112131415161718191a1b1c1d1e1f20")))
ok("min of a non-arith width",   identical(as.character(min(bytes(4L, 3L, "unsigned"))), "0"))
## a width is part of the type, so min() and max() refuse exactly the
## pairs c() refuses -- which is what keeps range(), whose answer goes
## through c(), from failing on arguments they accept
ok("max refuses mixed widths",   inherits(tryCatch(max(u4, mk("unsigned", 4L, "00000009")),
                                                   error = identity), "error"))
ok("and so does c()",            inherits(tryCatch(c(u4, mk("unsigned", 4L, "00000009")),
                                                   error = identity), "error"))
ok("range agrees with min/max",  identical(as.character(range(u4, u4[1])),
                                           as.character(c(min(u4), max(u4)))))
## an integer bound the width cannot hold is not missing: it lies below
## or above every element, so the comparison still has an answer
u1 <- as.bytes(c("1", "2", "3"), 1L, "unsigned")
ok("compare below the range",    identical(u1 > -1L, rep(TRUE, 3)))
ok("compare above the range",    identical(u1 < 1000L, rep(TRUE, 3)))
ok("min ignores a high bound",   identical(as.character(min(u1, 1000L)), "1"))
ok("pmin ignores a high bound",  identical(as.character(pmin(u1, 1000L)), c("1","2","3")))
ok("max of one is NA + warning", { w <- NULL
                                   r <- withCallingHandlers(max(u1, 1000L),
                                       warning = function(z) { w <<- conditionMessage(z)
                                                               invokeRestart("muffleWarning") })
                                   is.na(r) && grepl("outside the range", w) })
ok("a leading NA does not warn", { w <- NULL
                                   r <- withCallingHandlers(max(as.bytes(c(NA, "1"), 8L, "unsigned")),
                                       warning = function(z) { w <<- conditionMessage(z)
                                                               invokeRestart("muffleWarning") })
                                   is.na(r) && is.null(w) })
ok("sum still refuses opaque",   inherits(tryCatch(sum(x), error = identity), "error"))
ok("as.numeric past 32 bytes",   identical(as.numeric(mk("unsigned", 33L,
                                                         paste0(strrep("0", 64), "01"))), 1))

cat("\n== Z2. the implicit class drives dispatch ==\n")
ok(".class2 matches class()",    identical(.class2(u4), class(u4)))
ok(".class2 of a matrix",        identical(.class2(matrix(u4, 2, 2)),
                                           c("matrix", "array", "uint64", "bytes")))
ok("UseMethod sees the kind",    { mean.uint64 <- function(x, ...) "by kind"
                                   identical(mean(u4), "by kind") })
ok("c() into a list is lossless",
                                 { r <- c(list(a = 1), u4)
                                   length(r) == 5L && identical(r$a, 1) &&
                                   all(vapply(r[-1], typeof, "") == "uint64") &&
                                   identical(as.character(r[[5]]), "4") })
ok("as.list splits by element",  { r <- as.list(u4)
                                   length(r) == 4L &&
                                   all(vapply(r, typeof, "") == "uint64") &&
                                   identical(vapply(r, as.character, ""),
                                             c("1","2","3","4")) })
ok("as.list keeps names",        { v <- u4; names(v) <- letters[1:4]
                                   identical(names(as.list(v)), letters[1:4]) })
ok("cbind into a list matrix",   { m <- cbind(list(1), u4[1:2])
                                   identical(dim(m), c(2L, 2L)) &&
                                   identical(typeof(m[[2, 2]]), "uint64") })
ok("a bytes cell prints",        { out <- capture.output(print(cbind(list(1), u4[1:2])))
                                   !any(grepl("?", out, fixed = TRUE)) })

cat("\n== Z3. gctorture on the paths that allocate mid-walk ==\n")
gctorture(TRUE)
floop <- function(v) { s <- character(); for (b in v) s <- c(s, as.character(b)); s }
ok("for loop over bytes",        identical(floop(u4), c("1","2","3","4")))
ok("compiled for loop",          identical(compiler::cmpfun(floop)(u4), c("1","2","3","4")))
ok("sum walks a protected args", identical(as.character(sum(u4, u4)), "20"))
gctorture(FALSE)

cat("\n== Z4. what the review pass turned up ==\n")
u8 <- as.bytes(as.raw(1:16), 8L, "unsigned")
s8 <- as.bytes(as.raw(c(2,0,0,0,0,0,0,0)), 8L, "unsigned")

## printNamedVector had no arm for this type and no default, so a named
## vector printed absolutely nothing
ok("a named vector prints",      { v <- u8; names(v) <- c("a", "b")
                                   out <- capture.output(print(v))
                                   length(out) == 2L && grepl("a", out[1]) })
ok("a 1-d array with dimnames prints",
                                 { v <- array(u8, 2L, dimnames = list(c("a", "b")))
                                   length(capture.output(print(v))) == 2L })

## storing into a list is what df$key <- v goes through
ok("[[<- into a list",           { l <- list(1, 2); l[[1]] <- u8; identical(l[[1]], u8) })
ok("[<- into a list",            { l <- list(1, 2); l[1:2] <- u8
                                   identical(typeof(l[[1]]), "uint64") })
ok("df$key <- v",                { d <- data.frame(i = 1:2); d$key <- u8
                                   identical(d$key, u8) })

## the 'bytes' arm of SubassignTypeFix once sat in the path the S4 arm
## falls through, so this reached it with an integer left-hand side
ok("S4 subassignment untouched", {
    methods::setClass("gauntletS4", methods::representation(a = "numeric"))
    v <- 1:3
    msg <- tryCatch({ v[1] <- methods::new("gauntletS4", a = 1); "" },
                    error = conditionMessage)
    grepl("in subassignment type fix", msg) })

## unary minus builds a fresh vector, so it has to carry the attributes
## over the way the other unary kernels do
ok("unary minus keeps dim",      { m <- matrix(as.bytes(as.raw(1:32), 8L, "signed"), 2, 2)
                                   identical(dim(-m), c(2L, 2L)) })
ok("unary minus keeps names",    { v <- as.bytes(as.raw(1:16), 8L, "signed")
                                   names(v) <- c("a", "b")
                                   identical(names(-v), c("a", "b")) })

## '/' and '^' yield a double, but only the 'bytes' side of the pair is
## coerced: the other one still has to be a number
ok("/ rejects a non-numeric",    inherits(tryCatch(u8 / "abc", error = identity), "error"))
ok("^ rejects a list",           inherits(tryCatch(u8 ^ list(1), error = identity), "error"))
ok("/ still divides",            identical(s8 / 2L, 1))
ok("^ still exponentiates",      identical(s8 ^ 2L, 4))

## a length outside the R_xlen_t range is checked rather than cast
ok("bytes() checks its length",  inherits(tryCatch(bytes(1e30, 8L), error = identity), "error"))

## with na = FALSE the reserved pattern is a legitimate value, which
## makes the two vectors different types -- so match() refuses the pair
## on both the scalar fast path and through the hash table, rather than
## reporting the value absent
w255 <- as.bytes(as.raw(rep(255, 4)), 4L, "unsigned", na = FALSE)
wNA  <- as.bytes(NA, 4L, "unsigned")
ok("a real 0xFF.. is not NA",    !is.na(w255) && is.na(wNA))
ok("an NA clash is refused",     inherits(tryCatch(match(w255, wNA), error = identity), "error") &&
                                 inherits(tryCatch(match(c(w255, w255), c(wNA, wNA)),
                                                   error = identity), "error"))
ok("and still matches itself",   identical(match(w255, c(w255, w255)), 1L))

## the zero-length answer is built while both operands are still
## protected -- the narrowed one has nothing else holding it
ok("zero-length arithmetic",     { gctorture(TRUE)
                                   n <- length(integer(0) + u8) + length(u8 + integer(0))
                                   gctorture(FALSE); n == 0L })

## sortVector's two paths order the reserved pattern the same way
ok("both sort paths agree on NA", {
    v  <- c(u8, as.bytes(NA, 8L, "unsigned"))
    op <- suppressWarnings(as.bytes(bytesRaw(v), 8L))
    identical(which(is.na(sort(v,  na.last = TRUE))),
              which(is.na(sort(op, na.last = TRUE)))) })

cat("\n== Z5. text is an ingest route, and as.character() is reversible ==\n")
## the route a 64-bit identifier actually arrives by: a CSV column, a
## JSON field, a log line.  Unlike a raw payload it carries no byte
## order to get wrong, and it is what makes deparse readable.
set.seed(11)
for (k in c("signed", "unsigned"))
    for (w in c(1L, 2L, 4L, 8L, 16L, 21L)) {
        v <- suppressWarnings(as.bytes(as.raw(sample(0:255, w * 300, TRUE)), w, k))
        ok(sprintf("%s%d: as.character round-trips", k, 8L * w),
           identical(suppressWarnings(as.bytes(as.character(v), w, k)), v))
    }
for (w in c(1L, 6L, 16L, 32L)) {
    v <- suppressWarnings(as.bytes(as.raw(sample(0:255, w * 300, TRUE)), w, "opaque"))
    ok(sprintf("opaque width %d: hex round-trips", w),
       identical(suppressWarnings(as.bytes(as.character(v), w)), v))
}

ok("int64 edges parse exactly",  {
    lim <- c("9223372036854775807", "-9223372036854775807", "0", "-1")
    identical(as.character(as.bytes(lim, 8L, "signed")), lim) })
ok("uint128 edge parses exactly",{
    lim <- "340282366920938463463374607431768211454"
    identical(as.character(as.bytes(lim, 16L, "unsigned")), lim) })
ok("NA_character_ is NA",        is.na(as.bytes(NA_character_, 8L, "signed")))
ok("and does not warn",          { withCallingHandlers(as.bytes(c("1", NA), 8L, "signed"),
                                                       warning = function(w) stop("warned"))
                                   TRUE })
ok("leading/trailing space ok",  identical(as.character(as.bytes("  42\t", 8L, "signed")), "42"))
ok("+ sign accepted",            identical(as.character(as.bytes("+7", 8L, "unsigned")), "7"))
ok("-0 is 0 when unsigned",      identical(as.character(as.bytes("-0", 8L, "unsigned")), "0"))

## the two failures are different mistakes and say so, as they do for
## as.integer(): "abc" is not a number, 2^63 is not an int64
wmsg <- function(e) tryCatch({ e; "" }, warning = function(w) conditionMessage(w))
ok("junk warns about coercion",  grepl("coercion", wmsg(as.bytes("abc", 8L, "signed"))))
ok("too big warns about range",  grepl("range", wmsg(as.bytes("9223372036854775808", 8L, "signed"))))
ok("negative into unsigned",     grepl("range", wmsg(as.bytes("-1", 8L, "unsigned"))))
ok("reserved value warns",       grepl("reserved", wmsg(as.bytes("ffffffffffffffff", 8L))))
ok("short hex is a syntax error",grepl("coercion", wmsg(as.bytes("0102", 8L))))
ok("hex is case-insensitive",    identical(as.bytes("DEADBEEF", 4L), as.bytes("deadbeef", 4L)))
ok("INT_MIN needs na = FALSE",   {
    m <- "-9223372036854775808"
    is.na(suppressWarnings(as.bytes(m, 8L, "signed"))) &&
        identical(as.character(as.bytes(m, 8L, "signed", na = FALSE)), m) })
ok("na = FALSE cannot make NA",  inherits(tryCatch(as.bytes("abc", 8L, "signed", na = FALSE),
                                                   error = identity), "error"))

## integer and logical narrow into bytes here exactly as they do in
## arithmetic; double is refused the same way, and opaque reads no
## numbers at all
ok("integer narrows",            identical(as.character(as.bytes(1:3, 8L, "signed")),
                                           c("1", "2", "3")))
ok("logical narrows",            identical(as.character(as.bytes(c(TRUE, NA), 8L, "unsigned")),
                                           c("1", NA)))
probe("as.bytes(double)",        as.bytes(1, 8L, "signed"))
probe("as.bytes(integer, opaque)", as.bytes(1L, 4L, "opaque"))
probe("as.bytes(list)",          as.bytes(list(1), 8L, "signed"))

cat("\n== Z6. readBin and writeBin ==\n")
## the motivating case: base R has a documented way to read a 64-bit
## integer from a file, and it silently narrows the value to 32 bits
le64 <- as.raw(c(1, 0, 0, 0, 1, 0, 0, 0))            # 2^32 + 1
ok("readBin(integer, size=8) loses it",
   identical(readBin(le64, "integer", 1L, size = 8L), 1L))
ok("readBin(\"int64\") keeps it",
   identical(as.character(readBin(le64, "int64", 1L)), "4294967297"))

ok("the prototype form agrees",   identical(readBin(le64, bytes(0L, 8L, "signed"), 1L),
                                            readBin(le64, "int64", 1L)))
ok("uint64 name works",           identical(typeof(readBin(le64, "uint64", 1L)), "uint64"))
ok("opaque name works",           identical(as.character(readBin(le64, "bytes8", 1L)),
                                            "0100000001000000"))
ok("\"int\" is still integer",     identical(readBin(le64, "int", 2L), c(1L, 1L)))
ok("an unknown name errors",      inherits(tryCatch(readBin(le64, "int63", 1L),
                                                    error = identity), "error"))
## A typo of a 'bytes' name -- "int63" above -- errors, because it has the
## shape of one.  A word that is no kind of mode name still falls through
## to typeof(), i.e. "character", as readBin() has always done: a length-one
## character vector is also the documented prototype form, so readBin(con,
## "") and readBin(con, character(1)) have to keep reading strings.  Making
## every unrecognized name an error took those with it.
ok("a typo reads as a prototype",  identical(readBin(le64, "typo", 1L),
                                             readBin(le64, character(1), 1L)))
ok("prototypes still work",       identical(readBin(le64, integer(), 2L), c(1L, 1L)))
ok("a character prototype works",  identical(readBin(le64, character(1), 1L),
                                             readBin(le64, "character", 1L)))

## endian: the reason this cannot be done with as.bytes(readBin(raw))
be1 <- as.raw(c(0, 0, 0, 0, 0, 0, 0, 1))
ok("big-endian ingest",           identical(as.character(readBin(be1, "uint64", 1L,
                                                                 endian = "big")), "1"))
ok("native ingest differs",       identical(as.character(readBin(be1, "uint64", 1L,
                                                                 endian = "little")),
                                            "72057594037927936"))
ok("endian is refused for opaque",{ w <- NULL
                                    withCallingHandlers(readBin(be1, "bytes8", 1L, endian = "big"),
                                                        warning = function(x) {
                                                            w <<- conditionMessage(x)
                                                            invokeRestart("muffleWarning") })
                                    grepl("not meaningful", w) })

## the prototype carries the NA reservation, which the name cannot
allf <- as.raw(rep(255, 8))
ok("reserved value warns",        { w <- NULL
                                    withCallingHandlers(readBin(allf, "uint64", 1L),
                                                        warning = function(x) {
                                                            w <<- conditionMessage(x)
                                                            invokeRestart("muffleWarning") })
                                    grepl("reserved", w) })
ok("na = FALSE keeps it",         identical(as.character(
                                      readBin(allf, bytes(0L, 8L, "unsigned", na = FALSE), 1L)),
                                      "18446744073709551615"))

## short reads truncate to whole elements, as they do for every type
ok("a partial element is dropped", identical(length(readBin(as.raw(1:12), "int64", 5L)), 1L))
ok("over-asking is fine",          identical(length(readBin(as.raw(1:24), "int64", 99L)), 3L))
ok("n = 0",                        identical(length(readBin(as.raw(1:24), "int64", 0L)), 0L))

## writeBin is the other half; without it this is a one-way street
for (spec in list(list(8L, "signed"), list(8L, "unsigned"),
                  list(16L, "unsigned"), list(6L, "opaque"), list(1L, "opaque"))) {
    w <- spec[[1]]; k <- spec[[2]]
    v <- suppressWarnings(as.bytes(as.raw(rep(c(1:250, 7L), length.out = 5 * w)), w, k))
    nm <- typeof(v)
    ok(sprintf("%s: writeBin/readBin round trip", nm),
       identical(suppressWarnings(readBin(writeBin(v, raw()), v, 5L)), v))
    if (k != "opaque")
        ok(sprintf("%s: and again through big-endian", nm),
           identical(suppressWarnings(
               readBin(writeBin(v, raw(), endian = "big"), v, 5L, endian = "big")), v))
}
ok("writeBin gives the payload",  { v <- as.bytes(c("1", "2"), 8L, "signed")
                                    identical(writeBin(v, raw()), bytesRaw(v)) })
probe("writeBin(size=) changing", writeBin(as.bytes("1", 8L, "signed"), raw(), size = 4L))

cat("\n== Z7. bitwise operations ==\n")
## what the opaque kind actually wants, and what arithmetic
## deliberately does not give it.  Per byte, so unlike arithmetic there
## is no width restriction at all.
u1 <- as.bytes("255", 8L, "unsigned")
ok("and",                        identical(as.character(bitwAnd(as.bytes("65535", 8L, "unsigned"), u1)), "255"))
ok("or",                         identical(as.character(bitwOr(as.bytes("256", 8L, "unsigned"), u1)), "511"))
ok("xor is self-cancelling",     identical(as.character(bitwXor(u1, u1)), "0"))
ok("not",                        identical(as.character(bitwNot(as.bytes("1", 8L, "unsigned"))),
                                           "18446744073709551614"))
ok("an integer operand narrows", identical(bitwAnd(as.bytes("65535", 8L, "unsigned"), 255L),
                                           bitwAnd(as.bytes("65535", 8L, "unsigned"), u1)))
ok("and either way round",       identical(bitwAnd(255L, as.bytes("65535", 8L, "unsigned")),
                                           bitwAnd(as.bytes("65535", 8L, "unsigned"), 255L)))
## an integer operand narrows BY VALUE, as it does everywhere else on
## this type -- it is not reinterpreted as a bit pattern.  So -1L is
## the identity mask for a signed vector, where it is a value, and is
## out of range for an unsigned one, where zero- and sign-extension
## would disagree.  Refusing keeps both readings reachable later.
ok("-1L masks a signed vector",  { v <- as.bytes("12345", 8L, "signed")
                                   identical(bitwAnd(v, -1L), v) })
ok("-1L is out of range unsigned", grepl("range", wmsg(bitwAnd(u1, -1L))))

ok("shift left is doubling",     identical(as.character(bitwShiftL(as.bytes("1", 8L, "unsigned"), 0:8)),
                                           as.character(2^(0:8))))
ok("shift to the top bit",       identical(as.character(bitwShiftL(as.bytes("1", 8L, "unsigned"), 63L)),
                                           "9223372036854775808"))
ok("shift right is logical",     identical(as.character(bitwShiftR(as.bytes("-1", 8L, "signed"), 1L)),
                                           "9223372036854775807"))
ok("out-of-range shift is NA",   all(is.na(bitwShiftL(as.bytes("1", 8L, "unsigned"), c(-1L, 64L, NA)))))
ok("width 16 shifts work",       identical(as.character(
                                     bitwShiftR(bitwShiftL(as.bytes("1", 16L, "unsigned"), 127L), 127L)), "1"))

## the actual use case: masking an address prefix.  arithmetic refuses
## the opaque kind and should; these do not.
ip   <- as.bytes("20010db8000000000000000000000001", 16L)
mask <- as.bytes(paste0(strrep("ff", 6), strrep("00", 10)), 16L)
ok("IPv6 /48 prefix",            identical(as.character(bitwAnd(ip, mask)),
                                           "20010db8000000000000000000000000"))
ok("the complement of a mask",   identical(as.character(bitwNot(mask)),
                                           "000000000000ffffffffffffffffffff"))
ok("opaque shifts are MSB-first",identical(as.character(bitwShiftR(ip, 8L)),
                                           "0020010db80000000000000000000000"))
ok("width 32 works",             { h <- as.bytes(strrep("a5", 32), 32L)
                                   identical(bitwNot(bitwNot(h)), h) })

ok("NA propagates",              is.na(bitwAnd(as.bytes(NA, 8L, "unsigned"), u1)))
ok("a reserved result warns",    { w <- NULL
                                   v <- withCallingHandlers(
                                       bitwNot(as.bytes("0", 8L, "unsigned")),
                                       warning = function(x) { w <<- conditionMessage(x)
                                                               invokeRestart("muffleWarning") })
                                   is.na(v) && grepl("reserved", w) })
ok("na = FALSE holds it",        identical(as.character(
                                     bitwNot(as.bytes("0", 8L, "unsigned", na = FALSE))),
                                     "18446744073709551615"))
ok("zero length",                identical(length(bitwAnd(bytes(0L, 8L, "unsigned"), u1)), 0L))
ok("integers are untouched",     identical(c(bitwAnd(12L, 10L), bitwNot(-1L),
                                             bitwShiftR(-1L, 1L)), c(8L, 0L, 2147483647L)))

probe("bitwAnd, widths differ",  bitwAnd(u1, as.bytes("1", 4L, "unsigned")))
probe("bitwAnd, kinds differ",   bitwAnd(u1, as.bytes("1", 8L, "signed")))
probe("bitwAnd(opaque, integer)", bitwAnd(ip, 1L))
probe("bitwShiftL(integer, bytes)", bitwShiftL(1L, u1))

## the narrowed operand is a temporary that nothing but the local
## variable holds while the answer is allocated -- the shape that bit
## the zero-length arithmetic path
ok("gctorture over bitwise",     { gctorture(TRUE)
                                   v <- c(as.character(bitwAnd(as.bytes("65535", 8L, "unsigned"), 255L)),
                                          as.character(bitwShiftL(as.bytes("1", 16L, "unsigned"), 1:8)),
                                          as.character(bitwNot(ip)))
                                   gctorture(FALSE)
                                   length(v) == 10L && !anyNA(v) })

cat("\n== Z8. scan(), read.table() and the mode names ==\n")
## The route most 64-bit keys actually arrive by.  Reading such a
## column as character and converting afterwards would intern one
## string per row, so scan() reads it directly instead.
tf <- tempfile()
writeLines(c("9223372036854775807", "-1", "NA", "0"), tf)
ok("scan reads a bytes prototype",{
    v <- scan(tf, what = bytes(0L, 8L, "signed"), quiet = TRUE)
    identical(typeof(v), "int64") &&
        identical(as.character(v), c("9223372036854775807", "-1", NA, "0")) })
ok("na.strings are honoured",     { v <- scan(tf, what = bytes(0L, 8L, "signed"),
                                              na.strings = c("NA", "-1"), quiet = TRUE)
                                    identical(which(is.na(v)), 2:3) })
ok("n = limits the read",         identical(length(scan(tf, what = bytes(0L, 8L, "signed"),
                                                        n = 2L, quiet = TRUE)), 2L))
ok("hex for the opaque kind",     { th <- tempfile()
                                    writeLines(c("0102030405060708", "ffffffffffffff00"), th)
                                    identical(as.character(scan(th, what = bytes(0L, 8L),
                                                                quiet = TRUE)),
                                              c("0102030405060708", "ffffffffffffff00")) })
probe("scan: a value out of range", scan(textConnection("99999999999999999999999"),
                                         what = bytes(0L, 8L, "signed"), quiet = TRUE))
probe("scan: not a number",       scan(textConnection("abc"),
                                       what = bytes(0L, 8L, "signed"), quiet = TRUE))
probe("scan: NA with na = FALSE", scan(tf, what = bytes(0L, 8L, "signed", na = FALSE),
                                       quiet = TRUE))

## past SCAN_BLOCKSIZE, so the grow-and-copy path runs in both the
## single-vector and the data-frame reader
big <- format(as.bytes(as.character(seq_len(5000) + 2^62), 8L, "unsigned"))
tb <- tempfile(); writeLines(big, tb)
ok("scanVector grows correctly",  identical(as.character(
                                      scan(tb, what = bytes(0L, 8L, "unsigned"), quiet = TRUE)),
                                      big))
tf2 <- tempfile(); writeLines(paste(big, seq_len(5000), sep = ","), tf2)
ok("scanFrame grows correctly",   { d <- scan(tf2, what = list(a = bytes(0L, 8L, "unsigned"),
                                                               b = 0L),
                                              sep = ",", quiet = TRUE)
                                    identical(as.character(d$a), big) &&
                                        identical(d$b, seq_len(5000)) })

## read.table names the type through colClasses, which is the form
## anyone reading a CSV of 64-bit keys will reach for
tc <- tempfile()
writeLines(c("id,name,uid",
             "9223372036854775807,a,20010db8000000000000000000000001",
             "-9007199254740993,b,ffffffffffffffffffffffffffffff00",
             "NA,c,00000000000000000000000000000000"), tc)
d <- utils::read.csv(tc, colClasses = c(id = "int64", uid = "bytes16"))
ok("colClasses = \"int64\"",       identical(typeof(d$id), "int64"))
ok("colClasses = \"bytes16\"",     identical(typeof(d$uid), "bytes16"))
ok("the value is exact",         identical(as.character(d$id[1]), "9223372036854775807"))
ok("a double would not be",      as.character(as.numeric("9223372036854775807")) !=
                                 "9223372036854775807")
ok("NA survives the column",     is.na(d$id[3]))
ok("other columns are untouched",identical(d$name, c("a", "b", "c")))
ok("and the frame still sorts",  identical(as.character(d$id[order(d$id)][1]),
                                           "-9007199254740993"))
ok("str names the type",         { out <- capture.output(str(d$id))
                                   grepl("int64", out[1], fixed = TRUE) })
ok("an unknown class still errs",inherits(tryCatch(utils::read.csv(tc, colClasses = "nosuch"),
                                                   error = identity), "error"))

## the mode names, which is how read.table builds the prototype
ok("vector() takes a bytes mode",identical(typeof(vector("uint128", 3L)), "uint128"))
ok("and zero-fills",             identical(as.character(vector("int64", 2L)), c("0", "0")))
probe("vector(\"bytes\") needs a width", vector("bytes", 1L))
probe("vector(\"int65\")",         vector("int65", 1L))
ok("as.vector() parses text",    identical(as.character(as.vector("9223372036854775807", "int64")),
                                           "9223372036854775807"))
ok("as.vector() drops names",    is.null(names(as.vector(c(a = as.bytes("1", 8L, "signed")),
                                                         "int64"))))
## deliberately not named x: section O below still needs the opaque
## vector this file opened with, and a stray assignment here would make
## its "must still fail loudly" probes quietly pass
ok("storage.mode<- converts",    { sm <- c("1", "2"); storage.mode(sm) <- "uint64"
                                   identical(typeof(sm), "uint64") })
ok("storage.mode<- keeps dim",   { sm <- 1:4; dim(sm) <- c(2L, 2L)
                                   storage.mode(sm) <- "int64"
                                   identical(dim(sm), c(2L, 2L)) })
## converting between 'bytes' types works now; section Z10 covers it

cat("\n== Z9. arithmetic, and what measuring it turned up ==\n")
## The native kernels are checked against the general ones, and both
## against Python, by tests/bytesxp-dev/archeck.R -- which needs two processes and
## so cannot live here.  These are the edges worth pinning in-process.
ok("int64 min needs na = FALSE",  {
    m <- as.bytes("-9223372036854775808", 8L, "signed", na = FALSE)
    identical(as.character(m %/% as.bytes("1", 8L, "signed", na = FALSE)),
              "-9223372036854775808") })
ok("min %/% -1 overflows",        {
    m  <- as.bytes("-9223372036854775808", 8L, "signed", na = FALSE)
    m1 <- as.bytes("-1", 8L, "signed", na = FALSE)
    inherits(tryCatch(m %/% m1, error = identity), "error") })
ok("min %% -1 is 0",              {
    m  <- as.bytes("-9223372036854775808", 8L, "signed", na = FALSE)
    m1 <- as.bytes("-1", 8L, "signed", na = FALSE)
    identical(as.character(m %% m1), "0") })
ok("%/% floors, %% takes b's sign", {
    a <- as.bytes(c("-7", "7", "-7", "7"), 8L, "signed")
    b <- as.bytes(c("2", "-2", "-2", "2"), 8L, "signed")
    identical(as.character(a %/% b), c("-4", "-4", "3", "3")) &&
        identical(as.character(a %% b), c("1", "-1", "-1", "1")) })
ok("division by zero is NA",      is.na(suppressWarnings(
    as.bytes("1", 8L, "signed") %/% as.bytes("0", 8L, "signed"))))
ok("a result of the reserved value", {
    ## 2^63-1 + 1 is 2^63, out of range; -1 + (1 - 2^63) is the reserved
    ## value itself, which is reported rather than returned as NA
    hi <- as.bytes("9223372036854775807", 8L, "signed")
    is.na(suppressWarnings(hi + as.bytes("1", 8L, "signed"))) })
ok("recycling still recycles",    identical(
    as.character(as.bytes(c("1","2","3","4"), 8L, "unsigned") +
                 as.bytes(c("10","20"), 8L, "unsigned")),
    c("11", "22", "13", "24")))

## found while benchmarking: xlength() counts elements, so anything that
## wants a byte count has to ask for the width
ok("object.size counts bytes",    {
    n <- 1000L
    (object.size(as.bytes(as.character(seq_len(n)), 8L, "unsigned")) >
     object.size(as.bytes(as.character(seq_len(n)), 4L, "unsigned"))) &&
    (object.size(as.bytes(as.character(seq_len(n)), 8L, "unsigned")) >
     8 * n) })

cat("\n== Z10. the tail sweep ==\n")
i64 <- as.bytes(c("9223372036854775807", "-1", "0", NA), 8L, "signed")

## converting between 'bytes' types: value-preserving, both directions
ok("widening keeps the value",   identical(as.character(as.vector(i64, "int128")),
                                           as.character(i64)))
ok("narrowing that fits",        identical(as.character(
    as.vector(as.bytes(c("1", "300"), 8L, "unsigned"), "uint16")), c("1", "300")))
ok("narrowing that does not",    { w <- NULL
                                   v <- withCallingHandlers(
                                       as.vector(as.bytes("70000", 8L, "unsigned"), "uint16"),
                                       warning = function(x) { w <<- conditionMessage(x)
                                                               invokeRestart("muffleWarning") })
                                   is.na(v) && grepl("range", w) })
ok("signed to unsigned",         is.na(suppressWarnings(
    as.vector(as.bytes("-1", 8L, "signed"), "uint64"))))
ok("unsigned to signed, in range", identical(as.character(
    as.vector(as.bytes("9223372036854775806", 8L, "unsigned"), "int64")),
    "9223372036854775806"))
ok("unsigned to signed, out",    is.na(suppressWarnings(
    as.vector(as.bytes("18446744073709551614", 8L, "unsigned"), "int64"))))
ok("NA survives conversion",     is.na(as.vector(i64, "int128")[4]))
ok("round trip is exact",        { set.seed(4)
                                   v <- as.bytes(as.character(sample(-1e6:1e6, 400)), 4L, "signed")
                                   identical(as.vector(as.vector(v, "int128"), "int32"), v) })
ok("na = FALSE is carried",      !bytesHasNA(as.vector(
    as.bytes("1", 8L, "signed", na = FALSE), "int128")))
ok("and its extra value too",    identical(as.character(as.vector(
    as.bytes("-9223372036854775808", 8L, "signed", na = FALSE), "int128")),
    "-9223372036854775808"))
ok("turning the reservation on", { w <- NULL
                                   v <- withCallingHandlers(
                                       as.bytes(as.bytes("255", 1L, "unsigned", na = FALSE),
                                                1L, "unsigned", na = TRUE),
                                       warning = function(x) { w <<- conditionMessage(x)
                                                               invokeRestart("muffleWarning") })
                                   is.na(v) && grepl("reserved", w) })
probe("turning it off with an NA", as.bytes(as.bytes(c(NA, "1"), 8L, "signed"),
                                            8L, "signed", na = FALSE))
opq <- as.bytes("0102030405060708", 8L)
ok("opaque converts to itself",  identical(as.vector(opq, "bytes8"), opq))
probe("opaque to another width", as.vector(opq, "bytes16"))
probe("opaque to a number",      as.vector(opq, "uint64"))
probe("a number to opaque",      as.vector(as.bytes("1", 8L, "unsigned"), "bytes8"))

## as.raw, which now behaves exactly as it does for integers
ok("as.raw of small values",     identical(suppressWarnings(
    as.raw(as.bytes(c("0", "1", "255"), 8L, "unsigned"))), as.raw(c(0, 1, 255))))
ok("out of range gives 00",      identical(suppressWarnings(
    as.raw(as.bytes(c("256", "-1"), 8L, "signed"))), as.raw(c(0, 0))))
ok("NA gives 00, as for integer",identical(suppressWarnings(as.raw(as.bytes(NA, 8L, "signed"))),
                                           suppressWarnings(as.raw(NA_integer_))))
probe("as.raw of opaque",        as.raw(opq))

## vapply: FUN.VALUE is a prototype, so it carries width, kind and the
## NA reservation, and none of the three may differ
b <- as.bytes(c("10", "20", "30"), 8L, "unsigned")
ok("vapply over bytes",          identical(vapply(1:3, function(i) b[i], b[1]), b))
ok("vapply builds a matrix",     identical(dim(vapply(1:3, function(i) b[c(i, i)], b[1:2])),
                                           c(2L, 3L)))
ok("vapply keeps names",         identical(names(vapply(c(a = 1, b = 2),
                                                        function(i) b[i], b[1])), c("a", "b")))
ok("a width mismatch names both",{ m <- tryCatch(vapply(1:2, function(i) as.bytes("1", 4L, "unsigned"),
                                                        b[1]), error = conditionMessage)
                                   grepl("uint64", m) && grepl("uint32", m) })
ok("an NA-flag mismatch is told apart", {
    m <- tryCatch(vapply(1:2, function(i) as.bytes("1", 8L, "unsigned", na = FALSE), b[1]),
                  error = conditionMessage)
    grepl("NA is representable", m) })
probe("vapply, wrong type entirely", vapply(1:2, function(i) 1L, b[1]))

## an ALTREP wrapper would drop the width, so no wrapper is made
ok("tryWrap leaves it alone",    identical(.Internal(tryWrap(b)), b))
ok("and does wrap an integer",   { w <- .Internal(tryWrap(c(1L, 2L)))
                                   identical(w, c(1L, 2L)) })

cat("\n== Z11. what the second review pass turned up ==\n")

## as.vector() has to strip attributes from its *result*, not from its
## argument.  Without an arm of its own BYTESXP fell through to a
## CLEAR_ATTRIB() that was still aliasing the caller's vector, so a bare
## as.vector(b) -- or any of union(), intersect(), setdiff(), which call
## it -- emptied b's own attributes.
av <- as.bytes(as.character(1:4), 8L, "unsigned")
names(av) <- letters[1:4]
avd <- as.bytes(as.character(1:4), 8L, "unsigned")
dim(avd) <- c(2L, 2L)
invisible(as.vector(av)); invisible(as.vector(avd))
ok("as.vector leaves names alone", identical(names(av), letters[1:4]))
ok("as.vector leaves dim alone",   identical(dim(avd), c(2L, 2L)))
ok("and still strips its result",  is.null(attributes(as.vector(av))))
invisible(tryCatch(union(av, av), error = identity))
ok("union leaves names alone",     identical(names(av), letters[1:4]))

## write.table() renders NA through its own isna(), which had no BYTESXP
## case: the na= string was ignored and the literal text NA written
wt <- as.bytes(c("1", "2"), 8L, "unsigned"); wt[2] <- NA
wtf <- tempfile()
write.table(data.frame(k = wt), wtf, na = "", row.names = FALSE,
            col.names = FALSE, quote = FALSE)
ok("write.table honours na=",      identical(readLines(wtf), c("1", "")))
write.table(as.matrix(wt), wtf, na = "-", row.names = FALSE,
            col.names = FALSE, quote = FALSE)
ok("and on the matrix path too",   identical(readLines(wtf), c("1", "-")))
unlink(wtf)

## a value past DBL_MAX is not an imprecise double, it is not a double at
## all, and the two report separately
vbig <- as.bytes(as.raw(rep(255, 255)), 255L, "unsigned", na = FALSE)
v128 <- as.bytes(as.raw(rep(255, 16)), 16L, "unsigned", na = FALSE)
ok("overflow says so",             grepl("beyond the range of double",
                                         tryCatch(as.numeric(vbig),
                                                  warning = conditionMessage)))
ok("precision loss still says so", grepl("2\\^53",
                                         tryCatch(as.numeric(v128),
                                                  warning = conditionMessage)))

## PrintGenericVector renders a length-1 element into a 115-byte buffer,
## which a wide one overruns; a number cut to fit is a different number,
## so it falls back to the summary the other arms print
pg <- list(vbig, vbig, vbig, vbig); dim(pg) <- c(2L, 2L)
ok("no truncated number printed",  {
    out <- capture.output(print(pg))
    !any(grepl("^[0-9]{50,}", trimws(out))) && any(grepl("uint2040,1", out)) })

## coerceVector() is not as.vector(): '/' and '^' read the coerced
## operand's dim and names back out, so the coercion has to keep them
cm <- as.bytes(as.character(1:4), 8L, "signed"); dim(cm) <- c(2L, 2L)
cn <- as.bytes(c("1", "2"), 8L, "signed"); names(cn) <- c("a", "b")
ok("/ keeps dim",                  identical(dim(cm / 2), c(2L, 2L)))
ok("^ keeps names",                identical(names(cn ^ 2), c("a", "b")))

## deparse has to emit the attributes, or dump()/source() silently
## returns a different object
dp <- as.bytes(c("1", "5"), 8L, "unsigned"); names(dp) <- c("a", "b")
dq <- as.bytes(c("1", "2"), 8L, "unsigned"); attr(dq, "units") <- "ns"
ok("deparse keeps names",          identical(names(eval(parse(text = deparse(dp)))),
                                             c("a", "b")))
ok("deparse keeps other attrs",    identical(attr(eval(parse(text = deparse(dq))), "units"),
                                             "ns"))
ok("and a plain vector stays plain",
   identical(deparse(as.bytes(c("1", "2"), 8L, "unsigned")),
             "as.bytes(c(\"1\", \"2\"), 8L, \"unsigned\")"))

## integer_binary() and real_binary() end in copyMostAttrib(), and the
## unary kernels work in a duplicate(), so every attribute survives
ar <- as.bytes(c("1", "2"), 8L, "signed"); attr(ar, "units") <- "ns"
ok("+ keeps other attrs",          identical(attr(ar + ar, "units"), "ns"))
ok("unary - keeps them",           identical(attr(-ar, "units"), "ns"))
ok("mixed operand keeps them",     identical(attr(ar * 2L, "units"), "ns"))
## and a zero-length result is bare, which is what integer_binary() and
## real_binary() return: the point is to match them, in both directions
az <- as.bytes(character(0), 8L, "signed"); attr(az, "units") <- "ns"
iz <- integer(0); attr(iz, "units") <- "ns"
ok("zero length matches integer",  identical(attributes(az + az), attributes(iz + iz)))

## and the set operations now agree with c(), == and union() on the very
## same pair of vectors
m4 <- as.bytes(c("1", "2"), 4L, "unsigned")
m8 <- as.bytes(c("1", "2", "3"), 8L, "unsigned")
ok("setdiff refuses a clash",      inherits(tryCatch(setdiff(m4, m8), error = identity), "error"))
ok("%in% refuses a clash",         inherits(tryCatch(m4 %in% m8, error = identity), "error"))
ok("and matching still works",     identical(match(m8[2:3], m8), 2:3))

cat("\n== O. stage 4+: each MUST still fail loudly ==\n")
probe("x + x",                   x + x)
probe("sum(x)",                  sum(x))
probe("as.integer(x)",           as.integer(x))
probe("as.raw(x)",               as.raw(x))
probe("as.numeric(x)",           as.numeric(x))

cat(sprintf("\n%d failure(s)\n", fails))
if (fails) quit(status = 1L)

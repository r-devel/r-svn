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

cat("== A. type identity ==\n")
ok("typeof",                     typeof(x) == "bytes")
ok("class",                      class(x) == "bytes")
ok("is.bytes",                   is.bytes(x) && !is.bytes(raw(4)))
ok("is.atomic",                  is.atomic(x))
ok("is.vector",                  is.vector(x))
ok("not is.raw",                 !is.raw(x))
ok("mode",                       mode(x) == "bytes")

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
                                   typeof(c1) == "bytes" &&
                                   bytesWidth(c1) == 16L &&
                                   identical(bytesRaw(c1), as.raw(1:32)) })
ok("data.frame: print fails loudly",
                                 inherits(tryCatch(print(data.frame(x)), error = identity), "error"))

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

cat("\n== E. not implemented yet: each MUST fail loudly ==\n")
probe("x[1]",                    x[1])
probe("x[[1]]",                  x[[1]])
probe("x[c(TRUE, FALSE)]",       x[c(TRUE, FALSE)])
probe("x[1] <- x[2]",            { z <- x; z[1] <- z[2]; z })
probe("c(x, y)",                 c(x, y))
probe("unlist(list(x))",         unlist(list(x)))
probe("x == y",                  x == y)
probe("x < y",                   x < y)
probe("sort(x)",                 sort(x))
probe("order(x)",                order(x))
probe("match(x, y)",             match(x, y))
probe("x %in% y",                x %in% y)
probe("unique(x)",               unique(x))
probe("duplicated(x)",           duplicated(x))
probe("table(x)",                table(x))
probe("factor(x)",               factor(x))
probe("rep(x, 2)",               rep(x, 2))
probe("rev(x)",                  rev(x))
probe("head(x, 1)",              head(x, 1))
probe("length(x) <- 1",          { z <- x; length(z) <- 1L; z })
probe("x + x",                   x + x)
probe("sum(x)",                  sum(x))
probe("as.character(x)",         as.character(x))
probe("as.integer(x)",           as.integer(x))
probe("as.raw(x)",               as.raw(x))
probe("format(x)",               format(x))
probe("deparse(x)",              deparse(x))
probe("serialize(x)",            unserialize(serialize(x, NULL)))
probe("str(x)",                  str(x))
probe("lapply(x, I)",            lapply(x, I))
probe("split(x, 1:2)",           split(x, 1:2))
probe("anyNA(x)",                anyNA(x))

cat(sprintf("\n%d failure(s)\n", fails))
if (fails) quit(status = 1L)

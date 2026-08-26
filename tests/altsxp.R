## Tests for ALTSXP, and for the int64 / uint64 classes built on it.
##
## Nothing here gives an opaque vector a class attribute: everything is
## handled by the ALTSXP method table, so these also exercise the generic
## machinery in altrep.c, bind.c, subset.c, subassign.c and unique.c.

assertError <- function(...) tools::assertError(..., verbose = TRUE)
assertWarning <- function(...) tools::assertWarning(..., verbose = TRUE)

## --- construction and identity ---------------------------------------

x <- as.int64(1:5)
stopifnot(typeof(x) == "int64",
          class(x) == "int64",
          mode(x) == "int64",
          length(x) == 5L,
          is.numeric(x),
          is.atomic(x),
          !is.integer(x),
          !is.double(x),
          is.int64(x),
          !is.uint64(x))

u <- as.uint64(1:5)
stopifnot(typeof(u) == "uint64", is.uint64(u), !is.int64(u))

stopifnot(length(int64()) == 0L, typeof(int64()) == "int64",
          length(uint64(3L)) == 3L, typeof(uint64(3L)) == "uint64",
          identical(as.double(int64(3L)), c(0, 0, 0)))

stopifnot(identical(x, x), identical(as.int64(1:5), x),
          !identical(x, as.uint64(1:5)),
          !identical(x, 1:5))

## --- conversion in ---------------------------------------------------

stopifnot(identical(as.double(as.int64(c(TRUE, FALSE, NA))), c(1, 0, NA)),
          identical(as.double(as.int64(as.raw(c(1, 255)))), c(1, 255)),
          identical(as.double(as.int64(c(1.9, -1.9))), c(1, -1)),
          identical(as.character(as.int64("9223372036854775807")),
                    "9223372036854775807"),
          identical(as.character(as.int64("-9223372036854775807")),
                    "-9223372036854775807"),
          identical(as.character(as.uint64("18446744073709551614")),
                    "18446744073709551614"))

## the whole point: a value no double can hold survives the round trip
big <- as.int64("4611686018427387904")
stopifnot(as.character(big) == "4611686018427387904",
          as.character(big + as.int64(1L)) == "4611686018427387905",
          ## the two are distinct, but a double cannot tell them apart
          as.double(big) == as.double(big + as.int64(1L)),
          big != big + as.int64(1L))

assertWarning(as.int64("not a number"))
assertWarning(as.uint64(-1L))              # negative into an unsigned vector
assertWarning(as.int64(1e300))             # out of the 64-bit range
stopifnot(is.na(suppressWarnings(as.int64("not a number"))),
          is.na(suppressWarnings(as.uint64(-1L))))

## --- conversion out --------------------------------------------------

stopifnot(identical(as.double(x), as.double(1:5)),
          identical(as.integer(x), 1:5),
          identical(as.logical(as.int64(c(0, 2))), c(FALSE, TRUE)),
          identical(as.character(x), as.character(1:5)),
          identical(as.complex(as.int64(1:2)), as.complex(1:2)))

assertWarning(as.integer(as.int64("4611686018427387904")))

## --- NA --------------------------------------------------------------

xn <- c(x, NA)
stopifnot(typeof(xn) == "int64", length(xn) == 6L,
          identical(is.na(xn), c(rep(FALSE, 5), TRUE)),
          anyNA(xn), !anyNA(x),
          is.na(sum(xn)), sum(xn, na.rm = TRUE) == as.int64(15L))

## growing introduces NA, and so does an out-of-bounds subscript
y <- x
y[8L] <- as.int64(9L)
stopifnot(length(y) == 8L, identical(is.na(y), c(rep(FALSE, 5), TRUE, TRUE, FALSE)),
          is.na(x[99L]))

## --- the NA domain is a property of the object -----------------------

## na = FALSE gives up the sentinel and gains the whole 64-bit range
n <- as.int64(c("-9223372036854775808", "9223372036854775807"), na = FALSE)
stopifnot(typeof(n) == "int64",
          identical(is.na(n), c(FALSE, FALSE)),
          !anyNA(n),
          as.character(n) == c("-9223372036854775808", "9223372036854775807"),
          as.character(sum(n)) == "-1")

## a nullable vector cannot hold that value at all
assertWarning(as.int64("-9223372036854775808"))
stopifnot(is.na(suppressWarnings(as.int64("-9223372036854775808"))))

## and R refuses to move it into a context that must be able to be NA
assertError(c(n, as.int64(1L)))
assertError(n[5L])
assertError({ nn <- n; nn[1L] <- NA; nn })

## the same value is fine once it is not the sentinel
m <- as.int64(c("-9223372036854775807", "1"), na = FALSE)
stopifnot(length(c(m, as.int64(1L))) == 3L)

## --- arithmetic ------------------------------------------------------

stopifnot(identical(as.double(x + x), as.double(2 * 1:5)),
          identical(as.double(x - x), rep(0, 5)),
          identical(as.double(x * x), as.double((1:5)^2)),
          identical(as.double(-x), -as.double(1:5)),
          identical(as.double(+x), as.double(1:5)))

## other integer types promote to int64.  Raw is not among them: it is an
## exact integer type and c() takes one (see the promotion ladder below),
## but base R does not admit raw to arithmetic at all, so neither does this.
for (e in list(3L, TRUE))
    stopifnot(typeof(x + e) == "int64", typeof(e + x) == "int64")

## a double operand promotes the whole operation to double
stopifnot(typeof(x + 1.5) == "double",
          typeof(1.5 + x) == "double",
          identical(x + 1.5, 1:5 + 1.5))

## as do division and exponentiation, which leave the integers anyway
stopifnot(typeof(x / as.int64(2L)) == "double",
          identical(x / 2L, 1:5 / 2L),
          typeof(x ^ as.int64(2L)) == "double",
          identical(x ^ 2L, (1:5) ^ 2L))

## integer division and remainder stay exact
stopifnot(identical(as.double(as.int64(7L) %/% as.int64(2L)), 3),
          identical(as.double(as.int64(-7L) %/% as.int64(2L)), -4),
          identical(as.double(as.int64(-7L) %% as.int64(2L)), 1),
          identical(as.double(as.int64(7L) %% as.int64(-2L)), -1))

## recycling, and the shorter operand's attributes
stopifnot(identical(as.double(x + as.int64(1L)), as.double(2:6)),
          length(x + as.int64(1:5)) == 5L)

## overflow becomes NA, with a warning
assertWarning(as.int64("9223372036854775807") + as.int64(1L))
stopifnot(is.na(suppressWarnings(as.int64("9223372036854775807") + as.int64(1L))))

## but a vector with no NA in its domain must report it instead
assertError(as.int64("9223372036854775807", na = FALSE) +
            as.int64(1L, na = FALSE))

## int64 and uint64 do not share a representation
assertError(as.int64(1L) + as.uint64(1L))
assertError(as.int64(1L) < as.uint64(1L))
assertError(-as.uint64(1L))

## unsigned arithmetic uses the top half of the range
stopifnot(as.character(as.uint64("9223372036854775808") + as.uint64(1L)) ==
          "9223372036854775809")
assertWarning(as.uint64(0L) - as.uint64(1L))

## --- comparison ------------------------------------------------------

stopifnot(identical(x > as.int64(3L), 1:5 > 3L),
          identical(x == as.int64(3L), 1:5 == 3L),
          identical(x != 3L, 1:5 != 3L),
          identical(x <= 2.5, 1:5 <= 2.5),
          identical(as.int64(1L) < 1.5, TRUE),
          identical(is.na(as.int64(c(1, NA)) == as.int64(1L)), c(FALSE, TRUE)))

## unsigned comparison is not the signed comparison of the same bits
big2 <- as.uint64("18446744073709551614")
stopifnot(big2 > as.uint64(1L))

## --- the Math group --------------------------------------------------

stopifnot(identical(as.double(abs(as.int64(c(-2, 3)))), c(2, 3)),
          identical(as.double(sign(as.int64(c(-2, 0, 3)))), c(-1, 0, 1)),
          identical(as.double(cumsum(x)), cumsum(as.double(1:5))),
          identical(as.double(cummax(as.int64(c(1, 3, 2)))), c(1, 3, 3)),
          identical(as.double(cummin(as.int64(c(3, 1, 2)))), c(3, 1, 1)),
          identical(round(x), x), identical(floor(x), x),
          identical(ceiling(x), x), identical(trunc(x), x),
          identical(signif(x, 1), x))

## --- reductions ------------------------------------------------------

stopifnot(sum(x) == as.int64(15L), min(x) == as.int64(1L),
          max(x) == as.int64(5L),
          identical(as.double(range(x)), c(1, 5)),
          typeof(sum(x)) == "int64", typeof(min(x)) == "int64",
          typeof(range(x)) == "int64",
          sum(int64()) == as.int64(0L))

## sum() is exact where a double would round
s <- sum(as.int64(c("4611686018427387903", "1")))
stopifnot(as.character(s) == "4611686018427387904",
          as.double(s) == as.double(sum(as.int64("4611686018427387904"))))

## mean() is a double: the class says what its elements are worth
stopifnot(identical(mean(x), 3))

## several arguments reduce together, following the same promotion ladder
a1 <- as.int64(c("4611686018427387903", "1"))
b1 <- as.int64(c("1", "2"))
stopifnot(typeof(sum(a1, b1)) == "int64",
          as.character(sum(a1, b1)) == "4611686018427387907",
          typeof(sum(a1, 1L, TRUE)) == "int64",
          as.character(sum(a1, 1L, TRUE)) == "4611686018427387906",
          ## a double argument promotes the whole reduction
          identical(sum(as.int64(1:3), 1.5), 7.5),
          typeof(min(a1, b1)) == "int64",
          as.character(min(a1, b1)) == "1",
          as.character(max(a1, b1)) == "4611686018427387903",
          typeof(range(a1, b1)) == "int64",
          as.character(range(a1, b1)) == c("1", "4611686018427387903"),
          typeof(sum(as.uint64(1:3), as.uint64(4L))) == "uint64")

## NA and na.rm carry across the arguments
stopifnot(is.na(sum(as.int64(c(1, NA)), as.int64(2))),
          as.character(sum(as.int64(c(1, NA)), as.int64(2), na.rm = TRUE)) == "3")

## two opaque element types have nothing in common to reduce over
assertError(sum(as.int64(1L), as.uint64(1L)))

## and the ordinary types are untouched
stopifnot(identical(sum(1:3, 4L), 10L), identical(sum(1:3, 4.5), 10.5),
          identical(min(3L, 1L, 2L), 1L), identical(range(3L, 1L), c(1L, 3L)),
          identical(sum(), 0L))

## --- ordering, matching, tabulating ----------------------------------

z <- as.int64(c(3, 1, 2, 1))
stopifnot(identical(as.double(sort(z)), c(1, 1, 2, 3)),
          identical(order(z), order(c(3, 1, 2, 1))),
          identical(rank(z), rank(c(3, 1, 2, 1))),
          identical(as.double(rev(z)), c(1, 2, 1, 3)),
          identical(as.double(unique(z)), c(3, 1, 2)),
          identical(duplicated(z), c(FALSE, FALSE, FALSE, TRUE)),
          identical(match(as.int64(2L), z), 3L),
          identical(match(z, as.int64(1:3)), c(3L, 1L, 2L, 1L)),
          identical(as.int64(2L) %in% z, TRUE),
          identical(as.vector(table(z)), c(2L, 1L, 1L)),
          !is.unsorted(sort(z)))

## exact ordering, where a double comparison would tie
o <- as.int64(c("4611686018427387904", "4611686018427387905"))
stopifnot(identical(order(o), 1:2), is.unsorted(rev(o)))

## --- subsetting and subassignment ------------------------------------

stopifnot(identical(as.double(x[2:3]), c(2, 3)),
          identical(as.double(x[-1L]), c(2, 3, 4, 5)),
          identical(as.double(x[c(TRUE, FALSE)]), c(1, 3, 5)),
          typeof(x[[2L]]) == "int64",
          as.double(x[[2L]]) == 2,
          identical(as.double(head(x, 2L)), c(1, 2)),
          identical(as.double(rep(as.int64(1:2), 2L)), c(1, 2, 1, 2)))

y <- x
y[1L] <- 100L
stopifnot(typeof(y) == "int64", as.double(y[1L]) == 100)
y[2L] <- as.int64(200L)
stopifnot(as.double(y[2L]) == 200)

## names and other attributes survive
v <- x
names(v) <- letters[1:5]
stopifnot(identical(names(v), letters[1:5]),
          identical(names(v[2:3]), c("b", "c")))

## --- c() and the promotion ladder ------------------------------------

## an opaque vector subsumes the integer types ...
stopifnot(typeof(c(x, 6L)) == "int64",
          typeof(c(6L, x)) == "int64",
          typeof(c(x, TRUE)) == "int64",
          typeof(c(x, as.raw(1))) == "int64",
          identical(as.double(c(x, 6L)), as.double(1:6)))

## ... and yields to double, complex and character
stopifnot(typeof(c(x, 6.5)) == "double",
          identical(c(x, 6.5), c(as.double(1:5), 6.5)),
          typeof(c(x, 1i)) == "complex",
          typeof(c(x, "a")) == "character",
          identical(c(as.int64(1:2), "a"), c("1", "2", "a")))

## two opaque element types have no common representation, so c() lists them
cc <- c(as.int64(1:2), as.uint64(1:2))
stopifnot(is.list(cc), length(cc) == 4L,
          typeof(cc[[1L]]) == "int64", typeof(cc[[3L]]) == "uint64")

stopifnot(identical(as.double(unlist(list(as.int64(1:2), 3L))),
                    as.double(1:3)),
          typeof(unlist(list(as.int64(1:2), 3L))) == "int64")

## --- matrices and arrays ---------------------------------------------

M <- matrix(as.int64(1:6), 2L, 3L)
stopifnot(typeof(M) == "int64", identical(dim(M), c(2L, 3L)),
          identical(as.double(M[1L, ]), c(1, 3, 5)),
          identical(as.double(M[, 2L]), c(3, 4)),
          identical(as.double(M[1L, 2L]), 3),
          identical(as.double(M[cbind(1L, 2L)]), 3),
          identical(dim(t(M)), c(3L, 2L)),
          identical(as.double(t(M)), c(1, 3, 5, 2, 4, 6)),
          identical(dim(aperm(M, c(2L, 1L))), c(3L, 2L)),
          identical(as.double(diag(M)), c(1, 4)),
          identical(dim(format(M)), c(2L, 3L)),
          identical(dim(M + 1L), c(2L, 3L)),
          identical(dim(M == M), c(2L, 3L)),
          sum(M) == as.int64(21L))

dimnames(M) <- list(c("a", "b"), c("x", "y", "z"))
stopifnot(identical(as.double(M["a", ]), c(1, 3, 5)),
          identical(dimnames(t(M)), list(c("x", "y", "z"), c("a", "b"))))

A <- array(as.int64(1:8), c(2L, 2L, 2L))
stopifnot(typeof(A) == "int64", identical(dim(A), c(2L, 2L, 2L)),
          identical(as.double(A[, , 2L]), c(5, 6, 7, 8)))

stopifnot(identical(dim(cbind(as.int64(1:2), as.int64(3:4))), c(2L, 2L)),
          typeof(cbind(as.int64(1:2), as.int64(3:4))) == "int64",
          identical(dim(rbind(as.int64(1:2), as.int64(3:4))), c(2L, 2L)),
          identical(as.double(cbind(as.int64(1:2), as.int64(3:4))),
                    c(1, 2, 3, 4)))

## an opaque column mixed with a double one is bound as doubles
stopifnot(typeof(cbind(as.int64(1:2), c(1.5, 2.5))) == "double")

## --- printing and formatting -----------------------------------------

stopifnot(identical(format(as.int64(c(1, 100, 20))), c("  1", "100", " 20")),
          identical(format(as.int64(c(1, 100, 20)), trim = TRUE),
                    c("1", "100", "20")),
          identical(format(as.int64(c(1, NA))), c(" 1", "NA")),
          identical(as.character(as.int64(c(1, NA))), c("1", NA)),
          identical(paste(as.int64(1:2), collapse = ","), "1,2"))

out <- capture.output(print(M))
stopifnot(length(out) > 1L, any(grepl("int64", out)))

## --- serialization ---------------------------------------------------

for (val in list(x, u, n, M, as.int64(character())))
    stopifnot(identical(unserialize(serialize(val, NULL)), val))

## the NA domain is part of the object, so it has to survive a round trip
n2 <- unserialize(serialize(n, NULL))
stopifnot(identical(is.na(n2), c(FALSE, FALSE)),
          as.character(n2) == as.character(n))
assertError(c(n2, as.int64(1L)))

f <- tempfile()
saveRDS(x, f)
stopifnot(identical(readRDS(f), x))
unlink(f)

## --- copying semantics -----------------------------------------------

a <- as.int64(1:3)
b <- a
b[1L] <- 99L
stopifnot(as.double(a[1L]) == 1, as.double(b[1L]) == 99)

stopifnot(identical(duplicated(list(x, x)), c(FALSE, TRUE)))

## --- higher-order and data.frame use ---------------------------------

stopifnot(identical(lengths(split(x, c(1, 1, 2, 2, 2))), c("1" = 2L, "2" = 3L)),
          identical(as.double(unlist(as.list(as.int64(1:3)))), as.double(1:3)),
          identical(vapply(as.list(as.int64(1:3)), function(e) typeof(e), ""),
                    rep("int64", 3L)))

df <- data.frame(i = as.int64(1:3), d = 1:3)
stopifnot(nrow(df) == 3L, typeof(df$i) == "int64",
          identical(as.double(df$i[2:3]), c(2, 3)),
          identical(as.double(df[df$d > 1L, "i"]), c(2, 3)))

## --- element type is shared across classes ---------------------------

## int64 and uint64 are different classes with different element types;
## two objects of the same element type interoperate whatever produced them
stopifnot(typeof(as.int64(1L)) == typeof(as.int64(1L, na = FALSE)),
          typeof(c(as.int64(1L), as.int64(2L, na = FALSE))) == "int64")

## --- binary ingest: readBin() and writeBin() -------------------------

## the prototype is what names the type: an opaque vector cannot be built
## from a type name alone
bin <- writeBin(x, raw())
stopifnot(length(bin) == 5L * 8L,
          identical(readBin(bin, int64(), 5L), x),
          identical(readBin(bin, "int64", 5L), x))

## a short read gives what was there
stopifnot(identical(readBin(bin, int64(), 99L), x),
          identical(as.double(readBin(bin, int64(), 2L)), c(1, 2)))

## values no double could carry, through a file connection
f <- tempfile()
big <- as.int64(c("-9223372036854775807", "0", "9223372036854775807"))
con <- file(f, "wb"); writeBin(big, con); close(con)
con <- file(f, "rb"); back <- readBin(con, int64(), 3L); close(con)
stopifnot(identical(back, big), file.size(f) == 24)
unlink(f)

## byte order round trips, and reading the wrong way round does not
stopifnot(identical(readBin(writeBin(big, raw(), endian = "big"), int64(), 3L,
                            endian = "big"), big),
          !identical(readBin(writeBin(big, raw(), endian = "big"), int64(), 3L,
                             endian = "little"), big))

## uint64 keeps the top half of the range
ub <- as.uint64(c("0", "18446744073709551614"))
stopifnot(identical(readBin(writeBin(ub, raw()), uint64(), 2L), ub))

## the element width is fixed by the type
assertError(readBin(bin, int64(), 5L, size = 4L))
assertError(writeBin(x, raw(), size = 4L))
assertWarning(readBin(bin, int64(), 1L, signed = FALSE))

## --- text ingest: scan() ---------------------------------------------

tf <- tempfile()
writeLines(c("4611686018427387904", "1", "NA", "-9223372036854775807"), tf)
sc <- scan(tf, what = int64(), quiet = TRUE)
stopifnot(typeof(sc) == "int64", length(sc) == 4L,
          identical(is.na(sc), c(FALSE, FALSE, TRUE, FALSE)),
          as.character(sc)[1L] == "4611686018427387904")
unlink(tf)

## a multi-column record, opaque and ordinary side by side
tf <- tempfile()
writeLines(c("9007199254740992 a", "9007199254740993 b"), tf)
fr <- scan(tf, what = list(int64(), ""), quiet = TRUE)
stopifnot(typeof(fr[[1L]]) == "int64", typeof(fr[[2L]]) == "character",
          as.character(fr[[1L]]) == c("9007199254740992", "9007199254740993"),
          as.double(fr[[1L]][2L] - fr[[1L]][1L]) == 1)
unlink(tf)

## and through read.table()'s colClasses
tf <- tempfile()
writeLines(c("id,val", "9007199254740992,a", "9007199254740993,b"), tf)
df2 <- utils::read.csv(tf, colClasses = c("int64", "character"))
stopifnot(nrow(df2) == 2L, typeof(df2$id) == "int64",
          as.character(df2$id) == c("9007199254740992", "9007199254740993"),
          df2$id[1L] != df2$id[2L],
          ## the same file read as doubles cannot tell the two apart:
          ## 2^53 + 1 has no double
          identical(utils::read.csv(tf)$id, c(9007199254740992, 9007199254740992)))
stopifnot(any(grepl("int64", utils::capture.output(utils::str(df2)))))
unlink(tf)

## --- sorting and the summaries built on it ---------------------------

z2 <- as.int64(c(3, 1, NA, 2))
stopifnot(identical(as.double(sort(z2)), c(1, 2, 3)),
          identical(is.na(sort(z2, na.last = TRUE)), c(FALSE, FALSE, FALSE, TRUE)),
          identical(as.double(sort(z2, decreasing = TRUE)), c(3, 2, 1)),
          as.double(median(as.int64(c(5, 1, 3)))) == 3)

## sorting is not quadratic: this is instant with an ordering sort and
## minutes without one
zz <- as.int64(rev(seq_len(20000L)))
stopifnot(!is.unsorted(sort(zz)),
          identical(order(zz), order(rev(seq_len(20000L)))))


## --- regression tests ------------------------------------------------
##
## Each block below is a bug that the generic machinery had: they are
## grouped by what they exercise rather than by where the fix landed.

## rbind() lays out rows, not columns.  The ALTSXP arm started as a copy of
## the cbind() one, which transposed a two-argument result and wrote past
## the end of a three-argument one.
stopifnot(identical(as.character(rbind(as.int64(1:2), as.int64(3:4))),
                    as.character(rbind(1:2, 3:4))),
          identical(as.character(cbind(as.int64(1:2), as.int64(3:4))),
                    as.character(cbind(1:2, 3:4))),
          identical(as.character(rbind(as.int64(1:2), as.int64(1:2),
                                       as.int64(1:2))),
                    as.character(rbind(1:2, 1:2, 1:2))),
          ## recycling, and a matrix argument alongside a vector one
          identical(as.character(rbind(as.int64(1:6), as.int64(1:2))),
                    as.character(rbind(1:6, 1:2))),
          identical(as.character(rbind(matrix(as.int64(1:6), 2L, 3L),
                                       as.int64(7:9))),
                    as.character(rbind(matrix(1:6, 2L, 3L), 7:9))),
          identical(as.character(cbind(matrix(as.int64(1:6), 2L, 3L),
                                       as.int64(7:8))),
                    as.character(cbind(matrix(1:6, 2L, 3L), 7:8))))

## as.vector() drops attributes from a copy, never from the caller's object
local({
    v <- as.int64(1:3)
    names(v) <- c("a", "b", "c")
    stopifnot(is.null(names(as.vector(v))),
              identical(names(v), c("a", "b", "c")))
    m <- matrix(as.int64(1:6), 2L, 3L)
    stopifnot(is.null(dim(as.vector(m))), identical(dim(m), c(2L, 3L)))
})

## the results of Na_widen(), Coerce_from() and Format() are freshly
## allocated and were held across a later allocation
local({
    gctorture(TRUE)
    on.exit(gctorture(FALSE))
    w <- as.int64(1:3, na = FALSE)
    length(w) <- 5L
    stopifnot(identical(is.na(w), c(FALSE, FALSE, FALSE, TRUE, TRUE)))
    v <- 1:3
    v[5L] <- as.int64(9L, na = FALSE)
    stopifnot(typeof(v) == "int64", as.double(v[5L]) == 9)
    stopifnot(length(capture.output(print(as.int64(1:3)))) > 1L)
})

## --- the NA domain belongs to the object, not to the pair ------------

## Reading an operand in the *other* operand's domain turned a whole-range
## vector's extreme value into NA.  Comparison reads each side in its own
## domain; arithmetic has a result domain to settle, so it widens first and
## reports the clash the way c() does.
local({
    nn <- as.int64(c("-9223372036854775808", "9223372036854775807"),
                   na = FALSE)
    stopifnot(identical(nn < as.int64(0L), c(TRUE, FALSE)),
              identical(nn == nn, c(TRUE, TRUE)),
              identical(is.na(nn), c(FALSE, FALSE)))
    assertError(nn + as.int64(0L))

    ## no clash, so the values come through untouched
    mm <- as.int64(c("-9223372036854775807", "5"), na = FALSE)
    stopifnot(identical(as.character(mm + as.int64(0L)), as.character(mm)))
})

## A reduction keeps its input's NA domain: a whole-range vector must be
## able to report its own extreme value, and in exchange an overflow there
## is an error rather than a silent NA.
local({
    nn <- as.int64(c("-9223372036854775808", "9223372036854775807"),
                   na = FALSE)
    stopifnot(!is.na(min(nn)), !is.na(max(nn)),
              as.character(min(nn)) == "-9223372036854775808",
              as.character(max(nn)) == "9223372036854775807")

    qq <- as.int64(c("-9223372036854775807", "-1"), na = FALSE)
    stopifnot(as.character(sum(qq)) == "-9223372036854775808",
              identical(as.character(cumsum(qq)),
                        c("-9223372036854775807", "-9223372036854775808")))
    assertError(sum(as.int64(c("-9223372036854775807", "-2"), na = FALSE)))

    ## a nullable vector still degrades to NA, with a warning
    assertWarning(sum(as.int64(c("9223372036854775807", "1"))))
    stopifnot(is.na(suppressWarnings(sum(as.int64(c("9223372036854775807",
                                                    "1"))))))
})

## an NA cannot be laundered into a vector that has no NA
assertError(as.int64(as.int64(NA_integer_), na = FALSE))
assertError(as.uint64(as.uint64(NA_integer_), na = FALSE))

## identical() compares the domain too: the same bytes mean different
## things in a vector that reserves a value for NA and one that does not
local({
    aa <- as.int64("-9223372036854775808", na = FALSE)
    bb <- suppressWarnings(as.int64("-9223372036854775808"))
    stopifnot(!identical(aa, bb), is.na(bb), !is.na(aa),
              identical(aa, as.int64("-9223372036854775808", na = FALSE)))
})

## --- the operator machinery around the class hook ---------------------

## The hook sits inside R_binary()/do_relop_dflt() rather than in front of
## them, so the shape rules apply to an opaque vector as to any other.
assertError(matrix(as.int64(1:6), 2L, 3L) + matrix(as.int64(1:6), 3L, 2L))
assertError(matrix(as.int64(1:6), 2L, 3L) == matrix(as.int64(1:6), 3L, 2L))
assertWarning(as.int64(1:3) + as.int64(1:2))
assertWarning(as.int64(1:3) < as.int64(1:2))
stopifnot(identical(attributes(structure(as.int64(1:3), units = "m") +
                               as.int64(1L)),
                    list(units = "m")),
          identical(names(c(a = as.int64(1L), b = as.int64(2L)) + as.int64(1L)),
                    c("a", "b")),
          identical(dim(matrix(as.int64(1:6), 2L, 3L) + as.int64(1L)),
                    c(2L, 3L)))

## a Math-group method on a classed opaque vector wins over the class hook,
## for every member of the group
Math.money <- function(x, ...) "dispatched"
local({
    money <- structure(as.int64(1:3), class = "money")
    for (f in c("floor", "ceiling", "abs", "sign", "trunc",
                "cumsum", "cummax", "cummin"))
        stopifnot(identical(do.call(f, list(money)), "dispatched"))
    stopifnot(identical(round(money, 1), "dispatched"),
              identical(signif(money, 1), "dispatched"))
})
rm(Math.money)

## and arity is still checked
assertError(abs(as.int64(1L), 2))

## round() and signif() are exact, so they work past the range of a double
stopifnot(as.character(round(as.int64(1234L), -2L)) == "1200",
          as.character(signif(as.int64(1234L), 2L)) == "1200",
          as.character(signif(as.int64(-1234L), 2L)) == "-1200",
          ## halves go to even, as for the base types
          identical(as.character(round(as.int64(c(1250L, 1350L)), -2L)),
                    as.character(round(c(1250, 1350), -2))),
          as.character(round(as.int64("9223372036854775807"), -2L)) ==
              "9223372036854775800",
          ## a non-negative number of digits leaves an integer alone
          identical(as.character(round(as.int64(1234L), 2L)), "1234"))

## --- generic paths that had no ALTSXP arm ----------------------------

stopifnot(identical(which.min(as.int64(c(3, 1, 2))), 2L),
          identical(which.max(as.int64(c(3, 1, 2))), 1L),
          identical(which.min(c(as.int64(c(3, 1)), NA)), 2L),
          ## exact, where going through a double would tie
          identical(which.min(as.int64(c("9223372036854775807",
                                         "9223372036854775806"))), 2L))

## order() with more than one key uses a different comparator
stopifnot(identical(order(as.int64(c(3, 1, 2)), c("a", "b", "c")),
                    order(c(3, 1, 2), c("a", "b", "c"))),
          identical(order(c("a", "a", "b"), as.int64(c(3, 1, 2))),
                    order(c("a", "a", "b"), c(3, 1, 2))))

## a class attribute must not send an opaque vector to the radix sort
local({
    money <- structure(as.int64(c(3, 1, 2)), class = "money")
    stopifnot(identical(order(money), c(2L, 3L, 1L)),
              identical(sort.list(money), c(2L, 3L, 1L)),
              identical(as.character(sort.int(money)), c("1", "2", "3")))
})

## rep() in all three of its shapes
stopifnot(identical(as.character(rep.int(as.int64(1:3), c(1L, 2L, 3L))),
                    as.character(rep.int(1:3, c(1L, 2L, 3L)))),
          identical(as.character(rep(as.int64(1:2), length.out = 5L)),
                    as.character(rep(1:2, length.out = 5L))),
          identical(as.character(rep(as.int64(1:2), times = c(2L, 3L))),
                    as.character(rep(1:2, times = c(2L, 3L)))),
          identical(as.character(rep(as.int64(1:2), each = 2L)),
                    as.character(rep(1:2, each = 2L))))

## an out-of-range or NA subscript must not be converted before it is
## tested: the cast is undefined for NA_real_ and for a huge double
stopifnot(is.na(as.int64(1:3)[NA_real_]),
          is.na(as.int64(1:3)[1e300]),
          identical(is.na(matrix(as.int64(1:6), 2L, 3L)[1L, NA]),
                    c(TRUE, TRUE, TRUE)),
          identical(is.na(matrix(int64(), 2L, 2L)), matrix(TRUE, 2L, 2L)),
          identical(is.na(array(int64(), c(2L, 2L))), matrix(TRUE, 2L, 2L)))

## --- assignment ------------------------------------------------------

## widening an ordinary vector keeps its attributes, as coerceVector() does
local({
    m <- matrix(1:4, 2L)
    m[1L, 1L] <- as.int64(9L)
    stopifnot(typeof(m) == "int64", identical(dim(m), c(2L, 2L)))

    v <- c(a = 1L, b = 2L, c = 3L)
    v[2L] <- as.int64(9L)
    stopifnot(typeof(v) == "int64", identical(names(v), c("a", "b", "c")))

    a <- array(1:8, c(2L, 2L, 2L))
    a[1L, 1L, 1L] <- as.int64(9L)
    stopifnot(typeof(a) == "int64", identical(dim(a), c(2L, 2L, 2L)))
})

## subassignment follows the same promotion ladder as c(): an opaque
## element type is wider than the R integer types, narrower than double,
## complex and character, and a list holds it as an element
local({
    for (lhs in list(1:3, c(TRUE, FALSE, NA), as.raw(1:3))) {
        v <- lhs
        v[1L] <- as.int64(9L)
        stopifnot(typeof(v) == "int64", as.double(v[1L]) == 9)
    }

    ## the wider side wins, and keeps its own values intact
    d <- c(1.5, 2.5); d[1L] <- as.int64(9L)
    stopifnot(typeof(d) == "double", identical(d, c(9, 2.5)))
    z <- c(1+1i, 2+0i); z[1L] <- as.int64(9L)
    stopifnot(typeof(z) == "complex")
    ch <- c("a", "b"); ch[1L] <- as.int64(9L)
    stopifnot(typeof(ch) == "character", identical(ch, c("9", "b")))
    ch2 <- c("a", "b"); ch2[[1L]] <- as.int64(9L)
    stopifnot(identical(ch2, c("9", "b")))
    m <- matrix(c(1.5, 2.5, 3.5, 4.5), 2L); m[1L, 1L] <- as.int64(9L)
    stopifnot(typeof(m) == "double", identical(dim(m), c(2L, 2L)))

    ## an opaque vector cannot be allocated from a SEXPTYPE alone
    e <- NULL; e[1L] <- as.int64(9L)
    stopifnot(typeof(e) == "int64", as.double(e) == 9)
})

## an NA subscript names no element, so it needs a single value to recycle
local({
    z <- as.int64(1:5)
    z[c(1L, NA, 3L)] <- as.int64(7L)
    stopifnot(identical(as.character(z), c("7", "2", "7", "4", "5")))
})
assertError({ z <- as.int64(1:5); z[c(1L, NA, 3L)] <- as.int64(c(10, 20, 30)) })

## a list holds an opaque vector as one element rather than absorbing it
local({
    l <- list(1, 2)
    l[1L] <- as.int64(9L)
    stopifnot(typeof(l) == "list", typeof(l[[1L]]) == "int64")
    l[[2L]] <- as.int64(8L)
    stopifnot(typeof(l) == "list", typeof(l[[2L]]) == "int64")
    d <- data.frame(a = 1:3)
    d$i <- as.int64(1:3)
    stopifnot(typeof(d$i) == "int64", nrow(d) == 3L)
})

## --- dispatch, printing and text output ------------------------------

## the implicit class is the element type, so a method can be written for it
print.int64 <- function(x, ...) cat("<my int64>\n")
format.int64 <- function(x, ...) "formatted"
stopifnot(identical(capture.output(print(as.int64(1:3))), "<my int64>"),
          identical(format(as.int64(1:3)), "formatted"))
rm(print.int64, format.int64)
stopifnot(identical(.class2(as.int64(1:3)), c("int64", "numeric")),
          identical(.class2(matrix(as.int64(1:6), 2L, 3L)),
                    c("matrix", "array", "int64", "numeric")))

## cat() and write.table() reach the class rather than an unimplemented type
stopifnot(identical(capture.output(cat(as.int64(1:3))), "1 2 3"),
          identical(capture.output(cat(c(as.int64(1:2), NA))), "1 2 NA"))
local({
    f <- tempfile()
    on.exit(unlink(f))
    d <- data.frame(a = 1:2)
    d$b <- as.int64(c(10, 20))
    write.table(d, f)
    stopifnot(identical(as.double(read.table(f)$b), c(10, 20)))
    write.table(matrix(as.int64(1:4), 2L), f)
    stopifnot(identical(as.double(as.matrix(read.table(f))), as.double(1:4)))
})

## format() honours the arguments that mean something for an exact integer
stopifnot(identical(format(as.int64(1:3), width = 5L), format(1:3, width = 5L)),
          identical(format(as.int64(1:3), trim = TRUE), c("1", "2", "3")))
assertError(format(as.int64(1L), trim = NA))

## deparse() and dput() emit the class's own constructor, so the result is
## code that rebuilds the object -- including its NA domain, which is part
## of what the object is
local({
    for (v in list(as.int64(1:3), as.uint64(1:3), as.int64(integer()),
                   as.int64(c(1L, NA, 3L)), as.int64(c(-5L, 5L)),
                   ## outside the integer range, so written as character
                   as.int64("9223372036854775807"),
                   as.int64(c(1e18, -1e18)),
                   as.uint64("18446744073709551614"),
                   ## INT_MIN would deparse as NA_integer_, so not as integer
                   as.int64(-2147483648),
                   ## the NA domain has to survive
                   as.int64(1:3, na = FALSE),
                   as.uint64("18446744073709551615", na = FALSE),
                   as.int64(c("-9223372036854775808", "9223372036854775807"),
                            na = FALSE),
                   ## and so do the attributes
                   c(a = as.int64(1L), b = as.int64(2L)),
                   structure(as.int64(1:2), units = "m"),
                   matrix(as.int64(1:6), 2L, 3L),
                   structure(as.int64(1:2), class = "money"))) {
        y <- eval(parse(text = paste(deparse(v), collapse = "")))
        stopifnot(identical(v, y))
    }

    stopifnot(identical(capture.output(dput(as.int64(1:3))), "as.int64(1:3)"),
              identical(capture.output(dput(list(a = as.int64(1L)))),
                        "list(a = as.int64(1L))"),
              ## a class with no Deparse method still gets a report of what
              ## it is rather than an error, as an environment does
              identical(capture.output(dput(new.env())), "<environment>"))
})

## --- serialization and matching --------------------------------------

## version 2 has no ALTREP branch, and an opaque vector has no base type to
## fall back to, so say so instead of failing inside the serializer
local({
    f <- tempfile()
    on.exit(unlink(f))
    assertError(saveRDS(as.int64(1:3), f, version = 2))
    saveRDS(as.int64(1:3), f, version = 3)
    stopifnot(identical(readRDS(f), as.int64(1:3)))
})

## a mixed pair compares values, not renderings: 1e18 and
## "1000000000000000000" are the same number but not the same string
stopifnot(as.int64("1000000000000000000") %in% 1e18,
          !(as.int64(1L) %in% 1.5),
          identical(as.int64(1:3) %in% 2L, c(FALSE, TRUE, FALSE)),
          identical(match(NA, c(as.int64(1L), NA)), 2L),
          ## a value the type cannot hold matches nothing
          is.na(suppressWarnings(match(1e300, as.int64(1:3)))),
          ## a character operand still compares as text, as for an integer
          as.int64(1L) %in% "1")

## --- reading text ----------------------------------------------------

## the same leading and trailing whitespace as as.integer()
stopifnot(as.character(as.int64("123 ")) == "123",
          as.character(as.int64("\t42\n")) == "42",
          as.character(as.int64(" -7 ")) == "-7")
## whitespace is not a sign: strtoull() would wrap this to 2^64 - 2
assertWarning(as.uint64("\r-2"))
stopifnot(is.na(suppressWarnings(as.uint64("\r-2"))))
local({
    f <- tempfile()
    on.exit(unlink(f))
    writeLines(c("id", "123 ", " 456"), f)
    stopifnot(identical(as.character(read.csv(f, colClasses = "int64")$id),
                        c("123", "456")))
})

## --- review round: one block per bug ---------------------------------

## x[i] <- v follows the same promotion ladder as c(): the opaque type
## subsumes the R integer types but loses to double, complex, character and
## list, so the *left* side widens.  It used to coerce the RHS into the
## opaque type unconditionally and silently drop a fraction.
local({
    v <- as.int64(1:3); v[1] <- 1.5
    stopifnot(typeof(v) == "double", identical(v, c(1.5, 2, 3)))
    v <- as.int64(1:3); v[[1]] <- 1.5
    stopifnot(typeof(v) == "double")
    v <- as.int64(1:3); v[1] <- 1i
    stopifnot(typeof(v) == "complex")
    v <- as.int64(1:3); v[1] <- "a"
    stopifnot(typeof(v) == "character")
    v <- as.int64(1:3); v[[1]] <- list(1)
    stopifnot(typeof(v) == "list")
    m <- matrix(as.int64(1:4), 2L); m[1, 1] <- 1.5
    stopifnot(typeof(m) == "double", identical(dim(m), c(2L, 2L)))
    ## and quantile(), which assigns a double into its working vector
    stopifnot(identical(unname(quantile(as.int64(c(3, 1, 2)))),
                        unname(quantile(c(3, 1, 2)))))
    ## an opaque RHS still wins over the integer types
    w <- 1:3; w[1] <- as.int64(9)
    stopifnot(typeof(w) == "int64")
    ## NULL is a zero-length replacement, as it is for the base types
    v <- as.int64(1:3)
    assertError(v[1] <- NULL)
    v[integer()] <- NULL
    stopifnot(identical(v, as.int64(1:3)))
})

## the two sides of a subassignment must agree on what the NA pattern means
## before whole elements move: a raw copy used to launder an NA into a
## vector with no room for one, and an extreme value into NA
local({
    a <- as.int64(1:3, na = FALSE)
    a[1] <- as.int64(NA_integer_)
    stopifnot(is.na(a[1]))               # widened, not reinterpreted
    ## the same, without losing the names
    b <- as.int64(1:3, na = FALSE); names(b) <- c("a", "b", "c")
    b[1] <- as.int64(NA_integer_)
    stopifnot(identical(names(b), c("a", "b", "c")))
    ## the other direction: a whole-range value that collides with the
    ## target's NA is an error, as it is for c()
    x <- as.int64(1:3)
    y <- as.int64("-9223372036854775808", na = FALSE)
    assertError(x[1] <- y)
    ## an ordinary RHS is rendered into the target's domain, which stays
    z <- as.int64(1:3, na = FALSE); z[1] <- 9L
    stopifnot(!is.na(z[1]))
})

## c() and unlist() name one element per element, not one per argument,
## and honour an argument's own names
stopifnot(identical(names(c(a = as.int64(1:2), b = as.int64(3:4))),
                    c("a1", "a2", "b1", "b2")),
          identical(names(unlist(list(a = as.int64(1:2)))), c("a1", "a2")),
          identical(names(c(setNames(as.int64(1:3), c("a", "b", "c")), d = 4L)),
                    c("a", "b", "c", "d")))

## cbind() splits an opaque argument into elements when the result is a
## list, as rbind() already did; it used to replicate the whole vector
local({
    m <- cbind(as.int64(1:2), as.uint64(3:4))
    stopifnot(identical(dim(m), c(2L, 2L)), typeof(m) == "list",
              identical(vapply(m, length, 1L), rep(1L, 4L)),
              identical(as.double(unlist(m)), c(1, 2, 3, 4)))
    stopifnot(identical(as.double(unlist(cbind(as.int64(1:2), list(1, 2)))),
                        c(1, 2, 1, 2)))
})

## sort(method = "quick") reaches .Internal(qsort), which has no opaque
## arm and would round through double
local({
    v <- as.int64(c("9007199254740993", "9007199254740992"))
    s <- sort(v, method = "quick")
    stopifnot(typeof(s) == "int64",
              identical(as.character(s),
                        c("9007199254740992", "9007199254740993")))
})

## as.list() keeps the names, and is.na() sees through the boxes it makes
local({
    x <- setNames(as.int64(1:2), c("a", "b"))
    stopifnot(identical(names(as.list(x)), c("a", "b")),
              identical(names(lapply(x, function(z) 1L)), c("a", "b")))
    stopifnot(identical(is.na(as.list(as.int64(c(1L, NA)))), c(FALSE, TRUE)))
})

## a binary operator merges both operands' attributes, as every *_binary()
## does -- the right one used to be dropped whenever the left had any
stopifnot(identical(sort(names(attributes(structure(as.int64(1:3), units = "m") +
                                          structure(as.int64(1:3), foo = "bar")))),
                    c("foo", "units")))

## 10^19 is past INT64_MAX but still divides a 64-bit value exactly, so it
## decides between zero and an overflow rather than being one
stopifnot(as.character(round(as.uint64("9000000000000000000"), -19)) ==
              "10000000000000000000",
          as.character(round(as.int64("4000000000000000000"), -19)) == "0",
          as.character(round(as.uint64("10000000000000000000"), -19)) ==
              "10000000000000000000")
assertWarning(round(as.int64("9000000000000000000"), -19))  # 1e19: no int64
stopifnot(is.na(suppressWarnings(round(as.int64("9000000000000000000"), -19))))

## the opaque operand relaxes the numeric check for the *other* operand's
## type, but not for something that is not a vector at all: XLENGTH() would
## read the wrong union member of a symbol or a closure
assertError(as.int64(1:3) + quote(a))
assertError(as.int64(1:3) + sum)
assertError(as.int64(1:3) + new.env())

## %% and %/% by zero are NA, quietly: that is not an overflow
stopifnot(is.na(as.int64(5L) %% as.int64(0L)),
          is.na(as.int64(5L) %/% as.int64(0L)))
local({
    ## a vector with no NA to fall back on says which cause it is
    e <- tryCatch(as.int64(5L, na = FALSE) %/% as.int64(0L, na = FALSE),
                  error = conditionMessage)
    stopifnot(grepl("division by zero", e))
})

## unique() of an empty opaque vector: allocVector(ALTSXP, 0) has no meaning
stopifnot(identical(unique(int64()), int64()),
          identical(unique(as.int64(integer())), int64()),
          identical(duplicated(int64()), logical()),
          anyDuplicated(int64()) == 0L)

## a classed non-atomic object with an xtfrm method still reaches
## sort.list()'s radix path; the opaque storage test must not gate it
local({
    xtfrm.altsxpTest <- function(x) unlist(unclass(x))
    length.altsxpTest <- function(x) length(unclass(x))
    ## register them where sort.list() will find them
    environment(xtfrm.altsxpTest) <- environment(length.altsxpTest) <-
        globalenv()
    assign("xtfrm.altsxpTest", xtfrm.altsxpTest, globalenv())
    assign("length.altsxpTest", length.altsxpTest, globalenv())
    on.exit(rm(list = c("xtfrm.altsxpTest", "length.altsxpTest"),
               envir = globalenv()))
    a <- structure(list(3, 1, 2), class = "altsxpTest")
    stopifnot(identical(sort.list(a), c(2L, 3L, 1L)))
})

## complex outranks an opaque element type, as it does in c(), so a complex
## operand promotes rather than erroring
stopifnot(identical(as.int64(1L) + 1i, 1+1i),
          identical(as.int64(1L) == 1+0i, TRUE),
          typeof(c(as.int64(1L), 1i)) == "complex")

## for() over an opaque vector, in both interpreters
local({
    f <- function(v) { s <- character(); for (e in v) s <- c(s, format(e)); s }
    stopifnot(identical(f(as.int64(1:3)), c("1", "2", "3")),
              identical(f(as.uint64(1:2)), c("1", "2")))
    g <- compiler::cmpfun(f)
    stopifnot(identical(g(as.int64(1:3)), c("1", "2", "3")))
    ## the loop variable is a length-one vector of the same class
    for (e in as.int64(1:2)) NULL
    stopifnot(typeof(e) == "int64", length(e) == 1L)
})

## format() gives the atomic fall-through the same prettyNum() the numeric
## arm gets, so big.mark and friends are honoured rather than dropped
stopifnot(identical(format(as.int64(1234567), big.mark = ","), "1,234,567"),
          identical(format(as.int64(1:3), width = 5L), format(1:3, width = 5L)))

## is.finite() and friends: all R knows about an opaque element is whether
## the class calls it NA, and range(finite = TRUE) depends on them
stopifnot(identical(is.finite(as.int64(c(1L, NA))), c(TRUE, FALSE)),
          identical(is.infinite(as.int64(c(1L, NA))), c(FALSE, FALSE)),
          identical(is.nan(as.int64(c(1L, NA))), c(FALSE, FALSE)),
          identical(as.double(range(as.int64(c(3L, 1L, 2L)), finite = TRUE)),
                    c(1, 3)))

## min() and max() over several arguments compare the arguments' own
## reductions, so an argument that gave up its NA keeps the whole range
local({
    s <- as.int64("-9223372036854775808", na = FALSE)
    stopifnot(as.character(max(s, as.int64(1L))) == "1",
              as.character(min(s, as.int64(1L))) == "-9223372036854775808",
              as.character(max(as.int64(5L), 7L)) == "7",
              as.character(max(as.int64(5L), int64())) == "5",
              is.na(max(as.int64(NA_integer_), as.int64(1L))),
              as.character(max(as.int64(NA_integer_), as.int64(1L),
                               na.rm = TRUE)) == "1")
    ## nothing to reduce warns and gives the identity, as it does for the
    ## base types -- and as a double, since min(integer(0)) is one too
    assertWarning(min(int64()))
    stopifnot(identical(suppressWarnings(min(int64())), Inf),
              identical(suppressWarnings(max(int64())), -Inf))
})

## prod(), pmin() and pmax() reach an opaque vector
stopifnot(identical(prod(as.int64(1:4)), 24),
          identical(as.double(pmax(as.int64(1:3), as.int64(3:1))), c(3, 2, 3)),
          identical(as.double(pmin(as.int64(1:3), 2L)), c(1, 2, 2)),
          typeof(pmin(as.int64(1:3), 2L)) == "int64",
          ## a double argument promotes, as it does in c()
          typeof(pmax(as.int64(1:3), 2.5)) == "double",
          identical(as.double(pmin(as.int64(c(1L, NA, 3L)), as.int64(2L))),
                    c(1, NA, 2)),
          identical(as.double(pmin(as.int64(c(1L, NA, 3L)), as.int64(2L),
                                   na.rm = TRUE)), c(1, 2, 2)))

## rowsum() sums opaque columns exactly rather than dying in allocMatrix()
stopifnot(identical(as.double(rowsum(as.int64(1:3), c(1, 1, 2))), c(3, 3)),
          identical(as.double(rowsum(matrix(as.int64(1:6), 3L), c(1, 1, 2))),
                    c(3, 3, 9, 6)),
          identical(as.double(rowsum(data.frame(a = as.int64(1:3)),
                                     c(1, 1, 2))$a), c(3, 3)))

## psort() partially sorts an opaque vector rather than sorting all of it;
## every requested position must still hold the fully sorted value
local({
    set.seed(11)
    v <- sample(-50:50, 200L, replace = TRUE)
    ind <- c(3L, 100L, 197L)
    got <- .Internal(psort(as.int64(v), ind))
    stopifnot(identical(as.double(got)[ind], as.double(sort(v)[ind])))
    stopifnot(identical(as.double(median(as.int64(v))), median(as.double(v))))
})

## which.min()/which.max() test the sign of the comparison, which is all a
## Compare method promises
stopifnot(identical(which.min(as.int64(c(3L, 1L, 2L))), 2L),
          identical(which.max(as.int64(c(3L, 1L, 2L))), 1L))

## print() names the element type on its own line, and survives a class
## that reports none
stopifnot(identical(capture.output(print(as.int64(1:2))),
                    c("<int64[2]>", "[1] 1 2")))

## the shared formatter honours options(na.print=), rather than spelling
## NA out itself
local({
    op <- options(na.print = "...")
    on.exit(options(op))
    stopifnot(identical(format(as.int64(c(1L, NA))), format(c(1L, NA))))
})

## --- third review round: one block per bug ----------------------------

## cbind()'s fallback arm dispatches on TYPEOF(u), and ALTSXP ranks above
## every type it tests for, so an opaque argument fell through to the arm
## that reads RAW(u) -- one byte per element off the payload -- whenever
## another argument made the result double.  rbind() coerces first, which
## is why only cbind() was wrong.
local({
    stopifnot(identical(cbind(as.int64(1:2), c(1.5, 2.5)),
                        cbind(c(1, 2), c(1.5, 2.5))),
              identical(cbind(c(1.5, 2.5), as.int64(1:2)),
                        cbind(c(1.5, 2.5), c(1, 2))),
              identical(cbind(as.int64(1:2), 3:4, c(1.5, 2.5)),
                        cbind(c(1, 2), c(3, 4), c(1.5, 2.5))))
    ## a matrix argument, and a shorter one that recycles down the column
    stopifnot(identical(cbind(matrix(as.int64(1:4), 2L), c(1.5, 2.5)),
                        cbind(matrix(c(1, 2, 3, 4), 2L), c(1.5, 2.5))),
              identical(cbind(as.int64(1L), c(1.5, 2.5)),
                        cbind(1, c(1.5, 2.5))))
    ## a value that only the exact type can hold reaches the double result
    stopifnot(identical(cbind(as.int64("4611686018427387904"), 1.5),
                        cbind(4611686018427387904, 1.5)))
    ## the arms that already coerced, and rbind(), must stay as they were
    stopifnot(identical(rbind(as.int64(1:2), c(1.5, 2.5)),
                        rbind(c(1, 2), c(1.5, 2.5))),
              identical(cbind(as.int64(1:2), c("a", "b")),
                        cbind(c("1", "2"), c("a", "b"))),
              identical(cbind(as.int64(1:2), c(1i, 2i)),
                        cbind(c(1+0i, 2+0i), c(1i, 2i))))
    ## and the base types are untouched by the new arm
    stopifnot(identical(cbind(1:2, c(1.5, 2.5)), cbind(c(1, 2), c(1.5, 2.5))),
              identical(cbind(as.raw(1:2), c(1.5, 2.5)),
                        cbind(c(1, 2), c(1.5, 2.5))))
})

## memory.profile()'s table is indexed by SEXPTYPE with the two unused
## slots squeezed out, and was sized for the types up to OBJSXP: every live
## ALTSXP node incremented one past the end of it, and was counted nowhere.
local({
    keep <- lapply(1:100, function(i) as.int64(i))
    invisible(gc())
    p <- memory.profile()
    stopifnot(length(p) == length(names(p)),
              "altrep" %in% names(p),
              !anyNA(p),
              p[["altrep"]] >= length(keep))
})

## An ordinary operand was promoted as nullable whatever the opaque side
## reserved, so a whole-range vector was asked to widen -- which it cannot
## -- and a merely non-nullable one silently came back nullable.  c(),
## pmin() and x[i] <- v all took the domain from the opaque operand.
local({
    n <- as.int64(c("-9223372036854775808", "9223372036854775807"),
                  na = FALSE)
    stopifnot(identical(as.character(n + 0L), as.character(n)),
              identical(as.character(n * 1L), as.character(n)),
              identical(as.character(n %/% 1L), as.character(n)))
    ## the result keeps the operand's domain, as every other path does
    s <- as.int64(1:2, na = FALSE)
    assertError(c(s + 0L, NA_integer_))
    assertError(c(s * 2L, NA_integer_))
    stopifnot(identical(as.double(s + 0L), c(1, 2)),
              identical(as.double(s * 2L), c(2, 4)))
    ## an operand that really is missing is refused, exactly as c() refuses
    assertError(s + NA_integer_)
    assertError(c(s, NA_integer_))
    ## a nullable operand is unaffected
    stopifnot(identical(is.na(as.int64(1:2) + NA_integer_), c(TRUE, TRUE)))
    ## comparison builds no opaque result, so each side keeps its own
    ## domain: a whole-range operand still reports its extremes as data
    stopifnot(identical(n == n, c(TRUE, TRUE)),
              identical(n > 0L, c(FALSE, TRUE)),
              identical(is.na(n == NA_integer_), c(TRUE, TRUE)),
              identical(is.na(as.int64(1L) == NA_integer_), TRUE))
})

## do_first_min() gained an ALTSXP arm; a default arm alongside it would
## have turned an object whose xtfrm() method yields something other than
## a number from integer(0) into an error.
xtfrm.altsxpWM <- function(x) as.character(unclass(x))
stopifnot(identical(which.min(structure(1:3, class = "altsxpWM")), integer(0)),
          identical(which.max(structure(1:3, class = "altsxpWM")), integer(0)),
          identical(which.min(as.int64(c(3L, 1L, 2L))), 2L))
rm(xtfrm.altsxpWM)

## a raw byte is exact, and c() and the comparisons take one, but base R
## does not admit raw to arithmetic -- and the class used to
local({
    assertError(as.int64(3L) + as.raw(2))
    assertError(as.raw(2) + as.int64(3L))
    assertError(as.int64(3L) * as.raw(2))
    stopifnot(as.int64(3L) > as.raw(2),
              identical(as.double(c(as.int64(3L), as.raw(2))), c(3, 2)))
})

## sort.int()'s fast pass reads the class's Is_sorted and No_NA; an
## already-sorted vector comes back unchanged whatever na.last says
local({
    v <- as.int64(1:5)
    stopifnot(identical(as.double(sort(v)), as.double(v)),
              identical(as.double(sort(v, na.last = TRUE)), as.double(v)),
              identical(as.double(sort(v, na.last = NA)), as.double(v)),
              identical(as.double(sort(v, decreasing = TRUE)),
                        rev(as.double(v))))
})

## a list array renders each length-one element, and .Internal(inspect())
## names the type: both used to report an opaque one as unknown
local({
    stopifnot(identical(capture.output(print(cbind(as.int64(1:2),
                                                  list(1, 2)))),
                        capture.output(print(cbind(1:2, list(1, 2))))),
              any(grepl("ALTSXP",
                        capture.output(.Internal(inspect(as.int64(1:2)))))))
})

## --- allocating from a prototype --------------------------------------

## vector() and matrix() name the type they build, which an opaque vector
## cannot supply: its element type is a property of its ALTREP class, not of
## its SEXPTYPE.  .allocVectorLike() and .allocMatrixLike() take an example
## object instead -- the R-level counterparts of R_allocVectorLike() and
## R_allocMatrixLike(), and what base code that wrote vector(typeof(x), n)
## needs.
local({
    ## for the base types they are exactly vector() and matrix()
    stopifnot(identical(.allocVectorLike(TRUE, 3L), vector("logical", 3L)),
              identical(.allocVectorLike(1L, 3L), vector("integer", 3L)),
              identical(.allocVectorLike(1.5, 3L), vector("double", 3L)),
              identical(.allocVectorLike(1i, 3L), vector("complex", 3L)),
              identical(.allocVectorLike("a", 3L), vector("character", 3L)),
              identical(.allocVectorLike(as.raw(1), 3L), vector("raw", 3L)),
              identical(.allocVectorLike(list(), 3L), vector("list", 3L)),
              identical(.allocMatrixLike(1L, 2L, 3L), matrix(0L, 2L, 3L)))

    ## an opaque element type is zeroed too, as vector() zeroes every
    ## fixed-width type it knows -- and never left as the uninitialised
    ## payload New() hands back
    v <- .allocVectorLike(as.int64(1L), 3L)
    stopifnot(typeof(v) == "int64", length(v) == 3L, !anyNA(v),
              identical(as.double(v), c(0, 0, 0)))
    m <- .allocMatrixLike(as.int64(1L), 2L, 3L)
    stopifnot(typeof(m) == "int64", identical(dim(m), c(2L, 3L)),
              identical(as.double(m), numeric(6L)))
    stopifnot(identical(as.double(.allocVectorLike(as.uint64(1L), 2L)),
                        c(0, 0)))
    ## more elements than the staging buffer holds at once
    stopifnot(identical(as.double(.allocVectorLike(as.int64(1L), 1000L)),
                        numeric(1000L)))

    ## zero needs no NA to spare, so a vector that gave up its own
    ## allocates like any other
    w <- .allocVectorLike(as.int64(1L, na = FALSE), 3L)
    stopifnot(typeof(w) == "int64", !anyNA(w),
              identical(as.double(w), c(0, 0, 0)))
    e <- .allocVectorLike(as.int64(1L, na = FALSE))
    stopifnot(typeof(e) == "int64", length(e) == 0L)

    assertError(.allocVectorLike(1L, -1L))
    assertError(.allocVectorLike(quote(x), 1L))
    assertError(.allocMatrixLike(1L, -1L, 1L))
})

## the base functions that had no way to allocate an opaque result
local({
    g1 <- c("x", "x", "y", "y"); g2 <- c("p", "q", "p", "q")

    t1 <- tapply(as.int64(1:4), g1, sum)
    stopifnot(typeof(t1) == "int64", identical(as.double(t1), c(3, 7)),
              identical(names(t1), c("x", "y")))
    ## several factors, and the same answer the integers give
    t2 <- tapply(as.int64(1:4), list(g1, g2), sum)
    stopifnot(typeof(t2) == "int64", identical(dim(t2), c(2L, 2L)),
              identical(as.double(t2), as.double(tapply(1:4, list(g1, g2), sum))),
              identical(dimnames(t2), list(c("x", "y"), c("p", "q"))))
    ## a cell with nothing in it is NA, and default= still wins
    t3 <- tapply(as.int64(1:3), list(g1[1:3], g2[1:3]), sum)
    stopifnot(identical(is.na(as.vector(t3)), c(FALSE, FALSE, FALSE, TRUE)))
    t4 <- tapply(as.int64(1:3), list(g1[1:3], g2[1:3]), sum, default = 0L)
    stopifnot(identical(as.double(t4), c(1, 3, 2, 0)))

    ## diag() and apply() need one only on an empty extent
    d <- diag(matrix(as.int64(integer()), 0L, 3L))
    stopifnot(typeof(d) == "int64", length(d) == 0L,
              identical(as.double(diag(matrix(as.int64(1:6), 2L, 3L))),
                        c(1, 4)))
    a <- apply(matrix(as.int64(integer()), 0L, 3L), 1L, sum)
    stopifnot(typeof(a) == "int64", length(a) == 0L)

    ## asplit() allocated each piece with allocVector(TYPEOF(x), .), which
    ## an opaque vector has no answer for
    for (mar in 1:2) {
        got <- asplit(matrix(as.int64(1:6), 2L, 3L), mar)
        want <- asplit(matrix(1:6, 2L, 3L), mar)
        stopifnot(length(got) == length(want),
                  typeof(got[[1L]]) == "int64",
                  identical(lapply(got, as.double), lapply(want, as.double)))
    }
    stopifnot(identical(as.double(apply(matrix(as.int64(1:6), 2L, 3L), 1L, sum)),
                        c(9, 12)))
})

## vapply() allocates its result from FUN.VALUE, the one place R already had
## a prototype in hand
local({
    v <- vapply(1:3, function(i) as.int64(i), as.int64(1L))
    stopifnot(typeof(v) == "int64", identical(as.double(v), c(1, 2, 3)))
    ## a result that widens into the opaque type is taken, as c() takes it
    stopifnot(identical(as.double(vapply(1:3, function(i) i, as.int64(1L))),
                        c(1, 2, 3)))
    ## one that does not is the type error any other FUN.VALUE would give
    assertError(vapply(1:3, function(i) i + 0.5, as.int64(1L)))
    assertError(vapply(1:3, function(i) "a", as.int64(1L)))
    ## and so is the other opaque element type
    assertError(vapply(1:3, function(i) as.uint64(i), as.int64(1L)))
    ## a FUN.VALUE longer than one gives the usual matrix result
    m <- vapply(1:3, function(i) as.int64(c(i, i)), as.int64(c(1L, 1L)))
    stopifnot(typeof(m) == "int64", identical(dim(m), c(2L, 3L)),
              identical(as.double(m), c(1, 1, 2, 2, 3, 3)))
    ## names, and a zero-length input
    stopifnot(identical(names(vapply(c(a = 1L, b = 2L),
                                     function(i) as.int64(i), as.int64(1L))),
                        c("a", "b")),
              typeof(vapply(integer(), function(i) as.int64(i),
                            as.int64(1L))) == "int64")
})

## --- regressions -----------------------------------------------------

## match(x, table, incomparables = ) crashed: altsxpequal() was the one *equal
## callback in unique.c with no guard on a negative index, and removeEntry()
## marks a slot it has taken out of the table with exactly that.
local({
    x <- as.int64(c("1", "2", "3", "2"))
    stopifnot(identical(match(x, x, incomparables = as.int64("2")),
                        c(1L, NA, 3L, NA)))
    ## an ordinary vector of incomparables is promoted rather than refused:
    ## coerceVector() has no path to an opaque type, and asking for one used
    ## to raise an error naming a type typeof() never reports
    y <- as.int64(c(1, 2, 3, 2))
    stopifnot(identical(as.double(unique(y, incomparables = 2L)),
                        c(1, 2, 3, 2)),
              identical(duplicated(y, incomparables = 2L), rep(FALSE, 4)),
              identical(anyDuplicated(y, incomparables = 2L), 0L),
              identical(match(y, y, incomparables = 2), c(1L, NA, 3L, NA)),
              ## and one it cannot hold is refused, not quietly dropped
              identical(as.double(unique(y, incomparables = 2.5)),
                        c(1, 2, 3)))
    ## the other opaque type is an error rather than a silent no-op
    assertError(match(as.int64(1:3), as.int64(1:3),
                      incomparables = as.uint64(2)))
})

## x["d"] <- v assigned the element but dropped the name: the opaque arm of
## VectorAssign() returned before the block that installs the names a
## subscript introduces.
local({
    y <- as.int64(1:3)
    y["d"] <- as.int64(9L)
    stopifnot(length(y) == 4L, identical(names(y), c("", "", "", "d")),
              identical(as.double(y[["d"]]), 9))
    z <- c(a = as.int64(1L), b = as.int64(2L))
    z["c"] <- as.int64(3L)
    stopifnot(identical(names(z), c("a", "b", "c")),
              identical(as.double(z), c(1, 2, 3)))
    ## the [[<- form was always right, and still is
    w <- as.int64(1:3)
    w[["d"]] <- as.int64(9L)
    stopifnot(identical(names(w), c("", "", "", "d")))
    ## the names of the object assigned *from* are not touched, which is what
    ## installing them in place would risk
    a <- c(p = as.int64(1L), q = as.int64(2L))
    b <- a
    b["r"] <- as.int64(3L)
    stopifnot(identical(names(a), c("p", "q")),
              identical(names(b), c("p", "q", "r")))
    ## unchanged for the base types
    v <- 1:3
    v["d"] <- 9L
    stopifnot(identical(names(v), c("", "", "", "d")))
    d <- c(p = 1L, q = 2L)
    e <- d
    e["r"] <- 3L
    stopifnot(identical(names(d), c("p", "q")),
              identical(names(e), c("p", "q", "r")))
})

## Comparing against a double promoted both sides to double, so an exact
## value above 2^53 compared equal to its neighbour -- and disagreed with
## match(), which compares exactly.
local({
    a <- as.int64("9007199254740993")
    d <- 9007199254740992
    stopifnot(!(a == d), a > d, a != d, !(a <= d), a >= d,
              d < a, !(d == a),
              ## which is what %in% said all along
              !(a %in% d), is.na(match(a, d)))
    stopifnot(!(as.int64("9223372036854775807") == 9.223372036854776e18),
              !(as.uint64("10000000000000000001") == 1e19),
              as.uint64("10000000000000000001") > 1e19)
    ## fractions, infinities and NA still behave
    stopifnot(as.int64(2) < 2.5, as.int64(2) > 1.5, as.int64(3) == 3,
              as.int64(-1) < 0.5, as.int64(-1) > -1.5,
              as.int64(1) < Inf, as.int64(1) > -Inf,
              is.na(as.int64(1) == NA_real_), is.na(as.int64(1) == NaN))
})

## seq() and `:` took their endpoints through asReal(), which rounds a
## 64-bit value: the result then repeated some values and skipped others.
local({
    lo <- as.int64("9007199254740992")
    s <- seq(lo, lo + as.int64(4L))
    stopifnot(typeof(s) == "int64", length(s) == 5L, !anyDuplicated(s),
              identical(as.character(s[5L]), "9007199254740996"),
              identical(as.character(s), as.character(lo + as.int64(0:4))))
    stopifnot(identical(as.character(lo:(lo + as.int64(2L))),
                        as.character(lo + as.int64(0:2))))
    ## with a step, and in both directions
    stopifnot(identical(as.character(seq(lo, lo + as.int64(6L), by = 2)),
                        as.character(lo + as.int64(c(0, 2, 4, 6)))),
              identical(as.double(seq(as.int64(10), as.int64(1), by = -3)),
                        c(10, 7, 4, 1)),
              identical(as.double(as.int64(3):as.int64(1)), c(3, 2, 1)))
    ## a mixed pair is exact too, and seq(x) is 1:x as it is for an integer
    stopifnot(typeof(1L:as.int64(5)) == "int64",
              identical(as.double(1L:as.int64(5)), c(1, 2, 3, 4, 5)),
              identical(as.double(seq(as.int64(5))), c(1, 2, 3, 4, 5)),
              identical(as.double(seq(as.int64(-3))), c(1, 0, -1, -2, -3)))
    assertError(seq(as.int64(1), as.int64(10), by = -1))
    assertError(as.int64(NA):as.int64(3))
    ## and the ordinary path is untouched, byte-compiled or not
    f <- compiler::cmpfun(function(a, b) a:b)
    stopifnot(identical(f(1L, 5L), 1:5), identical(f(1.5, 4), c(1.5, 2.5, 3.5)),
              identical(seq(1, 10, by = 3), c(1, 4, 7, 10)),
              identical(seq(1.5, 3.5, by = 0.5), c(1.5, 2, 2.5, 3, 3.5)),
              identical(seq(0, 1, length.out = 3), c(0, 0.5, 1)))
})

## round() and signif() read only the first element of 'digits' and used it
## for the whole vector, and a non-negative first element made the call a
## no-op for every element.
local({
    x <- as.int64(c(1234, 5678, 91011))
    d <- c(1234, 5678, 91011)
    stopifnot(identical(as.double(round(x, c(-1, -2, -3))),
                        round(d, c(-1, -2, -3))),
              identical(as.double(signif(x, c(1, 2, 3))),
                        signif(d, c(1, 2, 3))),
              identical(as.double(round(x, c(1, -2, -3))),
                        round(d, c(1, -2, -3))),
              ## the scalar cases are unchanged
              identical(as.double(round(x)), d),
              identical(as.double(round(x, -2)), round(d, -2)),
              identical(as.double(signif(x, 2)), signif(d, 2)))
    ## an empty x gives an empty answer, and an empty 'digits' is the error
    ## do_Math2() gives for any type
    assertError(round(x, integer(0)))
    stopifnot(length(round(int64(0), 2)) == 0L,
              ## and 'digits' recycles the other way too
              identical(as.double(round(as.int64(1234), c(-1, -2))),
                        round(1234, c(-1, -2))))
})

## An unparseable field became NA with only a coercion warning: the class
## parses the whole column at once, so scan()'s own type check never ran.
local({
    f <- tempfile(fileext = ".csv")
    on.exit(unlink(f))
    writeLines(c("id", "1", "zz"), f)
    assertError(read.csv(f, colClasses = "int64"))
    assertError(scan(text = "1 2 zz", what = int64(), quiet = TRUE))
    ## an NA string, and an empty field, are still missing values
    stopifnot(identical(as.double(scan(text = "1 2 NA", what = int64(),
                                       quiet = TRUE)), c(1, 2, NA)))
    writeLines(c("a,b", "1,", "2,3"), f)
    got <- read.csv(f, colClasses = c("int64", "int64"))
    stopifnot(identical(as.double(got$a), c(1, 2)),
              identical(as.double(got$b), c(NA, 3)))
})

## as.int64() of a string took only a plain decimal, a much narrower grammar
## than as.integer(), and treated a blank field as a parse failure.
local({
    stopifnot(identical(as.double(as.int64(c("1e3", "1.5", "0x10", " 42 "))),
                        as.double(as.integer(c("1e3", "1.5", "0x10", " 42 ")))),
              ## a plain decimal is still read exactly, over the whole range
              identical(as.character(as.int64("9223372036854775806")),
                        "9223372036854775806"))
    ## a blank field is NA, silently, as it is for as.integer()
    stopifnot(is.na(as.int64("")), is.na(as.int64("   ")))
    old <- options(warn = 2)
    on.exit(options(old))
    stopifnot(is.na(as.int64("")))
    options(old)
    ## R can read back what write.csv() wrote
    f <- tempfile(fileext = ".csv")
    on.exit(unlink(f), add = TRUE)
    write.csv(data.frame(id = 1e15), f, row.names = FALSE)
    stopifnot(identical(as.character(read.csv(f, colClasses = "int64")$id),
                        "1000000000000000"))
})

## min() and max() over nothing returned NA, where every other numeric type
## returns the identity -- and R's own min(integer(0)) is a *double* Inf, so
## an exact element type does not force the answer.
local({
    assertWarning(stopifnot(identical(min(int64(0)), Inf)))
    assertWarning(stopifnot(identical(max(int64(0)), -Inf)))
    assertWarning(stopifnot(identical(min(as.int64(NA), na.rm = TRUE), Inf)))
    assertWarning(stopifnot(identical(range(int64(0)), c(Inf, -Inf))))
    ## so ordinary code over an empty selection works as it does for integer
    f <- function(v) if (min(v, na.rm = TRUE) < 5) "small" else "big"
    assertWarning(stopifnot(identical(f(int64(0)), f(integer(0)))))
    ## with something to reduce, the answer is still of the type
    x <- as.int64(c(3, 1, 2))
    stopifnot(typeof(min(x)) == "int64", as.double(min(x)) == 1,
              typeof(max(x)) == "int64", as.double(max(x)) == 3,
              is.na(min(as.int64(NA))))
})

## --- fixes from the fourth review round ------------------------------

## The only overflowing signed division has an exactly representable
## remainder, and zero-length arithmetic keeps the opaque operand's NA domain.
local({
    lo <- as.int64("-9223372036854775808", na = FALSE)
    stopifnot(identical(as.character(lo %% as.int64(-1L, na = FALSE)), "0"))
    assertError(lo %/% as.int64(-1L, na = FALSE))

    e <- as.int64(integer(), na = FALSE)
    z <- e + 1L
    stopifnot(identical(z, e))
    assertError(c(z, NA_integer_))
})

## An incomparable is represented in the opaque class before hashing, and a
## removed ALTSXP hash entry is a tombstone rather than an element index.
local({
    x <- as.int64(c(1L, 1L, 2L))
    stopifnot(identical(as.double(unique(x, incomparables = 1L)), c(1, 1, 2)),
              identical(duplicated(x, incomparables = 1L),
                        c(FALSE, FALSE, FALSE)),
              identical(anyDuplicated(x, incomparables = 1L), 0L),
              identical(match(as.int64(1:3), as.int64(1:3),
                              incomparables = 2L),
                        c(1L, NA_integer_, 3L)))
})

## int64 serialization stores portable high/low integer words, not native raw
## bytes.  0x0102030405060708 is 16909060 followed by 84281096 in that form.
local({
    x <- as.int64("72623859790382856", na = FALSE)
    txt <- rawToChar(serialize(x, NULL, ascii = TRUE))
    stopifnot(grepl("\n16909060\n84281096\n", txt, fixed = TRUE),
              identical(unserialize(serialize(x, NULL)), x),
              identical(unserialize(serialize(x, NULL, ascii = TRUE)), x))
})

## format.default() resets R_print even when reached from another object's
## print method, just as it does for the ordinary atomic types.
local({
    print.altsxpFormatProbe <- function(x, ...) cat(format(x), "\n")
    environment(print.altsxpFormatProbe) <- globalenv()
    assign("print.altsxpFormatProbe", print.altsxpFormatProbe, globalenv())
    on.exit(rm("print.altsxpFormatProbe", envir = globalenv()))
    z <- structure(as.int64(NA_integer_), class = "altsxpFormatProbe")
    stopifnot("NA" %in% trimws(capture.output(print(list(z), na.print = "-"))))
})

## The summary result must remain rooted while its empty-input warning is
## allocated and dispatched.  The answer itself is the identity, +/-Inf, as
## it is for every other numeric type; see the min()/max() regression above.
local({
    gctorture(TRUE)
    on.exit(gctorture(FALSE))
    z <- suppressWarnings(min(int64()))
    stopifnot(identical(z, Inf))
    stopifnot(identical(suppressWarnings(max(int64())), -Inf))
})

## --- fixes from the fifth review round -------------------------------

## isNumber() has to agree with isNumeric(): the logical operators reach for
## it, so an object that is.numeric() calls a number must answer `!`, `&`,
## `|`, `&&` and `||` rather than "invalid argument type".
local({
    x <- as.int64(c(0L, 2L, NA))
    stopifnot(is.numeric(x),
              identical(!x, c(TRUE, FALSE, NA)),
              identical(x & TRUE, c(FALSE, TRUE, NA)),
              identical(x | FALSE, c(FALSE, TRUE, NA)),
              identical(TRUE & x, c(FALSE, TRUE, NA)),
              identical(x & x, c(FALSE, TRUE, NA)),
              identical(as.int64(1L) && TRUE, TRUE),
              identical(as.int64(0L) || FALSE, FALSE))

    ## `!` keeps the shape, as it does for the base types
    names(x) <- c("a", "b", "c")
    stopifnot(identical(names(!x), c("a", "b", "c")))
    m <- as.int64(1:4)
    dim(m) <- c(2L, 2L)
    stopifnot(identical(dim(!m), c(2L, 2L)))

    ## the byte-compiled `&&` / `||` reach isNumber() through a different path
    f <- compiler::cmpfun(function(a, b) a && b)
    g <- compiler::cmpfun(function(a, b) a || b)
    stopifnot(identical(f(as.int64(1L), TRUE), TRUE),
              identical(g(as.int64(0L), FALSE), FALSE))

    ## a class whose traits do not claim to be numeric is still refused
    stopifnot(identical(!int64(), logical()))
})

## print() of an empty opaque vector reported the *rendering* it had just
## built -- "character(0)" -- under the header line.  The header already says
## the type and the length, which is all "integer(0)" says for a base vector.
local({
    stopifnot(identical(capture.output(print(int64())), "<int64[0]>"))

    z <- int64()
    names(z) <- character()
    stopifnot(identical(capture.output(print(z)), "<int64[0]>"))

    ## inside a list, where the element printer runs recursively
    stopifnot(identical(capture.output(print(list(int64()))),
                        c("[[1]]", "<int64[0]>", "")))

    ## a zero-extent array keeps its margins, as an empty base one does
    m <- int64()
    dim(m) <- c(0L, 3L)
    stopifnot(identical(capture.output(print(m)),
                        c("<int64[0]>", capture.output(print(array(integer(),
                                                                  c(0L, 3L)))))))
})

## rowSums() and friends refused an opaque matrix although is.numeric() calls
## it numeric.  They answer with a double, as they do for an integer matrix,
## so the class renders itself into one first -- what mean() and prod() do.
local({
    m <- as.int64(1:6)
    dim(m) <- c(2L, 3L)
    dimnames(m) <- list(c("a", "b"), c("x", "y", "z"))
    ref <- matrix(1:6, 2L, 3L, dimnames = dimnames(m))
    stopifnot(identical(rowSums(m), rowSums(ref)),
              identical(colSums(m), colSums(ref)),
              identical(rowMeans(m), rowMeans(ref)),
              identical(colMeans(m), colMeans(ref)))

    ## na.rm reaches the same accumulator
    n <- as.int64(c(1L, NA, 3L, 4L))
    dim(n) <- c(2L, 2L)
    nref <- matrix(c(1, NA, 3, 4), 2L, 2L)
    stopifnot(identical(rowSums(n), rowSums(nref)),
              identical(rowSums(n, na.rm = TRUE), rowSums(nref, na.rm = TRUE)),
              identical(colMeans(n, na.rm = TRUE), colMeans(nref, na.rm = TRUE)))

    ## and a data.frame column, which as.matrix() has to carry through
    stopifnot(identical(colSums(data.frame(p = as.int64(1:3))),
                        colSums(data.frame(p = 1:3))))
})

## complete.cases() reported "invalid 'type'" for an opaque argument, even
## though the class answers exactly the question it asks.  It takes vectors,
## matrices and data frames, and each of its three loops needed the arm.
local({
    x <- as.int64(c(1L, NA, 3L, NA))
    stopifnot(identical(complete.cases(x), c(TRUE, FALSE, TRUE, FALSE)),
              identical(complete.cases(x, c(1, 2, NA, 4)),
                        c(TRUE, FALSE, FALSE, FALSE)),
              identical(complete.cases(data.frame(a = x, b = 1:4)),
                        c(TRUE, FALSE, TRUE, FALSE)))

    ## a matrix argument recycles by column, as it does for a base type
    m <- as.int64(c(1L, NA, 3L, 4L, 5L, 6L))
    dim(m) <- c(3L, 2L)
    stopifnot(identical(complete.cases(m), c(TRUE, FALSE, TRUE)))

    ## and na.omit(), which is complete.cases() on a data frame
    d <- data.frame(a = x)
    stopifnot(identical(as.character(na.omit(d)$a), c("1", "3")))
})

## object.size() errored on any opaque vector.  The element type is opaque
## but its width is not, so the vector is charged for the bytes it would take
## laid out -- what object.size() already reports for a compact ALTREP
## integer, which it also charges as if it were materialised.
local({
    n <- 100L
    small <- object.size(int64(n))
    stopifnot(inherits(small, "object_size"),
              as.numeric(small) > 8 * n,
              ## and it grows with the length, so the payload is counted
              as.numeric(object.size(int64(2L * n))) >
              as.numeric(small) + 8 * n - 64)

    ## an opaque vector reached through a list is charged too
    stopifnot(as.numeric(object.size(list(int64(n)))) > 8 * n)
})

## printMatrix() had no ALTSXP arm, so prmatrix() reported "unimplemented
## type" for a matrix that print() renders happily -- print() gets there by
## formatting first and handing over a character matrix.  Doing that inside
## printMatrix() reaches every caller.
local({
    m <- as.int64(1:6)
    dim(m) <- c(2L, 3L)
    body <- capture.output(print(m))[-1L]   # drop the <int64[6]> header
    stopifnot(identical(capture.output(prmatrix(m)), body))

    ## the labels prmatrix() was given are used, and NA prints as such
    n <- as.int64(c(1L, NA, 3L, 4L))
    dim(n) <- c(2L, 2L)
    out <- capture.output(prmatrix(n, rowlab = c("r1", "r2"),
                                   collab = c("c1", "c2")))
    stopifnot(grepl("c1", out[[1L]]), grepl("c2", out[[1L]]),
              grepl("^r1", out[[2L]]), grepl("NA", out[[3L]]))
})

## `length<-` on a named opaque vector got the names wrong in two ways: the
## new tail was NA rather than blank, because the ALTSXP arm recursed into
## xlengthgets() on the character vector, which is where NA_STRING padding
## belongs; and for a class that had to be widened first the names went
## missing altogether, since Na_widen() answers with a bare vector.
local({
    z <- as.int64(1:3)
    names(z) <- c("a", "b", "c")
    length(z) <- 5L
    ref <- 1:3
    names(ref) <- c("a", "b", "c")
    length(ref) <- 5L
    stopifnot(identical(names(z), names(ref)), !anyNA(names(z)))

    ## the same object reached by growing through a subscript already agreed
    w <- as.int64(1:3)
    names(w) <- c("a", "b", "c")
    w[5L] <- as.int64(9L)
    stopifnot(identical(names(w), names(z)))

    ## a class with no NA of its own is widened first, and keeps its names
    v <- as.int64(1:3, na = FALSE)
    names(v) <- c("a", "b", "c")
    length(v) <- 5L
    stopifnot(identical(names(v), names(ref)),
              identical(as.double(v), c(1, 2, 3, NA, NA)))

    ## shrinking drops the tail of the names, as before
    u <- as.int64(1:3)
    names(u) <- c("a", "b", "c")
    length(u) <- 2L
    stopifnot(identical(names(u), c("a", "b")))
})

## format.info() rejected an opaque vector with "atomic vector arguments
## only", though is.atomic() calls it one.  It reports the width format()
## lays the vector out to, so the two have to agree.
local({
    x <- as.int64(c(1L, 22L, 333L))
    stopifnot(identical(format.info(x), format.info(c(1L, 22L, 333L))),
              identical(format.info(x)[[1L]], max(nchar(format(x)))))

    ## an NA counts for the width of the na.print string, as for an integer
    y <- as.int64(c(1L, NA))
    stopifnot(identical(format.info(y), format.info(c(1L, NA))),
              identical(format.info(y)[[1L]], max(nchar(format(y)))))

    ## and the wide values format() exists for
    big <- as.int64("9223372036854775807")
    stopifnot(identical(format.info(big)[[1L]], 19L))

    ## an empty vector reports formatInteger()'s floor, not zero
    stopifnot(identical(format.info(int64()), format.info(integer())))
})

## all.equal.numeric() computes target - current in the operands' own type,
## so an exact bounded type overflows on exactly the pair it is being asked to
## report on.  The function has always promoted an integer pair to double for
## that reason; is.integer() does not see the opaque exact types, so an
## unsigned pair underflowed in one direction and a signed pair overflowed at
## the ends of its range.
local({
    ## unsigned: target - current underflows when current is the larger
    stopifnot(identical(all.equal(as.uint64(1:3), as.uint64(5:7)),
                        all.equal(1:3, 5:7)),
              identical(all.equal(as.uint64(5:7), as.uint64(1:3)),
                        all.equal(5:7, 1:3)))

    ## signed: the difference does not fit in the type at all
    hi <- as.int64("9223372036854775807")
    lo <- as.int64("-9223372036854775806")
    stopifnot(identical(all.equal(hi, lo), "Mean relative difference: 2"),
              identical(all.equal(as.int64(1:3), as.int64(c(1L, 2L, 4L))),
                        all.equal(1:3, c(1L, 2L, 4L))),
              isTRUE(all.equal(as.int64(1:3), as.int64(1:3))))

    ## a difference below the tolerance is still equal, as it is for integer
    stopifnot(isTRUE(all.equal(as.int64("9007199254740993"),
                               as.int64("9007199254740992"))),
              isTRUE(all.equal(.Machine$integer.max,
                               .Machine$integer.max - 1L)))

    ## and the base types are untouched -- in particular complex, which
    ## is.numeric() keeps out of the promotion because as.double() would
    ## drop its imaginary part
    stopifnot(identical(all.equal(.Machine$integer.max, -.Machine$integer.max),
                        "Mean relative difference: 2"),
              isTRUE(all.equal(1L, 1)),
              isTRUE(all.equal(1 + 2i, 1 + 2i)),
              identical(all.equal(1 + 0i, 1 + 1i),
                        "Mean relative Mod difference: 1"),
              isTRUE(all.equal(as.Date("2020-01-01"), as.Date("2020-01-01"))))
})

## An opaque numeric vector is a valid subscript.  Every index R can address
## is exactly representable as a double -- R_XLEN_T_MAX is 2^52 and a double
## is exact to 2^53 -- so this is the double subscript path, and a value too
## large to be an index is out of bounds either way.  The two single-index
## entry points render one element rather than the whole subscript.
local({
    v <- 1:10
    names(v) <- letters[1:10]
    m <- matrix(1:12, 3L, 4L)
    L <- as.list(1:5)
    i <- as.int64(2L)

    ## [ , including the shapes that are not a plain positive index
    stopifnot(identical(v[i], v[2]),
              identical(v[c(i, as.int64(5L))], v[c(2, 5)]),
              identical(v[-i], v[-2]),
              identical(v[as.int64(0L)], v[0]),
              identical(v[as.int64(NA_integer_)], v[NA_integer_]),
              identical(v[as.int64(20L)], v[20]),
              identical(v[as.int64(integer())], v[integer()]),
              identical(names(v[c(i, as.int64(3L))]), names(v[c(2, 3)])))

    ## [[ , including recursive indexing, where pos is not 0
    stopifnot(identical(v[[i]], v[[2]]), identical(L[[i]], L[[2]]))
    l <- list(list(1, 2), list(3, 4))
    stopifnot(identical(l[[as.int64(c(2L, 1L))]], l[[c(2, 1)]]))

    ## assignment, including growing
    stopifnot(identical({z <- v; z[i] <- 99L; z}, {z <- v; z[2] <- 99L; z}),
              identical({z <- v; z[[i]] <- 99L; z}, {z <- v; z[[2]] <- 99L; z}),
              identical({z <- v; z[-i] <- 0L; z}, {z <- v; z[-2] <- 0L; z}),
              identical({z <- v; z[as.int64(13L)] <- 1L; z},
                        {z <- v; z[13] <- 1L; z}))

    ## matrix and array margins, which narrow to integer as a double does
    stopifnot(identical(m[i, as.int64(3L)], m[2, 3]),
              identical(m[i, ], m[2, ]),
              identical(m[, i], m[, 2]),
              identical(m[-i, ], m[-2, ]),
              identical({z <- m; z[i, as.int64(1L)] <- 0L; z},
                        {z <- m; z[2, 1] <- 0L; z}))
    a <- array(1:8, c(2L, 2L, 2L))
    stopifnot(identical(a[i, i, i], a[2, 2, 2]))

    ## uint64 indexes the same way
    stopifnot(identical(v[as.uint64(2L)], v[2]),
              identical(m[as.uint64(2L), as.uint64(3L)], m[2, 3]))

    ## an empty subscript selects nothing, as an empty double one does
    stopifnot(identical(v[int64()], v[integer()]))
})

## mode(x) reports an opaque class's element type, but the type table maps
## names to SEXPTYPEs and an element type is not one.  Two resolutions follow
## from that, and the order between them matters: the object's own name is
## the identity case, which keeps a trait belonging to this object that no
## name could carry, and a registered name resolves to a class to build from.
local({
    x <- as.int64(1:3)
    names(x) <- c("a", "b", "c")
    u <- as.uint64(1:3)

    ## the round trips, and as.vector() drops attributes as it does elsewhere
    stopifnot(identical(as.vector(x, "int64"), as.vector(x, "any")),
              identical(as.vector(x, mode(x)), as.vector(x, "any")),
              is.null(names(as.vector(x, "int64"))),
              identical(as.vector(u, mode(u)), as.vector(u, "any")),
              typeof(as.vector(u, "uint64")) == "uint64")

    ## storage.mode<- of the mode it already has keeps the object whole
    y <- x
    storage.mode(y) <- storage.mode(y)
    stopifnot(identical(y, x))

    ## The identity case is decided against the object, so it keeps the NA
    ## domain, which is per object; resolving the same name through the class
    ## would have replaced it with the type's default form.
    nn <- as.int64(1:3, na = FALSE)
    stopifnot(identical(as.vector(nn, "int64"), nn))
    v <- as.vector(nn, "int64")
    assertError(v[1L] <- NA)
    w <- nn
    storage.mode(w) <- "int64"
    stopifnot(identical(w, nn))

    ## The two classes must not become synonyms: they share a SEXPTYPE, so
    ## resolving either name to ALTSXP alone would make this silently answer
    ## with the other one.  Here the class declines -- int64 and uint64 have
    ## no common representation -- and as.uint64() is the explicit route.
    assertError(as.vector(x, "uint64"))
    assertError(as.vector(u, "int64"))
    assertError(`storage.mode<-`(x, "uint64"))
    stopifnot(!is.vector(x, "uint64"), is.vector(x, "int64"))

    ## converting *into* a class by name, which is what a name resolving to a
    ## class buys: the class builds the object from the ordinary vector
    stopifnot(identical(as.vector(1:3, "int64"), as.int64(1:3)),
              identical(as.vector(c(1, 2, 3), "int64"), as.int64(1:3)),
              identical(as.vector("9007199254740993", "int64"),
                        as.int64("9007199254740993")),
              typeof(as.vector(1:3, "uint64")) == "uint64",
              is.null(names(as.vector(c(a = 1L), "int64"))))
    assertError(as.vector(list(1), "int64"))

    z <- 1:3
    storage.mode(z) <- "int64"
    stopifnot(identical(z, as.int64(1:3)))
    z <- 1:3
    mode(z) <- "int64"
    stopifnot(typeof(z) == "int64")

    ## storage.mode<- keeps attributes, as it does for the base types
    m <- matrix(1:6, 2L, 3L)
    storage.mode(m) <- "int64"
    stopifnot(typeof(m) == "int64", identical(dim(m), c(2L, 3L)),
              identical(as.double(m), as.double(1:6)))

    ## nothing moved for the base types
    stopifnot(identical(as.vector(1:3, "integer"), 1:3),
              identical(as.vector(1:3, mode(1:3)), as.double(1:3)),
              identical(as.vector(x, "double"), as.double(1:3)),
              typeof(as.vector(x, "altrep")) == "int64")
    assertError(as.vector(1:3, "altrep"))
})

## vector() resolves its mode through the type table too, and an element type
## is not a SEXPTYPE, so an opaque class was unreachable by name.  A class
## that has claimed its element type builds the zero-filled vector itself --
## only it knows what its zero is.
local({
    stopifnot(identical(vector("int64", 3L), int64(3L)),
              identical(vector("int64"), int64()),
              identical(vector("uint64", 3L), uint64(3L)),
              identical(vector("int64", 3L), .allocVectorLike(int64(), 3L)),
              typeof(vector("uint64", 2L)) == "uint64")

    ## the elements are the type's zero, as they are for every other mode
    stopifnot(identical(as.double(vector("int64", 4L)), rep(0, 4L)),
              !anyNA(vector("int64", 4L)),
              identical(as.double(vector("uint64", 4L)), rep(0, 4L)))

    ## A name gives the type's default form.  A trait that belongs to the
    ## object rather than to the type is what .allocVectorLike() carries over
    ## instead, and no name could.
    v <- vector("int64", 2L)
    v[1L] <- NA
    stopifnot(is.na(v[1L]))
    w <- .allocVectorLike(as.int64(1L, na = FALSE), 2L)
    assertError(w[1L] <- NA)

    ## the length argument is checked as it is for the base types
    assertError(vector("int64", -1L))
    assertError(vector("int64", c(1L, 2L)))
    assertError(vector("int64", NA_integer_))

    ## the type table still wins, and a name no class has claimed is still an
    ## error -- with the message vector() has always given
    stopifnot(identical(vector("integer", 2L), c(0L, 0L)),
              identical(vector("numeric", 2L), c(0, 0)),
              identical(vector("list", 2L), list(NULL, NULL)))
    e <- tryCatch(vector("no_such_type", 2L), error = identity)
    stopifnot(grepl("cannot make a vector of mode", conditionMessage(e),
                    fixed = TRUE))
    assertError(vector("altrep", 2L))

    ## vector(typeof(x), n) round trips, which is why the name a class claims
    ## is its own element type rather than one it registers separately
    for (p in list(int64(1L), uint64(1L), TRUE, 1L, 1.5, 1i, "a", as.raw(1),
                   list()))
        stopifnot(identical(vector(typeof(p), 3L), .allocVectorLike(p, 3L)))

    gctorture(TRUE)
    stopifnot(identical(vector("int64", 40L), int64(40L)),
              identical(as.vector(1:5, "uint64"), as.uint64(1:5)))
    gctorture(FALSE)
})

## --- fixes from the sixth review round --------------------------------

## R_binary() emits the recycling warning before it reaches the ALTSXP arm, so
## an operation the class declines and hands back to R_binary() warned twice
## where every base numeric type warns once.
local({
    n.warnings <- function(expr) {
        n <- 0L
        withCallingHandlers(expr, warning = function(e) {
            n <<- n + 1L
            invokeRestart("muffleWarning")
        })
        n
    }
    for (e in list(quote(as.int64(1:3) + c(1.5, 2.5)),
                   quote(c(1.5, 2.5) + as.int64(1:3)),
                   quote(as.int64(1:3) / as.int64(1:2)),
                   quote(as.int64(1:5) ^ c(1.5, 2.5, 3.5)),
                   quote(as.uint64(1:3) * c(1.5, 2.5))))
        stopifnot(identical(n.warnings(eval(e)), 1L))

    ## recycling to the common length must not disturb the answer: the result
    ## and its attributes still come from the original operands
    x <- as.int64(1:3); names(x) <- c("a", "b", "c")
    d <- c(a = 1, b = 2, c = 3)
    y <- c(p = 1.5, q = 2.5)
    stopifnot(identical(suppressWarnings(x + y), suppressWarnings(d + y)),
              identical(suppressWarnings(y + x), suppressWarnings(y + d)))
    m <- as.int64(1:6); dim(m) <- c(2L, 3L)
    dm <- as.double(1:6); dim(dm) <- c(2L, 3L)
    short <- c(1.5, 2.5, 3.5, 4.5)
    stopifnot(identical(suppressWarnings(m + short), suppressWarnings(dm + short)),
              identical(suppressWarnings(short + m), suppressWarnings(short + dm)))

    ## the operand may still be a compact sequence, which the recycling has to
    ## materialize -- and that allocates
    gctorture(TRUE)
    stopifnot(identical(suppressWarnings(as.int64(1:5) / (1:3)),
                        suppressWarnings(as.double(1:5) / as.double(1:3))),
              identical(suppressWarnings((1:5) / as.int64(1:3)),
                        suppressWarnings(as.double(1:5) / as.double(1:3))))
    gctorture(FALSE)
})

## cbind()/rbind() refused a NULL argument: the other modes reach it through
## coerceVector(), which makes it the zero-length vector the fill loop then
## skips, but the opaque arm asked the class to coerce NILSXP.  A NULL only
## gets that far when every argument is zero length, since lenmin is 0 there.
local({
    stopifnot(identical(dim(cbind(int64(0), NULL)), c(0L, 2L)),
              identical(dim(rbind(int64(0), NULL)), c(2L, 0L)),
              identical(dim(cbind(NULL, int64(0))), c(0L, 2L)),
              identical(dim(cbind(int64(0), NULL, int64(0))), c(0L, 3L)))
    for (f in c(cbind, rbind))
        stopifnot(identical(dim(f(int64(0), NULL)), dim(f(integer(0), NULL))),
                  identical(as.double(f(as.int64(1:3), NULL)),
                            as.double(f(1:3, NULL))))
})

## A character `incomparables` was silently discarded.  Unlike match(), which
## compares as character when the promotion is not exact, the answer here has
## to be in the class, so there is nothing to decline to -- and as.int64("2")
## is the documented way to enter a value a double cannot hold.
local({
    y <- as.int64(c(1, 2, 2, 3))
    stopifnot(identical(as.double(unique(y, incomparables = "2")),
                        c(1, 2, 2, 3)),
              identical(duplicated(y, incomparables = "2"), rep(FALSE, 4)),
              identical(anyDuplicated(y, incomparables = "2"), 0L),
              identical(match(y, y, incomparables = "2"), c(1L, NA, NA, 4L)))

    ## every spelling as.int64() itself takes
    for (s in c("2", " 2 ", "+2", "0x2", "2e0"))
        stopifnot(identical(as.double(unique(y, incomparables = s)),
                            c(1, 2, 2, 3)))

    ## a fractional string must not round onto a neighbour and make *that*
    ## value incomparable, exactly as for the double it parses to
    stopifnot(identical(as.double(unique(y, incomparables = "2.5")), c(1, 2, 3)),
              identical(as.double(unique(y, incomparables = 2.5)), c(1, 2, 3)))

    ## a value the class cannot hold is still dropped, with its warning
    stopifnot(identical(as.double(suppressWarnings(
                            unique(y, incomparables = "99999999999999999999"))),
                        c(1, 2, 3)))

    ## the point of taking the string at all: exact past 2^53, where routing
    ## it through a double would land on the neighbouring even value
    big <- as.int64(c("9007199254740993", "9007199254740993", "1"))
    stopifnot(identical(format(unique(big, incomparables = "9007199254740993")),
                        format(big)),
              identical(as.double(unique(big, incomparables = "9007199254740992")),
                        c(9007199254740992, 1)))

    ## match() still compares a character operand as character, unchanged
    stopifnot(identical(match(as.int64(2L), c("1", "2", "3")), 2L),
              identical(match("1e3", as.int64(1000)), NA_integer_))
})

## isNumeric() is TRUE for an opaque vector whose class says so, but a guard
## that then reads REAL()/INTEGER() directly reinterprets the payload rather
## than erroring: inside R those are the unchecked Defn.h macros.  Both sites
## below took the "not a real vector, so it must be an integer one" branch.
local({
    ## tspgets(): the bytes were read as three int32s.  Chosen so the garbage
    ## passes tspgets()'s own validation -- low32(v0)=1, high32(v0)=10,
    ## low32(v1)=1 spelled a perfectly plausible tsp of (1, 10, 1).
    z <- 1:10
    tsp(z) <- as.int64(c(1L, 10L, 1L))
    stopifnot(identical(tsp(z), c(1, 10, 1)))

    z2 <- 1:10
    forged <- c(as.int64("42949672961"), as.int64("1"), as.int64(0))
    e <- tryCatch({ tsp(z2) <- forged; tsp(z2) }, error = identity)
    stopifnot(inherits(e, "error"), is.null(tsp(z2)))

    ## and a value the class holds but tsp cannot use is still refused
    assertError(`tsp<-`(1:10, as.int64(c(1L, 10L, 0L))))

    ## C_plot_window(): the 16-byte int64 payload was read as two doubles,
    ## which set the window to ~4.9e-324 .. 4.9e-323 without complaint
    f <- tempfile(fileext = ".pdf")
    pdf(f)
    on.exit({ dev.off(); unlink(f) })
    plot.window(as.int64(c(1L, 10L)), c(0, 1))
    alt <- par("usr")
    plot.window(c(1, 10), c(0, 1))
    stopifnot(identical(alt, par("usr")))

    plot.window(c(0, 1), as.int64(c(1L, 10L)))
    alt <- par("usr")
    plot.window(c(0, 1), c(1, 10))
    stopifnot(identical(alt, par("usr")))
})

## format()'s opaque arm used to return before the argument checks, so every
## argument it does not itself use went unvalidated -- and it rejected a
## negative 'width' the base path accepts.  The answer for an opaque vector
## still comes from the class; only the checking is shared.
local({
    bad <- list(quote(format(X, nsmall = -1)),
                quote(format(X, nsmall = 21)),
                quote(format(X, digits = 1e6)),
                quote(format(X, digits = -99)),
                quote(format(X, scientific = list())),
                quote(format(X, scientific = character(0))),
                quote(format(X, na.encode = NA)),
                quote(format(X, decimal.mark = 1)),
                quote(format(X, decimal.mark = character(0))),
                quote(format(X, trim = NA)))
    msg <- function(e, v)
        tryCatch({ eval(e, list(X = v)); NA_character_ },
                 error = conditionMessage)
    for (e in bad) {
        alt <- msg(e, as.int64(1))
        int <- msg(e, 1L)
        stopifnot(!is.na(alt), identical(alt, int))
    }

    ## a negative width is accepted, as it is for an integer vector
    stopifnot(identical(format(as.int64(1), width = -1), "1"),
              identical(format(1L, width = -1), "1"))

    ## and the arguments that do reach the answer still do
    stopifnot(identical(format(as.int64(1:3), width = 5),
                        format(1:3, width = 5)),
              identical(format(as.int64(1:3), trim = TRUE), c("1", "2", "3")),
              identical(format(as.int64(c(1, NA))), format(c(1L, NA))))
})

## int64 and uint64 have no conversion between them, so the routes that ask
## the class for one report an error while the explicit constructors, which
## range-check each value, do not.  int64.Rd documents the split.
local({
    u <- as.uint64(1:3)
    i <- as.int64(1:3)
    stopifnot(identical(as.double(as.int64(u)), c(1, 2, 3)),
              identical(as.double(as.uint64(i)), c(1, 2, 3)))
    v <- u; mode(v) <- "int64"
    stopifnot(identical(v, i))

    assertError(as.vector(u, "int64"))
    assertError(as.vector(i, "uint64"))
    assertError(`storage.mode<-`(u, "int64"))
    assertError(`storage.mode<-`(i, "uint64"))
    ## the same answer is what makes these two a list rather than a vector
    stopifnot(is.list(c(i, u)))
})

## sign() answers a double for every numeric type, integer included, so it
## does for these too -- it used to keep the opaque type, which also put the
## answer under the input's NA domain.  abs() does keep the type, as it does
## for an integer vector.
local({
    for (v in list(as.int64(c(-5, 0, 7)), as.uint64(c(0, 7)),
                   as.int64(c(NA, -3)))) {
        d <- as.double(v)
        stopifnot(identical(typeof(sign(v)), "double"),
                  identical(sign(v), sign(d)))
    }
    ## exact past 2^53, where the rendering rounds but the sign cannot
    stopifnot(identical(sign(as.int64(c("9007199254740993",
                                        "-9007199254740993", "0"))),
                        c(1, -1, 0)))
    ## and a whole-range vector is no longer read in its own NA domain
    w <- as.int64(c("-9223372036854775808", "9223372036854775807"),
                  na = FALSE)
    stopifnot(identical(sign(w), c(-1, 1)))

    stopifnot(identical(typeof(abs(as.int64(1:3))), "int64"),
              identical(typeof(abs(as.uint64(1:3))), "uint64"),
              identical(as.double(abs(as.int64(c(-5, 5)))), c(5, 5)))
})

## x[[i]] <- v on an opaque vector reaches the shared tail through a goto,
## which used to be preceded by an UNPROTECT(2)/PROTECT/PROTECT that only the
## fall-through path ran.  Exercise both paths with the GC on every allocation.
local({
    gctorture(TRUE)
    for (i in 1:15) {
        x <- as.int64(1:5); names(x) <- letters[1:5]
        x[[2]] <- as.int64(99)
        x[["f"]] <- as.int64(6)
        stopifnot(identical(as.double(x), c(1, 99, 3, 4, 5, 6)),
                  identical(names(x), c(letters[1:5], "f")))
        m <- as.int64(1:6); dim(m) <- c(2L, 3L)
        m[[2, 3]] <- as.int64(60)
        stopifnot(identical(as.double(m), c(1, 2, 3, 4, 5, 60)))
        l <- list(a = 1)
        l[["b"]] <- as.int64(2)
        stopifnot(identical(as.double(l$b), 2))
    }
    gctorture(FALSE)
})

## A 1-D opaque array is labelled by its dimnames like any other, and the
## name of that dimnames element is a title above them -- the arm reached the
## labels through getAttrib(names), which answers them for a 1-D array, but
## never the title.
local({
    labs <- c("a", "b", "c")
    body <- function(x) {
        out <- capture.output(print(x))
        out[!grepl("^<int64\\[", out)]      # drop the opaque type header
    }
    stopifnot(identical(body(array(as.int64(1:3), 3L, list(g = labs))),
                        capture.output(print(array(1:3, 3L, list(g = labs))))),
              identical(body(array(as.int64(1:3), 3L, list(labs))),
                        capture.output(print(array(1:3, 3L, list(labs))))),
              identical(body(array(as.int64(1:3), 3L)),
                        capture.output(print(array(1:3, 3L)))))
    ## a zero-extent 1-D array still says only what it is, as before
    stopifnot(identical(capture.output(print(array(as.int64(integer(0)), 0L))),
                        "<int64[0]>"))
})

## pmin()/pmax() picked the argument the answer is built from inside the
## zero-length shortcut, before the rule that prefers an operand whose domain
## includes NA.  The traits of the result must not depend on the length.
local({
    nullable <- function(x)
        grepl("nullable=1", capture.output(.Internal(inspect(x)))[1L])
    nn <- as.int64(1:3, na = FALSE)
    ok <- as.int64(1:3)
    nn0 <- as.int64(integer(0), na = FALSE)
    ok0 <- as.int64(integer(0))
    for (f in list(pmin, pmax)) {
        stopifnot(nullable(f(nn, ok)), nullable(f(ok, nn)),
                  ## same rule when there is nothing to compare
                  nullable(f(nn0, ok0)), nullable(f(ok0, nn0)),
                  ## and no argument that can be NA still means it cannot
                  !nullable(f(nn0, nn0)), !nullable(f(nn, nn)))
        stopifnot(identical(length(f(nn0, ok0)), 0L))
    }
})

## The non-nullable (na = FALSE) corner: a vector that gave up its NA had
## three paths that asked it for one anyway.
local({
    ## min()/max() of nothing: base R warns and answers +/-Inf as a double,
    ## which is what makes `if (min(x, na.rm = TRUE) < 5)` over an empty
    ## selection work.  The class was asked first, and a class with no NA to
    ## offer rightly refused a reduction of nothing.
    for (v in list(int64(0), as.int64(integer(0), na = FALSE),
                   as.uint64(integer(0), na = FALSE))) {
        stopifnot(identical(tryCatch(min(v), warning = function(w) w$message),
                            "no non-missing arguments to min; returning Inf"),
                  identical(tryCatch(max(v), warning = function(w) w$message),
                            "no non-missing arguments to max; returning -Inf"),
                  identical(suppressWarnings(min(v)), Inf),
                  identical(suppressWarnings(max(v)), -Inf))
    }
    x <- as.int64(1:3, na = FALSE)
    stopifnot(identical(suppressWarnings(min(x[x > 99])), Inf))
    ## with elements, the class still answers
    stopifnot(identical(as.double(min(x)), 1), identical(as.double(max(x)), 3))
    ## and an all-NA nullable vector keeps the base answer
    stopifnot(identical(suppressWarnings(min(as.int64(c(NA, NA)), na.rm = TRUE)),
                        Inf),
              is.na(min(as.int64(c(NA, 1)))))

    ## match() promotes the other operand so that values are compared rather
    ## than renderings.  It used to decline whenever the opaque operand was
    ## whole-range, which sent exactly those vectors down the character path:
    ## "1e+06" is not "1000000".
    for (na in c(TRUE, FALSE)) {
        stopifnot(1e6 %in% as.int64(1e6, na = na),
                  1e15 %in% as.int64(1e15, na = na),
                  identical(match(1e6, as.int64(1e6, na = na)), 1L),
                  identical(match(as.int64(c(2, 5), na = na), c(1, 2, 3)),
                            c(2L, NA)))
    }
    ## a value the class cannot hold still matches nothing rather than erroring
    stopifnot(!(1.5 %in% as.int64(1:3, na = FALSE)))

    ## incomparables needs a nullable *prototype*, not a nullable copy of the
    ## table: widening the data errors on a whole-range vector that actually
    ## holds the pattern its class reserves for NA.
    w <- as.int64(c("1", "-9223372036854775808"), na = FALSE)
    stopifnot(identical(format(unique(w, incomparables = 1)), format(w)),
              identical(duplicated(w, incomparables = 1), c(FALSE, FALSE)),
              identical(anyDuplicated(w, incomparables = 1), 0L),
              identical(match(w, w, incomparables = 1), c(NA_integer_, 2L)))
    ## and the ordinary whole-range case keeps working
    n2 <- as.int64(c("1", "2", "2"), na = FALSE)
    stopifnot(identical(as.double(unique(n2, incomparables = 1)), c(1, 2)))
})

## Coercion from a double truncates toward zero, so the range test has to be
## against the truncated value: -0.5 is 0, which uint64 holds perfectly well.
## The unsigned arm rejected on the raw sign instead, so a value in range came
## back NA -- and an error for a vector that cannot hold NA.
local({
    for (na in c(TRUE, FALSE))
        stopifnot(identical(as.double(as.uint64(-0.5, na = na)), 0),
                  identical(as.double(as.uint64(-0.999, na = na)), 0),
                  identical(as.double(as.uint64("-0.5", na = na)), 0),
                  identical(as.double(as.uint64(-0, na = na)), 0))
    ## the same answers the signed class and as.integer() give
    stopifnot(identical(as.double(as.uint64(-0.5)), as.double(as.int64(-0.5))),
              identical(as.double(as.uint64(-0.5)), as.double(as.integer(-0.5))))
    ## and a value that really is out of range is still NA
    stopifnot(is.na(suppressWarnings(as.uint64(-1))),
              is.na(suppressWarnings(as.uint64(-1.5))),
              is.na(suppressWarnings(as.uint64(-1e18))))
    stopifnot(identical(as.double(as.uint64(c(-0.5, 1.5, 2))), c(0, 1, 2)))
})

## An opaque matrix is laid out column by column, as the base numeric types
## are: the rendering used to be padded to one width across the whole slice
## first, so every column printed at the width of the widest cell.
local({
    body <- function(x) capture.output(print(x))[-1L]   # drop the type header
    cases <- list(
        list(matrix(as.int64(c(1, 1000000)), 1, 2), matrix(c(1L, 1000000L), 1, 2)),
        list(matrix(as.int64(1:6), 3, 2), matrix(1:6, 3, 2)),
        list(matrix(as.int64(c(1, NA, 3, 1e6)), 2, 2),
             matrix(c(1L, NA, 3L, 1000000L), 2, 2)),
        list(array(as.int64(1:8), c(2, 2, 2)), array(1:8, c(2, 2, 2))),
        list(matrix(as.int64(1:4), 2, 2, dimnames = list(c("r1", "r2"),
                                                         c("c1", "c2"))),
             matrix(1:4, 2, 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))))
    for (p in cases)
        stopifnot(identical(body(p[[1L]]), capture.output(print(p[[2L]]))))

    ## prmatrix() reaches the other arm, in printarray.c
    stopifnot(identical(capture.output(prmatrix(matrix(as.int64(c(1, 1e6)), 1, 2))),
                        capture.output(prmatrix(matrix(c(1L, 1000000L), 1, 2)))))

    ## a plain vector still takes one common width, as before
    stopifnot(identical(body(as.int64(c(1, 1000000))),
                        capture.output(print(c(1L, 1000000L)))))
})

## --- generic ALTSXP region-contract regressions ----------------------

## Source-tree tests build a tiny pointer-less ALTSXP class whose Get/Set
## methods deliberately handle only a few elements per call.  Installed tests
## may run without a compiler, so leave this block to the source build when the
## helper DSO is present.
dll.name <- paste0("altsxp_test", .Platform$dynlib.ext)
dll.paths <- c(dll.name, file.path("tests", dll.name))
dll.paths <- dll.paths[file.exists(dll.paths)]
if (length(dll.paths)) local({
    dll <- dyn.load(dll.paths[[1L]])
    on.exit(dyn.unload(dll[["path"]]))
    call.test <- function(name, ...)
        .Call(name, ..., PACKAGE = "altsxp_test")
    new.test <- function(x, wide = FALSE)
        call.test("C_altsxp_test_constructor", x, wide)
    new.kind <- function(kind, x)
        call.test("C_altsxp_test_constructor2", kind, x)
    contents <- function(x) call.test("C_altsxp_test_contents", x)
    counts <- function(x) call.test("C_altsxp_test_counts", x)

    ## writeBin(), readBin(), and the default serializer must drain short
    ## positive returns instead of consuming an uninitialised buffer tail.
    bytes <- as.raw(0:31)
    x <- new.test(bytes)
    encoded <- writeBin(x, raw())
    stopifnot(identical(encoded, bytes), counts(x)[[1L]] >= ceiling(32 / 3))

    y <- readBin(encoded, x, length(bytes))
    stopifnot(identical(contents(y), bytes),
              counts(y)[[2L]] >= ceiling(32 / 2))

    z <- unserialize(serialize(x, NULL))
    stopifnot(identical(contents(z), bytes),
              counts(z)[[2L]] >= ceiling(32 / 2))

    ## A self-copy larger than the staging chunk has memmove semantics in both
    ## overlap directions, even though both region methods return short.
    original <- as.raw(rep(0:255, length.out = 2000L))
    right <- new.test(original)
    expected <- original
    expected[601:1600] <- original[1:1000]
    moved <- call.test("C_altsxp_test_copy", right, 600, 0, 1000)
    stopifnot(identical(moved, 1000), identical(contents(right), expected))

    left <- new.test(original)
    expected <- original
    expected[1:1000] <- original[601:1600]
    moved <- call.test("C_altsxp_test_copy", left, 0, 600, 1000)
    stopifnot(identical(moved, 1000), identical(contents(left), expected))

    ## coerceVector(ALTSXP, VECSXP) invokes the default one-element subset once
    ## per element.  Each invocation must restore its R_alloc mark; the wide
    ## element size makes a leaked mark unambiguous without consuming memory.
    wide.bytes <- as.raw(0:63)
    probe <- call.test("C_altsxp_test_as_list_vmax",
                       new.test(wide.bytes, wide = TRUE))
    stopifnot(isTRUE(probe[[2L]]), length(probe[[1L]]) == length(wide.bytes),
              identical(vapply(probe[[1L]], function(e) contents(e)[[1L]],
                               raw(1L)), wide.bytes))

    ## A class that registers no Elt_type method takes the default, which has
    ## to name the package as well as the class: the registry keys a class on
    ## both, so without the package two classes that merely share a name
    ## would promise each other a layout neither knows anything about.
    stopifnot(typeof(new.kind("plain_byte", as.raw(1:4))) ==
              "altsxpTest::plain_byte")

    ## twin_byte reports short_byte's element type at 4096 bytes.  Sharing an
    ## element type deliberately is how a package class interoperates with a
    ## base one, but the promise includes the width, so nothing may read one
    ## at the other's.  The hash table also stages an element in a fixed stack
    ## buffer and refuses one that does not fit -- whichever operand it
    ## belongs to, not just the one HashTableSetup() was handed.
    narrow <- new.test(as.raw(1:4))
    twin <- new.kind("twin_byte", as.raw(1:4))
    stopifnot(typeof(narrow) == typeof(twin), !identical(narrow, twin))
    assertError(c(narrow, twin))
    assertError(match(narrow, twin))
    assertError(match(twin, narrow))

    ## R_ext/Altrep.h says a class need not provide Compare; order() reports
    ## that rather than inventing an order of its own.
    assertError(order(narrow))
    assertError(sort(narrow))

    ## The message has to say what is missing.  short_byte declares
    ## BITWISE_EQ, so identical() and the hash table are already served and
    ## only the ordering is absent; bare_byte declares neither, so it has no
    ## equality either and identical() -- which is not asking about order --
    ## must not be told that an ordering is what it wants.
    bare <- new.kind("bare_byte", as.raw(1:4))
    stopifnot(identical(bare, bare))          # same object short-circuits
    for (e in list(tryCatch(sort(narrow), error = identity),
                   tryCatch(order(narrow), error = identity))) {
        m <- conditionMessage(e)
        stopifnot(grepl("no 'Compare' method", m, fixed = TRUE),
                  grepl("cannot be ordered", m, fixed = TRUE),
                  !grepl("BITWISE_EQ", m, fixed = TRUE))
    }
    for (e in list(tryCatch(identical(bare, new.kind("bare_byte", as.raw(1:4))),
                            error = identity),
                   tryCatch(sort(bare), error = identity))) {
        m <- conditionMessage(e)
        stopifnot(grepl("no 'Compare' method", m, fixed = TRUE),
                  grepl("R_ALTREP_TRAITS_BITWISE_EQ", m, fixed = TRUE),
                  grepl("equality", m, fixed = TRUE))
    }
    ## A class that does not call itself numeric is not a subscript: the
    ## arms added for that gate on the NUMERIC trait, not on ALTSXP.
    stopifnot(!is.numeric(narrow))
    one <- new.test(as.raw(1L))   # [[ rejects a longer subscript first
    for (e in list(tryCatch((1:10)[narrow], error = identity),
                   tryCatch((1:10)[[one]], error = identity),
                   tryCatch(matrix(1:4, 2L)[narrow, 1L], error = identity)))
        stopifnot(inherits(e, "error"),
                  grepl("invalid subscript type", conditionMessage(e),
                        fixed = TRUE))

    ## A class whose bytes do not decide equality supplies its own Hash, and
    ## a Compare for the table to settle collisions with.  hash_byte reads its
    ## byte modulo 16, so 0x01 and 0x11 are one value in two spellings -- the
    ## shape a floating element type has with +0 and -0, where declaring
    ## BITWISE_EQ would be a lie.  Its elements are also 4096 bytes wide, and
    ## Hash and Compare read them where they lie, so the staging-buffer cap
    ## that stops wide_byte does not apply.
    h <- new.kind("hash_byte", as.raw(c(1, 17, 2, 1)))
    stopifnot(!is.numeric(h),
              ## by value, not by bytes: 0x11 counts as 0x01
              identical(contents(unique(h)), as.raw(c(1, 2))),
              identical(match(h, h), c(1L, 1L, 3L, 1L)),
              identical(duplicated(h), c(FALSE, TRUE, FALSE, TRUE)),
              identical(anyDuplicated(h), 2L),
              identical(h %in% new.kind("hash_byte", as.raw(17)),
                        c(TRUE, TRUE, FALSE, TRUE)),
              ## and Compare still orders it
              identical(contents(sort(h)), as.raw(c(1, 17, 1, 2))))

    ## the width cap is the byte route's, and only the byte route's
    e <- tryCatch(unique(new.test(as.raw(1:4), wide = TRUE)), error = identity)
    stopifnot(grepl("4096 bytes", conditionMessage(e), fixed = TRUE))

    ## The hash table refuses for two unrelated reasons, and they want
    ## different things done about them: no BITWISE_EQ, or an element wider
    ## than the staging buffers.  One message for both said neither.
    e <- tryCatch(unique(bare), error = identity)
    stopifnot(grepl("R_ALTREP_TRAITS_BITWISE_EQ", conditionMessage(e),
                    fixed = TRUE),
              grepl("'Hash' method", conditionMessage(e), fixed = TRUE))
    e <- tryCatch(unique(new.test(as.raw(1:4), wide = TRUE)), error = identity)
    stopifnot(grepl("4096 bytes", conditionMessage(e), fixed = TRUE),
              !grepl("BITWISE_EQ", conditionMessage(e), fixed = TRUE))

    ## the shape methods alone still carry everything the header promises
    ## them for: subsetting, concatenation, duplication, serialization
    stopifnot(identical(contents(bare[2:3]), as.raw(2:3)),
              identical(contents(c(bare, bare)), as.raw(c(1:4, 1:4))),
              identical(contents(rev(bare)), as.raw(4:1)),
              identical(contents(unserialize(serialize(bare, NULL))),
                        as.raw(1:4)))

    ## cmp_byte has one, so sort() reaches the write-back that hands the
    ## ordered elements to Set_region -- two at a time, here.
    jumbled <- as.raw(c(3, 1, 4, 1, 5, 9, 2, 6))
    cmp <- new.kind("cmp_byte", jumbled)
    stopifnot(identical(contents(sort(cmp)),
                        as.raw(sort(as.integer(jumbled)))),
              identical(contents(cmp[3:5]), jumbled[3:5]),
              identical(match(cmp, cmp), match(jumbled, jumbled)))

    ## Reaching a class by name is opt-in: an element type names a
    ## representation and two classes may share one (twin_byte shares
    ## short_byte's), so R cannot pick the owner by itself.  None of these
    ## classes has claimed a name, so none is reachable by one -- including
    ## the qualified name the default Elt_type builds for plain_byte.
    b <- new.test(as.raw(1:4))
    plain <- new.kind("plain_byte", as.raw(1:4))
    stopifnot(typeof(b) == "altsxp_test_byte",
              typeof(plain) == "altsxpTest::plain_byte")
    for (nm in c("altsxp_test_byte", "altsxpTest::plain_byte")) {
        assertError(vector(nm, 2L))
        assertError(as.vector(as.raw(1:2), nm))
        assertError(`storage.mode<-`(as.raw(1:2), nm))
    }
    ## the identity resolution needs no registration, and still works
    stopifnot(identical(as.vector(b, typeof(b)), b),
              identical(as.vector(plain, typeof(plain)), plain))

    ## Set_na_region() has to invalidate whatever the class cached about its
    ## contents, exactly as a writable Dataptr does.  int64 wrote straight to
    ## its payload instead, so a no-NA answer computed earlier survived the
    ## NAs and anyNA() then contradicted is.na().  No base caller reaches
    ## set_na_region on an object that has been asked yet, so it takes the
    ## public entry point a package would use.
    ## A class that shares base int64's element type is not one of int64's
    ## objects: the shared name promises the C type of an element, not where
    ## the other class keeps it.  fake_int64 stores one byte per element, so
    ## reading its data1 as an int64 array runs off the end of it.  Its
    ## element for byte b is b repeated across all eight bytes.
    fake <- new.kind("fake_int64", as.raw(c(1, 2)))
    twin64 <- c(as.int64("72340172838076673"),    # 0x0101010101010101
                as.int64("144680345676153346"))  # 0x0202020202020202
    stopifnot(identical(typeof(fake), "int64"),
              identical(length(fake), 2L),
              ## identical() compares through Compare, since fake_int64 does
              ## not declare BITWISE_EQ -- and it must agree both ways round
              identical(twin64, fake),
              identical(fake, twin64),
              !identical(as.int64(1:2), fake))
    ## min()/max() fold across arguments by Compare too
    stopifnot(identical(format(min(twin64, fake)), "72340172838076673"),
              identical(format(max(twin64, fake)), "144680345676153346"))

    ## Which hash a table is keyed with is a property of the class, but the
    ## table is built from one operand and probed with another: mod_byte
    ## reports cmp_byte's element type, so the two are matchable, yet one
    ## hashes for itself and the other by its bytes.  That used to key the
    ## table one way and probe it the other, finding nothing and saying so
    ## with an ordinary NA.
    cmpb <- new.kind("cmp_byte", as.raw(c(1, 2, 3)))
    modb <- new.kind("mod_byte", as.raw(c(1, 2, 3)))
    stopifnot(identical(typeof(cmpb), typeof(modb)))
    for (e in list(quote(match(modb, cmpb)), quote(match(cmpb, modb)),
                   quote(modb %in% cmpb), quote(cmpb %in% modb))) {
        err <- tryCatch(eval(e), error = identity)
        stopifnot(inherits(err, "error"),
                  grepl("decides equality by its bytes",
                        conditionMessage(err), fixed = TRUE))
    }
    ## each class on its own is unaffected, mod_byte's own hash included:
    ## 0x01 and 0x11 are one value to it, two to a byte-wise class
    stopifnot(identical(match(modb, modb), 1:3),
              identical(match(cmpb, cmpb), 1:3),
              identical(match(new.kind("mod_byte", as.raw(c(0x01, 0x11))),
                              new.kind("mod_byte", as.raw(c(0x01, 0x11)))),
                        c(1L, 1L)),
              identical(match(new.kind("cmp_byte", as.raw(c(0x01, 0x11))),
                              new.kind("cmp_byte", as.raw(c(0x01, 0x11)))),
                        c(1L, 2L)))

    ## It is the BITWISE_EQ bit that chooses the route, not the presence of a
    ## Hash method: both_byte declares the bit *and* registers hash_byte's
    ## modulo-16 pair, and the header says the bit wins.  So it must still
    ## interoperate with cmp_byte, which has the bit and no Hash -- and its
    ## own Hash must never be consulted, or 0x01 and 0x11 would merge.
    bothb <- new.kind("both_byte", as.raw(c(1, 2, 3)))
    stopifnot(identical(typeof(bothb), typeof(cmpb)),
              identical(match(bothb, cmpb), 1:3),
              identical(match(cmpb, bothb), 1:3),
              identical(bothb %in% cmpb, rep(TRUE, 3)),
              identical(match(new.kind("both_byte", as.raw(c(0x01, 0x11))),
                              new.kind("both_byte", as.raw(c(0x01, 0x11)))),
                        c(1L, 2L)),
              identical(match(new.kind("both_byte", as.raw(c(0x01, 0x11))),
                              new.kind("cmp_byte", as.raw(c(0x01, 0x11)))),
                        c(1L, 2L)))
    ## mod_byte disagrees with both of them, and still says so
    for (e in list(quote(match(modb, bothb)), quote(match(bothb, modb))))
        stopifnot(inherits(tryCatch(eval(e), error = identity), "error"))

    ## the width cap goes with the route: a wide class escapes it by
    ## supplying Hash and Compare *instead of* the bit, not as well as it
    stopifnot(identical(match(new.test(as.raw(1:6)), new.test(as.raw(1:6))),
                        1:6),
              identical(match(new.kind("hash_byte", as.raw(1:6)),
                              new.kind("hash_byte", as.raw(1:6))), 1:6))
    err <- tryCatch(match(new.kind("wide_byte", as.raw(1:6)),
                          new.kind("wide_byte", as.raw(1:6))), error = identity)
    stopifnot(inherits(err, "error"),
              grepl("more than", conditionMessage(err), fixed = TRUE))

    ## The default Elt_type built its "pkg::class" symbol on every call, with
    ## a one-slot cache that alternating two classes defeats -- and the header
    ## promises the method does not allocate.  It is now built once per class,
    ## so the answer has to stay right no matter what is asked in between.
    plain2 <- new.kind("plain_byte", as.raw(1:4))
    other <- new.test(as.raw(1:4))
    gctorture(TRUE)
    for (i in 1:20)
        stopifnot(identical(typeof(plain2), "altsxpTest::plain_byte"),
                  identical(typeof(other), "altsxp_test_byte"),
                  identical(typeof(plain2), typeof(new.kind("plain_byte",
                                                            as.raw(1:2)))))
    gctorture(FALSE)

    set.na <- function(x, i, n) call.test("C_altsxp_test_set_na", x, i, n)
    for (v in list(as.int64(1:5), as.uint64(1:5))) {
        stopifnot(!anyNA(v), !is.unsorted(v))    # caches no-NA and sortedness
        stopifnot(identical(set.na(v, 0, 2), 2))
        stopifnot(anyNA(v),
                  identical(is.na(v), c(TRUE, TRUE, FALSE, FALSE, FALSE)),
                  is.na(is.unsorted(v)),
                  identical(as.double(sort(v)), c(3, 4, 5)),
                  identical(as.double(min(v, na.rm = TRUE)), 3))
    }
})

cat("altsxp tests OK\n")

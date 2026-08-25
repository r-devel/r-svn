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
    ## nothing to reduce warns, as it does for the base types
    assertWarning(min(int64()))
    stopifnot(is.na(suppressWarnings(min(int64()))))
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

    ## an opaque element type has no zero R could name, so its elements are
    ## NA -- and never the uninitialised payload New() hands back
    v <- .allocVectorLike(as.int64(1L), 3L)
    stopifnot(typeof(v) == "int64", length(v) == 3L, all(is.na(v)))
    m <- .allocMatrixLike(as.int64(1L), 2L, 3L)
    stopifnot(typeof(m) == "int64", identical(dim(m), c(2L, 3L)),
              all(is.na(m)))

    ## the prototype's NA domain rides along, so a vector that gave up its
    ## NA has no element R could invent -- unless none is wanted
    assertError(.allocVectorLike(as.int64(1L, na = FALSE), 3L))
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

cat("altsxp tests OK\n")

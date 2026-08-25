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

## other integer types promote to int64
for (e in list(3L, TRUE, as.raw(3)))
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

cat("altsxp tests OK\n")

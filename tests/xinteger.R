## Tests for 'xinteger' vectors (ALTSXP): fixed-width integer vectors.
##
## Tests should be written to raise an error on test failure.

u   <- as.xinteger(c("1", "2", "3"), 8L, "unsigned")
s   <- as.xinteger(c("-1", "0", "1"), 8L, "signed")
b1  <- as.xinteger("42", 8L, "unsigned")
un  <- as.xinteger(c("1", NA), 8L, "unsigned")
nn  <- as.xinteger(c("1", "2"), 8L, "unsigned", na = FALSE)

### type identity

stopifnot(is.xinteger(u), is.xinteger(s),
	  !is.xinteger(1:3), !is.xinteger(1:3), !is.raw(u),
	  typeof(u) == "alt", typeof(s) == "alt",
	  storage.mode(u) == "uint64", storage.mode(s) == "int64",
	  identical(class(u), "uint64"), identical(class(s), "int64"),
	  is.atomic(u), is.vector(u), is.vector(u, mode(u)),
	  is.vector(u, storage.mode(u)), is.numeric(u), is.numeric(s),
	  mode(u) == "numeric", mode(s) == "numeric",
	  xintegerWidth(u) == 8L, xintegerKind(u) == "unsigned", xintegerHasNA(u),
	  !xintegerHasNA(nn),
	  length(u) == 3L,			# elements, not bytes
	  length(xintegerRaw(u)) == 24L)

## Numeric-mode lookup agrees with mode() and is.numeric().  A detailed
## storage-mode name names the whole element type, so it matches that
## type and no other: 'u' is a uint64.
e <- list2env(list(x = u))
stopifnot(exists("x", e, mode = "numeric"),
	  identical(get("x", e, mode = "numeric"), u),
	  identical(mget("x", e, mode = "numeric")$x, u),
	  exists("x", e, mode = "xinteger"),
	  identical(get("x", e, mode = "xinteger"), u),
	  identical(mget("x", e, mode = "xinteger")$x, u),
	  exists("x", e, mode = "uint64"),
	  identical(get("x", e, mode = "uint64"), u),
	  !exists("x", e, mode = "int64"),	# a kind apart
	  inherits(tryCatch(exists("x", e, mode = "uint8"),
			    error = identity), "error"),
	  inherits(tryCatch(mget("x", e, mode = "uint8",
				 ifnotfound = list(NULL)), error = identity),
		   "error"))

## Public constructors describe the values; ALTSXP remains an internal
## storage detail.
stopifnot(identical(as.int64(c("1", "2", "3")), as.xinteger(1:3, 8L, "signed")),
	  storage.mode(as.uint64("1")) == "uint64",
	  inherits(tryCatch(as.xinteger(-1L, 1L, "signed"),
			    error = identity), "error"),
	  inherits(tryCatch(as.xinteger("1", 16L, "unsigned"),
			    error = identity), "error"))

## Assigning the structural typeof() back does not install an explicit
## class, as for the other atomic types, and leaves the semantic implicit
## class supplied by the width and kind unchanged.
cu <- u; class(cu) <- typeof(cu)
stopifnot(identical(cu, u), identical(class(cu), "uint64"))

## Compiled is.numeric() must agree with the primitive path.
isnum <- compiler::cmpfun(function(x) is.numeric(x))
stopifnot(isnum(u), isnum(s))

## Entry points whose C implementations use the older isNumeric()
## predicate must agree with the public is.numeric() answer.  These
## operations are double- or complex-valued, so they use the same checked
## conversion as mixed arithmetic rather than fixed-width kernels.
local({
    x <- as.int64(1:4)
    names(x) <- letters[1:4]
    m <- x; dim(m) <- c(2L, 2L)

    stopifnot(identical(Re(x), setNames(as.double(1:4), letters[1:4])),
	      identical(Im(x), setNames(double(4), letters[1:4])),
	      identical(Mod(as.int64(c("-2", "3"))), c(2, 3)),
	      identical(Arg(as.int64(c("-2", "0", "3"))), c(pi, 0, 0)),
	      identical(Conj(x), setNames(as.double(1:4), letters[1:4])),
	      identical(m %*% m, matrix(c(7, 10, 15, 22), 2L, 2L)),
	      identical(crossprod(m), crossprod(matrix(1:4, 2L, 2L))),
	      identical(tcrossprod(m), tcrossprod(matrix(1:4, 2L, 2L))),
	      identical(stats::fft(x), stats::fft(setNames(1:4, letters[1:4]))),
	      identical(stats::mvfft(m), stats::mvfft(matrix(1:4, 2L, 2L))),
	      identical(rowSums(m), c(4, 6)),
	      identical(colSums(m), c(3, 7)),
	      identical(rowMeans(m), c(2, 3)),
	      identical(colMeans(m), c(1.5, 3.5)),
	      identical(rowsum(m, c("a", "b")),
			rowsum(matrix(as.double(1:4), 2L, 2L), c("a", "b"))))

    g <- c("a", "a", "b", "b")
    xd <- unname(x)
    stopifnot(identical(rowsum(xd, g), rowsum(as.double(1:4), g)),
	      identical(rowsum(data.frame(a = xd, b = 4:1), g),
			rowsum(data.frame(a = as.double(1:4), b = 4:1), g)),
	      identical(dnorm(xd), dnorm(1:4)),
	      identical(dchisq(xd, 2), dchisq(1:4, 2)),
	      identical(dhyper(xd, 2, 3, 2), dhyper(1:4, 2, 3, 2)),
	      identical(compiler::cmpfun(function(z) dnorm(z, 0, 1))(xd),
			dnorm(1:4)))

    set.seed(1729); r1 <- rchisq(4, xd)
    set.seed(1729); r2 <- rchisq(4, 1:4)
    set.seed(1729); r3 <- rnorm(4, xd, 1)
    set.seed(1729); r4 <- rnorm(4, 1:4, 1)
    set.seed(1729); r5 <- rhyper(4, xd, as.uint64(4:1), as.uint64(2))
    set.seed(1729); r6 <- rhyper(4, 1:4, 4:1, 2)
    stopifnot(identical(r1, r2), identical(r3, r4), identical(r5, r6))

    ## These two answers use only the fact that the operand is real and
    ## its sign, so no conversion to double -- and no precision warning --
    ## is involved even when the stored integer is wider than 2^53.
    oo <- options(warn = 2)
    stopifnot(identical(Im(as.uint64("9007199254740993")), 0),
	      identical(Arg(as.uint64("9007199254740993")), 0),
	      identical(Arg(as.int64("-9007199254740993")), pi))
    options(oo)

    w <- NULL
    withCallingHandlers(Re(as.uint64("9007199254740993")), warning = function(e) {
	w <<- conditionMessage(e)
	invokeRestart("muffleWarning")
    })
    stopifnot(length(w) == 1L, grepl("lose precision", w))
})

## A double operand promotes numeric fixed-width integers, warning only
## when their actual values cannot be represented exactly.
stopifnot(identical(u + 1, c(2, 3, 4)),
	  identical(1 + u, c(2, 3, 4)),
	  identical(u + (1 + 2i), c(2 + 2i, 3 + 2i, 4 + 2i)),
	  identical((1 + 2i) + u, c(2 + 2i, 3 + 2i, 4 + 2i)),
	  identical(u / 2L, c(.5, 1, 1.5)),
	  identical(2L / u, c(2, 1, 2/3)),
	  identical(u ^ 2L, c(1, 4, 9)),
	  identical(u ^ u, c(1, 4, 27)),
	  identical(names(setNames(u, letters[1:3]) + 0), letters[1:3]),
	  identical(c(u, 4), c(1, 2, 3, 4)),
	  identical(u < 2.5, c(TRUE, TRUE, FALSE)),
	  identical(sum(u, 1), 7), identical(max(u, 2.5), 3),
	  identical(pmin(u, 1.5), c(1, 1.5, 1.5)),
	  identical(pmax(u, 1.5), c(1.5, 2, 3)),
	  identical(quantile(u, names = FALSE), c(1, 1.5, 2, 2.5, 3)))
lost <- as.uint64("9007199254740993")
stopifnot(grepl("lose precision", conditionMessage(tryCatch(
	  { withCallingHandlers(lost + 0, warning = function(w) stop(w)); NULL },
	  warning = identity, error = identity))))

## The type gate still runs before recycling or checked promotion.  In
## particular, a node on which XLENGTH() is invalid is rejected without a
## precision warning from the other operand.
nw <- 0L
bad <- tryCatch(
    withCallingHandlers(lost + sum, warning = function(w) {
	nw <<- nw + 1L
	invokeRestart("muffleWarning")
    }),
    error = identity)
stopifnot(inherits(bad, "error"), nw == 0L)

## Coercion belongs to the selected arithmetic dispatch arm, after structural
## checks.  Under warn = 2 a non-conformable array must therefore report
## its shape error rather than an otherwise irrelevant precision warning.
ow <- options(warn = 2)
shape.error <- tryCatch(
    matrix(rep(lost, 4L), 2L, 2L) + matrix(0, 4L, 1L),
    error = conditionMessage,
    finally = options(ow))
stopifnot(grepl("non-conformable arrays", shape.error, fixed = TRUE))

## Comparison and matching use the exact binary value of a double; the
## fixed-width operand is never rounded to make a logical answer.
stopifnot(lost > 9007199254740992,
	  !(lost == 9007199254740992),
	  lost < 9007199254740994,
	  !(lost %in% c(9007199254740992, 9007199254740994)),
	  identical(as.uint64(c("1", "2")) %in% c(1, 2.5),
		    c(TRUE, FALSE)),
	  identical(c(u, "key"), c("1", "2", "3", "key")))

## A character operand promotes, as it does for every other type and as
## it already does for c() and [<-: as.character() of an element is
## exact at every width, so nothing is lost, and ==, match() and %in%
## answer for the same pairs c() accepts.  What they inherit with it is
## string collation, exactly as an integer operand does.
stopifnot(identical(u == "1", c(TRUE, FALSE, FALSE)),
	  identical(u != "1", c(FALSE, TRUE, TRUE)),
	  identical(u == "1", as.integer(1:3) == "1"),
	  identical("1" %in% u, TRUE),
	  identical(match(c("2", "9"), u), c(2L, NA)),
	  identical(u %in% c("1", "3"), c(TRUE, FALSE, TRUE)),
	  identical(sort(union(u, "9")), sort(c("1", "2", "3", "9"))),
	  ## setdiff() subsets x rather than coercing it, so the answer
	  ## keeps its type -- as it does for setdiff(1:3, "2")
	  identical(setdiff(u, "2"), u[c(1L, 3L)]),
	  identical(intersect(u, "2"), "2"),
	  ## the wide values a double could not carry survive the trip
	  identical(as.uint64("18446744073709551614") ==
		    "18446744073709551614", TRUE),
	  identical(as.uint64("18446744073709551614") ==
		    "18446744073709551615", FALSE),
	  ## and an empty operand is settled the same way, not refused
	  identical(as.uint64(character(0)) == "1", logical(0)))

## Unit-step sequences stay exact above 2^53, and the Math group behaves
## numerically without routing abs() or sign() through double.
ends <- as.uint64(c("9223372036854775801", "9223372036854775805"))
want <- c("9223372036854775801", "9223372036854775802",
	  "9223372036854775803", "9223372036854775804",
	  "9223372036854775805")
## seq.int(<one numeric operand>) is 1:x, as it is for an integer or a
## double and as seq() is for anything whose mode() is "numeric"; the
## length rule that answers 1 here is for the operands that are not.
stopifnot(identical(seq.int(as.int64(5)), 1:5),
	  identical(seq.int(as.uint64(4)), 1:4),
	  identical(seq(as.int64(5)), 1:5),
	  identical(seq.int(as.int64(c(5, 6))), 1:2))

stopifnot(identical(as.character(seq(ends[1], ends[2])), want),
	  identical(as.character(ends[1]:ends[2]), want),
	  identical(as.character(abs(as.int64(c("-2", "0", "3")))),
		    c("2", "0", "3")),
	  identical(sign(as.int64(c("-2", "0", "3"))), c(-1, 0, 1)),
	  identical(sqrt(as.uint64(c("4", "9"))), c(2, 3)),
	  identical(log(as.uint64(c("1", "2"))), log(c(1, 2))),
	  identical(cumvar(as.uint64(c("1", "2", "3"))), c(NA, .5, 1)),
	  identical(unname(quantile(as.uint64(c("1", "2", "3", "4")))),
		    c(1, 1.75, 2.5, 3.25, 4)))

## mean() warns where the total needs more than a double's 53 bits and
## the result is then not the exact rational mean.  Where the total
## fits, the single division left is correctly rounded and the fixed
## width lost nothing: 7/3 is not an integer whatever the operands were
## stored in, and mean(c(1L, 2L, 4L)) says nothing about it either.
## The last total below does need 54 bits, but its mean is exact.
## options(warn = 2) is the point of the check -- a warning is fatal
## there, and mean() has to be usable under it.
mw <- tryCatch({ withCallingHandlers(mean(ends), warning = function(w) stop(w)); NULL },
	       warning = identity, error = identity)
opw <- options(warn = 2)
quiet.means <- tryCatch(c(mean(as.int64(c(1, 2, 4))),
			  mean(as.uint64(c("1", "2", "4"))),
			  mean(as.uint64(rep("4503599627370497", 3L)))),
                        finally = options(opw))
stopifnot(grepl("loses precision", conditionMessage(mw)),
	  identical(quiet.means,
		    c(mean(c(1L, 2L, 4L)), mean(c(1L, 2L, 4L)), 4503599627370497)))

## the accessors are about 'xinteger' vectors, and say so in R's terms
## rather than naming the C entry point
for (f in list(xintegerWidth, xintegerKind, xintegerHasNA, xintegerRaw))
    stopifnot(grepl("'x' must be an 'xinteger' vector",
		    tryCatch(f(1L), error = conditionMessage)))

### subsetting and subassignment

stopifnot(identical(as.character(u[2:3]), c("2", "3")),
	  identical(as.character(rev(u)), c("3", "2", "1")),
	  as.character(u[[1L]]) == "1")

x <- u
x[2L] <- as.xinteger("9", 8L, "unsigned")
stopifnot(identical(as.character(x), c("1", "9", "3")),
	  identical(as.character(u), c("1", "2", "3")))	# copy on write

## Character is the reversible, lossless meeting type for combination,
## including subassignment as well as c().
x <- u
x[2L] <- "9"
stopifnot(identical(x, c("1", "9", "3")),
	  identical(as.character(u), c("1", "2", "3")))

## a >= 3-dimensional array must be readable, not only writable
a <- as.xinteger(as.character(1:8), 8L, "unsigned")
dim(a) <- c(2L, 2L, 2L)
stopifnot(as.character(a[1, 1, 1]) == "1",
	  identical(as.character(a[1, , ]), c("1", "3", "5", "7")))
a[1, 1, 1] <- as.xinteger("99", 8L, "unsigned")
stopifnot(as.character(a[1, 1, 1]) == "99")

### matrices, and the asplit() path behind unique.matrix()

m  <- u[c(1, 1, 2, 2)];	dim(m)  <- c(2L, 2L)
mi <- c(1L, 1L, 2L, 2L); dim(mi) <- c(2L, 2L)
stopifnot(nrow(unique(m)) == nrow(unique(mi)),
	  identical(duplicated(m), duplicated(mi)),
	  anyDuplicated(m) == anyDuplicated(mi),
	  length(asplit(m, 1L)) == 2L, is.xinteger(asplit(m, 1L)[[1L]]))
d <- u[c(1, 2, 1, 2)]; dim(d) <- c(2L, 2L)
stopifnot(nrow(unique(d)) == 2L)

### combining

stopifnot(identical(as.character(c(u, u)), rep(c("1", "2", "3"), 2L)),
	  ## names get the seqno form every other atomic type uses ...
	  identical(names(c(a = u)), names(c(a = as.raw(1:3)))),
	  ## ... and a zero-length argument must not miscount the slots
	  identical(names(c(a = xinteger(0L, 8L, "unsigned"), b = 1L)), "b"),
	  ## NULL contributes nothing, as it does for every other type
	  ncol(cbind(xinteger(0L, 8L, "unsigned"), NULL)) ==
	  ncol(cbind(complex(0), NULL)))

## width and kind are part of the type: combining across them is an error,
## not a silent promotion
stopifnot(inherits(tryCatch(as.xinteger("1", 4L, "unsigned"),
			    error = identity), "error"),
	  inherits(tryCatch(c(u, s), error = identity), "error"),
	  inherits(tryCatch(c(u, nn), error = identity), "error"))

### the empty vector

for (e in list(xinteger(0L, 8L, "unsigned"), xinteger(0L, 8L, "signed"),
	       xinteger(0L, 8L, "signed", na = FALSE))) {
    stopifnot(is.xinteger(e), length(e) == 0L,
	      identical(unique(e), e), length(duplicated(e)) == 0L,
	      ## print() names the type it printed, as dput() does; "xinteger(0)"
	      ## would be a valid call producing a different object
	      identical(capture.output(print(e)), capture.output(dput(e))))
}

### coercion

stopifnot(identical(as.integer(u), 1:3),
	  identical(as.double(u), c(1, 2, 3)),
	  identical(as.raw(u), as.raw(1:3)),
	  identical(as.character(u), c("1", "2", "3")),
	  identical(as.complex(u), complex(real = 1:3)),
	  identical(as.logical(as.xinteger(c("0", "1"), 8L, "unsigned")),
		    c(FALSE, TRUE)),
	  is.na(as.logical(as.xinteger(NA_character_, 8L, "unsigned"))),
	  length(as.expression(u)) == 3L, is.xinteger(as.expression(u)[[1L]]),
	  length(as.list(u)) == 3L, is.xinteger(as.list(u)[[1L]]),
	  length(as.pairlist(u)) == 3L, is.xinteger(as.pairlist(u)[[1L]]))

## Logical operators use the logical coercion the type already exposes.
lx <- as.uint64(c("0", "1", NA))
stopifnot(identical(!lx, c(TRUE, FALSE, NA)),
	  identical(lx & TRUE, c(FALSE, TRUE, NA)),
	  identical(lx | FALSE, c(FALSE, TRUE, NA)),
	  identical(lx[1L] && TRUE, FALSE),
	  identical(lx[2L] && TRUE, TRUE),
	  is.na(lx[3L] && TRUE),
	  identical(lx[1L] || FALSE, FALSE),
	  identical(lx[2L] || FALSE, TRUE),
	  is.na(lx[3L] || FALSE))
lm <- matrix(as.uint64(c("0", "1", "1", "0")), 2L)
stopifnot(identical(dim(!lm), dim(lm)),
	  identical(dim(lm & TRUE), dim(lm)))
lf <- compiler::cmpfun(function(x) {
    c(!x[1L], x[1L] & TRUE, x[2L] | FALSE,
	x[1L] && TRUE, x[2L] || FALSE)
})
stopifnot(identical(lf(lx), c(TRUE, FALSE, TRUE, FALSE, TRUE)))

x <- u
storage.mode(x) <- "logical"
stopifnot(identical(x, c(TRUE, TRUE, TRUE)))

## as.xinteger() and as.vector() drop attributes on every path, including the
## one where nothing needs converting; storage.mode<- keeps them
nx <- as.xinteger(c(a = 1L, b = 2L), 8L, "unsigned")
names(nx) <- c("a", "b")
stopifnot(is.null(names(as.xinteger(nx, 8L, "unsigned"))),	# no change needed
	  is.null(names(as.xinteger(nx, 8L, "signed"))),	# kind changed
	  is.null(names(as.vector(nx, "uint64"))),
	  { y <- nx; storage.mode(y) <- "uint64"; identical(names(y), c("a","b")) })

### scalar coercion: control flow must not hit an internal error

stopifnot(if (b1) TRUE else FALSE,
	  identical(seq_len(as.xinteger("4", 8L, "unsigned")), 1:4),
	  identical(rep(1L, as.xinteger("2", 8L, "unsigned")), c(1L, 1L)),
	  identical(matrix(1:4, nrow = as.int64(2)), matrix(1:4, nrow = 2L)),
	  identical(matrix(1:4, ncol = as.uint64(2)), matrix(1:4, ncol = 2L)))

### the missing-value predicates

stopifnot(identical(is.na(un), c(FALSE, TRUE)),
	  identical(is.finite(un), c(TRUE, FALSE)),
	  identical(is.nan(un), c(FALSE, FALSE)),
	  identical(is.infinite(un), c(FALSE, FALSE)),
	  all(is.finite(nn)),
	  identical(lengths(u), c(1L, 1L, 1L)))

### ordering

stopifnot(identical(sort.int(as.int64(c(3, 1, 2)), method = "quick"),
		    as.int64(1:3)))
qi <- sort.int(as.int64(c(3, 1, 2)), method = "quick", index.return = TRUE)
stopifnot(identical(qi$x, as.int64(1:3)), identical(qi$ix, c(2L, 3L, 1L)))

set.seed(1)
for (i in 1:50) {
    n <- sample(2:12, 1L)
    v <- as.xinteger(as.character(sample(1000L, n)), 8L, "unsigned")
    k <- sample(seq_len(n), 1L)
    ## the partial sort must agree with the full one at the pivot
    stopifnot(identical(as.character(sort(v, partial = k)[k]),
			as.character(sort(v)[k])),
	      !is.unsorted(as.numeric(as.character(sort(v)))))
}
## values beyond what a double can name still order correctly.  na = FALSE
## because the all-ones pattern is the reserved NA when NA is representable,
## so only there is the full range of the width available.
big <- as.xinteger(c("18446744073709551615", "18446744073709551614"),
		8L, "unsigned", na = FALSE)
stopifnot(identical(as.character(sort(big)),
		    c("18446744073709551614", "18446744073709551615")),
	  big[1L] > big[2L],
	  ## and with NA representable the top of the range is reserved
	  is.na(suppressWarnings(
	      as.xinteger("18446744073709551615", 8L, "unsigned"))))

### arithmetic keeps full precision

p <- as.xinteger("9007199254740993", 8L, "unsigned")	# 2^53 + 1
stopifnot(as.character(p + 1L) == "9007199254740994",
	  as.character(p * 2L) == "18014398509481986",
	  ## the same value through a double would have lost the low bit
	  as.character(as.xinteger(as.character(as.numeric(as.character(p))),
				8L, "unsigned")) != as.character(p))

stopifnot(as.character(sum(u)) == "6", as.character(prod(u)) == "6",
	  as.character(min(u)) == "1", as.character(max(u)) == "3",
	  as.character(min(s)) == "-1")

## An out-of-range operand is discarded by na.rm before it becomes NA,
## so a successful answer must remain usable under options(warn = 2).
ow <- options(warn = 2)
quiet.sum <- tryCatch(sum(as.uint64(1L), -1L, na.rm = TRUE),
		      finally = options(ow))
stopifnot(identical(quiet.sum, as.uint64(1L)))

## min/max of nothing: every other type warns and returns +/-Inf.  There is
## no Inf here, so NA stands in where NA exists, and only where it does not
## is there nothing to return.
stopifnot(is.na(suppressWarnings(min(xinteger(0L, 8L, "unsigned")))),
	  is.na(suppressWarnings(max(xinteger(0L, 8L, "unsigned")))),
	  grepl("returning NA",
		tryCatch(min(xinteger(0L, 8L, "unsigned")),
			 warning = conditionMessage)),
	  grepl("cannot represent NA",
		tryCatch(min(xinteger(0L, 8L, "unsigned", na = FALSE)),
			 error = conditionMessage)))

### bitwise operations

h <- as.xinteger("255", 8L, "unsigned")
stopifnot(as.character(bitwAnd(h, as.xinteger("15", 8L, "unsigned"))) == "15",
	  as.character(bitwOr(as.xinteger("240", 8L, "unsigned"),
			      as.xinteger("15", 8L, "unsigned"))) == "255",
	  as.character(bitwShiftL(as.xinteger("1", 8L, "unsigned"), 63L)) ==
	  "9223372036854775808",
	  ## as bitwShiftL(1L, 32L) is NA
	  is.na(bitwShiftL(as.xinteger("1", 8L, "unsigned"), 64L)),
	  ## but a type with no NA has nothing to return, and says which
	  ## argument was at fault
	  grepl("shift out of range",
		tryCatch(bitwShiftL(nn, 64L), error = conditionMessage)),
	  identical(tryCatch(bitwShiftL(1L, as.uint64(2L)),
			     error = conditionMessage),
		    "invalid 'b' argument"))

## The documentation's network-byte-order example must not depend on the
## host byte order, and character conversion remains decimal rather than hex.
local({
    ip.bytes <- as.raw(c(0x20, 0x01, 0x0d, 0xb8, 0, 0, 0, 1))
    prefix.bytes <- as.raw(c(0x20, 0x01, 0x0d, 0xb8, 0, 0, 0, 0))
    mask.bytes <- as.raw(c(rep(0xff, 4), rep(0, 4)))
    proto <- xinteger(width = 8L, kind = "unsigned", na = FALSE)
    ip <- readBin(ip.bytes, proto, endian = "big")
    mask <- readBin(mask.bytes, proto, endian = "big")
    stopifnot(identical(writeBin(bitwAnd(ip, mask), raw(), endian = "big"),
			prefix.bytes))
})

### format() and printing

big2 <- as.xinteger("1234567890123", 8L, "unsigned")
stopifnot(format(big2) == "1234567890123",
	  format(big2, big.mark = ",") == "1,234,567,890,123",
	  nchar(format(big2, width = 20L)) == 20L)

## cat() must not truncate a wide element
wide <- as.uint64("18446744073709551614")
stopifnot(identical(capture.output(cat(wide)), as.character(wide)))

## sprintf(): %d keeps the whole width, and every part of the
## specification that describes the number rather than the field means
## what it means for an ordinary integer.  Checked against %d on the
## same values as integers, so the two cannot drift.
s2 <- as.int64(c(-1, 0, 1))
for (fmt in c("%d", "%i", "%+d", "% d", "%8d", "%-8d|", "%08d", "%+08d",
	      "%.0d", "%+.0d", "%.6d", "%+.6d", "%-08d|"))
	stopifnot(identical(sprintf(fmt, s2), sprintf(fmt, c(-1L, 0L, 1L))))
stopifnot(identical(sprintf("%d", wide), as.character(wide)),
	  identical(sprintf("%+d", wide), paste0("+", as.character(wide))),
	  identical(sprintf("%d and %s", s2, s2),
		    sprintf("%s and %s", c("-1", "0", "1"),
			    c("-1", "0", "1"))),
	  ## NA follows the integer arm, including string precision and padding
	  all(vapply(c("%5d|", "%05d|", "%.0d|", "%.3d|"), function(fmt)
	      identical(sprintf(fmt, as.uint64(NA)), sprintf(fmt, NA_integer_)),
	      logical(1L))),
	  ## the conversions this type does not define say so by name
	  grepl("xinteger", tryCatch(sprintf("%x", wide), error = conditionMessage)))

## formatC() defaults to an exact integer rendering.  Floating formats are
## an explicit request for the ordinary checked conversion to double.
stopifnot(identical(formatC(wide), as.character(wide)),
	  identical(formatC(wide, big.mark = ","),
		    "18,446,744,073,709,551,614"),
	  identical(formatC(s2, width = 6L, flag = "0"),
		    formatC(c(-1L, 0L, 1L), width = 6L, flag = "0")),
	  identical(formatC(as.int64(NA), width = 5L, flag = "0"),
		    formatC(NA_integer_, width = 5L, flag = "0")),
	  identical(formatC(as.int64(2), digits = 2L, format = "f"), "2.00"),
	  inherits(tryCatch(formatC(s2, width = 0L, digits = 0L),
			    error = identity), "error"))

### round trips

## ALTSXP uses ordinary ALTREP class/state serialization.  The built-in
## classes put endian-neutral bytes in the state, so stream version 3 is
## sufficient and no new standard-vector serialization format is needed.
streamVersion <- function(r) readBin(r[3:6], "integer", 1L, endian = "big")
for (v in list(u, s, un, nn, xinteger(0L, 8L, "unsigned"))) {
    stopifnot(identical(unserialize(serialize(v, NULL)), v),
	      identical(unserialize(serialize(v, NULL, version = 3)), v),
	      identical(eval(parse(text = paste(deparse(v), collapse = ""))), v),
	      identical(v[seq_along(v)], v),
	      streamVersion(serialize(v, NULL)) == 3L)
    f <- tempfile()
    saveRDS(v, f)
    stopifnot(identical(readRDS(f), v),
	      infoRDS(f)$version == 3L)
    unlink(f)
}

## Nested ALTSXP objects follow the same existing ALTREP path.
local({
    f <- tempfile()
    object <- list(sequence = 1:3, value = u,
		   attributed = structure(1:3, key = s))
    saveRDS(object, f)
    on.exit(unlink(f))
	stopifnot(infoRDS(f)$version == 3L,
		  identical(readRDS(f), object),
		  streamVersion(serialize(object, NULL)) == 3L)
})

### readBin()/writeBin()

f <- tempfile()
writeBin(as.xinteger("4294967297", 8L, "signed"), f)
stopifnot(as.character(readBin(f, "int64", 1L)) == "4294967297")
unlink(f)

## The names readBin() gained are the ten this type has, exactly.  One
## of the same shape but another width is not among them and keeps the
## meaning it had before: readBin() has always taken a length-one
## character vector it does not recognise as a prototype, i.e. as
## character(1), and that is what "int65" still is.
f <- tempfile()
writeBin(c("ab", "cd"), f)
stopifnot(identical(readBin(f, "int65", 2L), c("ab", "cd")),
	  identical(readBin(f, "int24", 2L), c("ab", "cd")))
unlink(f)

## a length-one character vector that is not a mode name is a prototype,
## which is the documented "an object whose mode will give the mode" form
f <- tempfile()
writeBin(c("ab", "cd"), f)
stopifnot(identical(readBin(f, character(1), 2L), c("ab", "cd")),
	  identical(readBin(f, character(), 2L), c("ab", "cd")),
	  identical(readBin(f, "", 2L), c("ab", "cd")),
	  identical(readBin(f, character(2), 2L), c("ab", "cd")),
	  ## A multi-element character vector is a prototype even when its
	  ## elements happen to spell newly-supported scalar mode names.
	  identical(readBin(f, c("int64", "int64"), 2L), c("ab", "cd")))
unlink(f)

### scan() accepts the same detailed names as prototypes

v <- scan(text = "9223372036854775807 -1 NA 0", what = "int64", quiet = TRUE)
d <- scan(text = "18446744073709551614 7",
          what = list(id = "uint64", value = 0L), quiet = TRUE)
stopifnot(storage.mode(v) == "int64",
	  identical(as.character(v), c("9223372036854775807", "-1", NA, "0")),
	  storage.mode(d$id) == "uint64",
	  identical(as.character(d$id), "18446744073709551614"),
	  identical(d$value, 7L),
	  ## only the ten supported names change meaning
	  identical(scan(text = "int64", what = "int24", quiet = TRUE), "int64"))

### vector(), storage.mode<- and mode<-

stopifnot(identical(vector("uint64", 2L), xinteger(2L, 8L, "unsigned")),
	  identical(as.vector(u, "int64"), as.xinteger(c("1","2","3"), 8L, "signed")),
	  inherits(tryCatch(vector("xinteger", 2L), error = identity), "error"),
	  identical(storage.mode(.vectorlike(u, 2L)), "uint64"),
	  !xintegerHasNA(.vectorlike(nn, 2L)))

## .vectorlike() and .arraylike() name a type by an object rather than by a
## string, which is the only way to ask for one no string spells: a
## storage mode carries this type's width and kind but not its sentinel
## policy.  Neither is specific to it -- taking every type the same way
## is what keeps apply(), diag() and tapply() from having to ask which
## one they were handed.  Literal expected values pin the fill
## independently of vector(): allocVector() leaves an atomic payload
## uninitialized, so a path that skipped vector()'s Memzero would return
## heap garbage -- and would do it only once the heap was dirty enough
## to notice.
ordinary <- list(
    list(logical(),    c(FALSE, FALSE, FALSE)),
    list(integer(),    c(0L, 0L, 0L)),
    list(double(),     c(0, 0, 0)),
    list(complex(),    c(0+0i, 0+0i, 0+0i)),
    list(character(),  c("", "", "")),
    list(raw(),        as.raw(c(0, 0, 0))),
    list(list(),       list(NULL, NULL, NULL)),
    list(expression(), expression(NULL, NULL, NULL)))
for (z in ordinary) {
    p <- z[[1L]]
    expected <- z[[2L]]
    stopifnot(identical(.vectorlike(p, 3L), expected),
	      identical(.arraylike(p, c(1L, 3L)),
		array(expected, c(1L, 3L))))
}
au <- .arraylike(u)
stopifnot(identical(.vectorlike(u, 3L), vector(storage.mode(u), 3L)),
	  identical(.vectorlike(u), xinteger(0L, 8L, "unsigned")),
	  ## and they carry the one thing a name cannot
	  !xintegerHasNA(.vectorlike(nn, 1L)),
	  xintegerHasNA(vector(storage.mode(nn), 1L)),
	  !xintegerHasNA(.arraylike(nn, c(2L, 3L))),
	  ## its shape is 'dim'; only the type comes from the object
	  identical(dim(.arraylike(u, c(2L, 3L))), c(2L, 3L)),
	  identical(storage.mode(.arraylike(u, c(2L, 3L))), "uint64"),
	  identical(dimnames(.arraylike(u, 2L, list(c("a", "b")))),
		    list(c("a", "b"))),
	  ## with dim omitted, .arraylike() follows array()'s length default
	  identical(dim(au), length(u)), length(au) == length(u),
	  storage.mode(au) == "uint64",
	  ## attributes do not stop a vector from donating its element type
	  identical(.vectorlike(matrix(1L, 1L, 1L), 2L), c(0L, 0L)),
	  ## an object is not a mode name, however long it is
	  inherits(tryCatch(vector(u, 2L), error = identity), "error"),
	  inherits(tryCatch(vector(b1, 2L), error = identity), "error"),
	  ## and a name is only ever the character vector it is, so
	  ## .vectorlike() needs no carve-out for one
	  identical(.vectorlike("uint64", 2L), c("", "")),
	  ## vector() is otherwise as it was
	  identical(vector("integer", 3L), integer(3L)),
	  identical(vector("character", 2L), c("", "")),
	  identical(vector(), logical()),
	  inherits(tryCatch(vector("intger", 3L), error = identity), "error"),
	  ## .vectorlike() wants a vector
	  inherits(tryCatch(.vectorlike(NULL, 3L), error = identity), "error"),
	  inherits(tryCatch(.vectorlike(pairlist(1), 3L),
			    error = identity), "error"),
	  inherits(tryCatch(.vectorlike(sum, 3L), error = identity), "error"))

## the callers that no longer have to ask
m0 <- matrix(nn, length(nn), 1L)[0L, , drop = FALSE]
stopifnot(length(apply(m0, 1L, identity)) == 0L,
	  !xintegerHasNA(diag(m0)),
	  is.xinteger(tapply(u, seq_along(u), function(z) z)[[1L]]),
	  ## unchanged for the ordinary types
	  identical(apply(matrix(1:6, 3L, 2L), 2L, sum), c(6L, 15L)),
	  identical(diag(matrix(0, 0L, 0L)), numeric(0)),
	  identical(diag(matrix(0L, 0L, 0L)), integer(0)))

x <- 1:3
storage.mode(x) <- "uint64"
stopifnot(is.xinteger(x), storage.mode(x) == "uint64")

x <- 1:3
mode(x) <- "uint64"			# no as.uint64(): goes to storage.mode<-
stopifnot(is.xinteger(x), storage.mode(x) == "uint64")

x <- u
mode(x) <- storage.mode(x)		# must be a no-op, not a conversion
stopifnot(identical(x, u))

## "xinteger" names no complete storage type
for (e in list(quote(as.vector(u, "xinteger")),
	       quote({x <- c("01","02"); mode(x) <- "xinteger"}),
	       quote({x <- c("01","02"); storage.mode(x) <- "xinteger"})))
    stopifnot(inherits(tryCatch(eval(e), error = identity), "error"))

### memory.profile() must have a slot for the type

stopifnot("alt" %in% names(memory.profile()))

### storage.mode<- is the one mode that changes the element count

## raw bytes regroup into width-byte elements, so names and dim would
## then describe more elements than the result has -- and printing or
## m[i, j] would read past the payload.  No other route reaches that
## state: dim<-, attr(, "dim")<- and structure() all reject it.
m <- matrix(as.raw(1:64), 8, 8)
storage.mode(m) <- "uint64"
stopifnot(length(m) == 8L, is.null(dim(m)), is.null(dimnames(m)))
nx <- as.raw(1:16); names(nx) <- letters[1:16]
storage.mode(nx) <- "uint64"
stopifnot(length(nx) == 2L, is.null(names(nx)))
## but a conversion that keeps the count keeps them
ny <- as.int64(1:4); names(ny) <- letters[1:4]
storage.mode(ny) <- "uint64"
stopifnot(identical(names(ny), letters[1:4]))

### an 'xinteger' right-hand side keeps the destination's attributes

## every other arm of SubassignTypeFix() coerces through coerceVector(),
## which carries them over; this one narrows into a fresh vector
b <- as.int64(9L)
m <- matrix(1:4, 2, 2); m[1, 1] <- b
stopifnot(identical(dim(m), c(2L, 2L)))
v <- c(a = 1L, b = 2L); v[1] <- b
stopifnot(identical(names(v), c("a", "b")))
o <- structure(1:3, class = "myclass"); o[1] <- b
stopifnot(is.object(o), identical(class(o), "myclass"))

## a list right-hand side promotes the destination, as for every other
## atomic type, and a NULL one takes the width and kind from the value
z <- as.xinteger(1:3, 8L, "unsigned"); z[1] <- list(1)
stopifnot(is.list(z), length(z) == 3L)
z <- NULL; z[1] <- as.xinteger("7", 8L, "unsigned")
stopifnot(is.xinteger(z), length(z) == 1L, as.character(z) == "7")

### ALTREP serialization through the ordinary class/state path
f <- tempfile()
save(list = "b", file = f, envir = environment(), version = 3)
e <- new.env(); load(f, envir = e)
stopifnot(identical(e$b, b))
unlink(f)

### comparison against a bound the type cannot hold

## it is not missing: it lies below or above every element, so the
## answer is determined and the filter idioms must not go quiet
u8 <- as.uint64(c("1", "2", "3"))
stopifnot(identical(u8 > -1L, rep(TRUE, 3)),
	  identical(-1L < u8, rep(TRUE, 3)),
	  identical(u8[u8 > -1L], u8), identical(which(u8 > -1L), 1:3),
	  ## an element that really is missing still answers NA
	  identical(as.uint64(c("1", NA)) > -1L, c(TRUE, NA)))

## min and max only compare, so a bound they cannot hold is ignored
## unless it wins outright, when the answer itself is out of range
stopifnot(as.character(max(u8, -1L)) == "3",
	  identical(as.character(pmax(u8, -1L)), c("1", "2", "3")))

## a width is part of the type, so min() and max() refuse the pairs c()
## refuses -- range() goes through c() and must not fail where they work
a8 <- as.uint64("10"); s4 <- as.int64("5")
for (e in list(quote(max(a8, s4)), quote(min(a8, s4)), quote(sum(a8, s4)),
	       quote(pmin(a8, s4)), quote(c(a8, s4))))
    stopifnot(inherits(tryCatch(eval(e), error = identity), "error"))
stopifnot(identical(as.character(range(a8, as.xinteger("3", 8L, "unsigned"))),
		    c("3", "10")))

## max() saw two non-missing arguments here, whatever order they came in
stopifnot(is.na(max(as.xinteger(c(NA, "1", "2"), 8L, "unsigned"))),
	  as.character(max(as.xinteger(c(NA, "1", "2"), 8L, "unsigned"),
			   na.rm = TRUE)) == "2")
op <- options(warn = 2)
stopifnot(is.na(max(as.xinteger(c(NA, "1"), 8L, "unsigned"))))
options(op)

### as.numeric() must give the nearest double

## accumulating byte by byte rounds at every step once the running total
## passes 2^53, and the errors compound to as much as a whole ulp
stopifnot(suppressWarnings(
	      as.numeric(as.xinteger("3119042104763040036", 8L, "unsigned"))) ==
	  3119042104763040256,
	  as.numeric(as.xinteger("12345", 8L, "unsigned")) == 12345,
	  as.numeric(as.xinteger("0", 8L, "unsigned")) == 0,
	  as.numeric(as.xinteger("-12345", 8L, "signed")) == -12345,
	  mean(as.xinteger(c("1", "2", "3"), 8L, "unsigned")) == 2)

### all.equal() describes a difference, and never stops

## Fixed-width types that cannot compare still need a descriptive result.
a <- as.xinteger(1:3, 8L, "signed")
stopifnot(isTRUE(all.equal(a, a)),
	  is.character(all.equal(a, as.xinteger(1:3, 8L, "unsigned"))),
	  is.character(all.equal(a, as.xinteger(1:3, 8L, "signed", na = FALSE))),
	  is.character(all.equal(a, 1:3)),
	  is.character(all.equal(a, structure(1:3, class = "int64"))),
	  is.character(all.equal(a, as.xinteger(c(1L, 2L, 4L), 8L, "signed"))),
	  is.character(all.equal(list(k = a), list(k = as.uint64(1:3)))))

### match() narrows an integer operand, as == and c() do

x <- as.xinteger(1:3, 8L, "signed")
stopifnot(identical(x %in% 1L, c(TRUE, FALSE, FALSE)), identical(1L %in% x, TRUE),
	  identical(match(x, 1L), c(1L, NA, NA)),
	  length(union(x, 1L)) == 3L, length(setdiff(x, 1L)) == 2L,
	  ## a value the width cannot hold matches nothing and is matched
	  ## by nothing; it is dropped rather than given a stand-in, every
	  ## bit pattern of the width being a value in its own right
	  identical(u8 %in% c(1L, -1L), c(TRUE, FALSE, FALSE)),
	  identical(match(u8, c(-1L, 2L, 1L)), c(3L, 2L, NA)),
	  identical(match(u8, c(-1L, 2L, 1L), nomatch = 3L), c(3L, 2L, 3L)),
	  ## two 'xinteger' vectors are still refused on a clash
	  inherits(tryCatch(as.int64(1:2) %in% as.uint64(1:2),
			    error = identity), "error"))

### the constructors take scalars

for (e in list(quote(xinteger()), quote(xinteger(2)), quote(as.xinteger(2)),
	       quote(xinteger(2, width = c(4L, 8L))), quote(xinteger(c(2, 3))),
	       quote(xinteger(2, 4L, "signed", na = c(FALSE, TRUE))),
	       quote(as.xinteger("1", c(4L, 8L), "signed"))))
    stopifnot(inherits(tryCatch(eval(e), error = identity), "error"))

### mode(x) <- mode(x) and class(x) <- class(x) are identities

x <- as.xinteger(1:3, 8L, "unsigned")
y <- x; mode(y) <- mode(y)
z <- x; mode(z) <- "numeric"
stopifnot(identical(x, y), identical(x, z))
y <- x; class(y) <- class(y)
stopifnot(identical(x, y), !is.object(y), is.null(attributes(y)))
## An additional class is explicit and the caller means to keep it.
y <- x; class(y) <- c(class(y), "xinteger")
stopifnot(identical(class(y), c("uint64", "xinteger")), is.object(y))
y <- matrix(x[1:4 %% 3 + 1], 2, 2); z <- y; class(z) <- class(z)
stopifnot(identical(y, z))
z <- y; class(z) <- typeof(z)
stopifnot(identical(y, z), is.null(attr(z, "class")))
## and mode<- still converts away from the type by the ordinary route
y <- x; mode(y) <- "character"
stopifnot(identical(y, c("1", "2", "3")))

### rbind() with an argument that contributes no rows

z <- matrix(as.xinteger(character(0), 8L, "signed"), nrow = 0, ncol = 2)
stopifnot(identical(dim(rbind(z, as.xinteger(1:2, 8L, "signed"))), c(1L, 2L)))

### psort() places NA as it does for every other type

stopifnot(identical(as.character(.Internal(psort(
	      as.xinteger(c("3", NA, "1"), 8L, "signed"), 2L))), c("1", "3", NA)))

### format() honours 'width' whatever 'trim' says

stopifnot(identical(format(x, trim = TRUE, width = 6), c("     1", "     2", "     3")),
	  identical(format(as.xinteger(c("1", "22"), 8L, "unsigned"), trim = TRUE),
		    c("1", "22")))

### a list absorbs 'xinteger' vectors that cannot be combined with each other

b4 <- as.int64(1:2); b8 <- as.uint64(1:2)
stopifnot(length(c(list(1), b4, b8)) == 5L,
	  inherits(tryCatch(c(b4, b8), error = identity), "error"))

### growing a vector that reserves no missing value

## Only the positions the assignment does not reach need a value to be
## left in them, and only those have none to leave: an append or a
## subscript that covers every new position produces no missing value
## and must go through.
local({
    v <- as.xinteger("7", 8L, "unsigned", na = FALSE)
    six <- as.xinteger(1:6, 8L, "unsigned", na = FALSE)
    mk <- function() as.xinteger(1:3, 8L, "unsigned", na = FALSE)

    x <- mk(); x[4L] <- v
    stopifnot(identical(as.character(x), c("1", "2", "3", "7")))
    x <- mk(); x[[4L]] <- v
    stopifnot(identical(as.character(x), c("1", "2", "3", "7")))
    x <- mk(); x[length(x) + 1L] <- v
    stopifnot(length(x) == 4L)
    x <- mk(); x[1:6] <- six
    stopifnot(identical(as.character(x), as.character(six)))
    x <- mk(); x[4:6] <- six[1:3]
    stopifnot(identical(as.character(x), c("1", "2", "3", "1", "2", "3")))

    ## a position left over is still refused, and so is an explicit NA
    for (bad in list(quote(x[6L] <- v), quote(x[[6L]] <- v),
		     quote(x[c(4L, 6L)] <- six[1:2]), quote(x[2L] <- NA))) {
	x <- mk()
	if (!inherits(tryCatch(eval(bad), error = identity), "error"))
	    stop("no error from ", deparse(bad))
    }

    ## repeated appends run through the growable path, which exposes
    ## slack an earlier enlargement filled
    x <- as.xinteger(1L, 8L, "unsigned", na = FALSE)
    for (i in 2:50) x[i] <- v
    stopifnot(length(x) == 50L, all(as.character(x)[-1L] == "7"))
    x <- as.xinteger(1L, 8L, "unsigned", na = FALSE)
    x[2L] <- v
    stopifnot(inherits(tryCatch(x[10L] <- v, error = identity), "error"))

    ## a vector that does reserve one is unchanged
    y <- as.xinteger(1:3, 8L, "unsigned"); y[6L] <- as.xinteger("7", 8L, "unsigned")
    stopifnot(is.na(y[5L]), !is.na(y[6L]))
})

### a destination the assignment overwrites entirely is not converted

## Narrowing the destination is for the values that survive.  When none
## does, there is nothing to convert, so out-of-range values about to be
## thrown away must not prevent the assignment.
local({
    o <- as.uint64(1:3, na = FALSE)

    x <- -1:1; x[1:3] <- o
    stopifnot(identical(as.character(x), as.character(o)))
    x <- c(-1L, 0L, 1L); x[1:3] <- o
    stopifnot(identical(typeof(x), "alt"),
	      identical(storage.mode(x), "uint64"))
    x <- -1:1; x[3:1] <- o
    stopifnot(identical(as.character(x), rev(as.character(o))))

    ## the attributes still come across
    m <- matrix(1:4, 2, 2); m[1:4] <- as.xinteger(1:4, 8L, "unsigned")
    stopifnot(identical(dim(m), c(2L, 2L)))

    ## a value that survives still has to fit the new type
    stopifnot(inherits(tryCatch({x <- -1:1; x[2:3] <- o[1:2]},
			       error = identity), "error"))
    x <- 1:3; x[1] <- as.xinteger("9", 8L, "unsigned")
    stopifnot(identical(as.character(x), c("9", "2", "3")))
})

### prod() follows the numeric coercion rule

## Its result is a double for every type, as it is for integer, so the
## operands convert and take the ordinary path.  A double promotes the
## fixed-width operand just as it does in the other numeric summaries.
local({
    p <- as.xinteger(c("2", "3"), 8L, "unsigned")
    stopifnot(identical(prod(p), 6),
	      identical(prod(p, 2L), 12),		# integer narrows in
	      identical(prod(p, 2.5), 15),		# double promotes
	      identical(storage.mode(sum(p)), "uint64"), # sum keeps the type
	      identical(prod(1:3), 6))		# every other type unchanged
    for (bad in list(quote(prod(p, "a")),
		     quote(prod(p, as.int64("2")))))
	if (!inherits(tryCatch(eval(bad), error = identity), "error"))
	    stop("no error from ", deparse(bad))
})

### partial sorting, and a size that is not a size

## psort's three-way swap used to hand memcpy() the same address twice
## when its two scans met on one element, which is the common case.
local({
    set.seed(1)
    v <- as.xinteger(sample(100), 8L, "signed")
    for (k in c(1L, 2L, 50L, 99L, 100L))
	stopifnot(identical(sort(v, partial = k)[k], sort(v)[k]))
    stopifnot(identical(as.character(sort(v)), as.character(sort(as.integer(v)))))
})

## a negative element is an invalid length, not a cast out of range
local({
    neg <- as.xinteger("-1", 8L, "signed")
    stopifnot(inherits(tryCatch(vector("numeric", neg), error = identity),
		       "error"),
	      length(vector("numeric", as.xinteger("3", 8L, "unsigned"))) == 3L)
})

## A double converts where it is exactly the integer it appears to be,
## at any width -- including magnitudes no C integer type would hold.
## It has to give the same answer as the text form, because the text
## form is what it is checked against everywhere else.
local({
    stopifnot(identical(as.xinteger(42, 8L, "signed"),
		        as.xinteger("42", 8L, "signed")),
	      ## exact well past 2^53, and past what a long long holds
	      identical(as.xinteger(2^62, 8L, "signed"),
		        as.xinteger("4611686018427387904", 8L, "signed")),
	      identical(as.xinteger(2^63, 8L, "unsigned"),
		        as.xinteger("9223372036854775808", 8L, "unsigned")),
	      identical(as.xinteger(-2^62, 8L, "signed"),
		        as.xinteger("-4611686018427387904", 8L, "signed")))

    ## the same conversion behind the other two spellings
    stopifnot(identical(as.vector(42, "int64"), as.xinteger(42, 8L, "signed")),
	      identical({ z <- 42; storage.mode(z) <- "int64"; z },
		        as.xinteger(42, 8L, "signed")))

    ## a value that is not an integer is not one at any width, and is a
    ## different mistake from one that is simply too large
    w <- NULL
    val <- withCallingHandlers(as.xinteger(c(1.5, Inf, -Inf), 8L, "signed"),
			       warning = function(e) {
				   w <<- c(w, conditionMessage(e))
				   invokeRestart("muffleWarning") })
    stopifnot(all(is.na(val)), length(w) == 1L,
	      grepl("coercion", w))

    w <- NULL
    val <- withCallingHandlers(as.xinteger(c(1e30, -1), 8L, "unsigned"),
			       warning = function(e) {
				   w <<- c(w, conditionMessage(e))
				   invokeRestart("muffleWarning") })
    stopifnot(all(is.na(val)), length(w) == 1L,
	      grepl("outside the range", w))

    ## NA and NaN are missing values, not failed conversions
    stopifnot(identical(is.na(as.xinteger(c(NA_real_, NaN, 1), 8L, "signed")),
		        c(TRUE, TRUE, FALSE)))

    ## with nothing reserved there is no NA to produce
    stopifnot(inherits(tryCatch(as.xinteger(NA_real_, 8L, "signed", na = FALSE),
			        error = identity), "error"),
	      inherits(tryCatch(as.xinteger(1e30, 8L, "unsigned", na = FALSE),
			        error = identity), "error"))
})

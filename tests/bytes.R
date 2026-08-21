## Tests for 'bytes' vectors (BYTESXP): fixed-width opaque data vectors.
##
## Tests should be written to raise an error on test failure.

u   <- as.bytes(c("1", "2", "3"), 8L, "unsigned")
s   <- as.bytes(c("-1", "0", "1"), 8L, "signed")
op  <- as.bytes(c("0102", "0304"), 2L, "opaque")
b1  <- as.bytes("42", 8L, "unsigned")
un  <- as.bytes(c("1", NA), 8L, "unsigned")
nn  <- as.bytes(c("1", "2"), 8L, "unsigned", na = FALSE)

### type identity

stopifnot(is.bytes(u), !is.bytes(1:3), !is.raw(u),
	  typeof(u) == "uint64", typeof(s) == "int64", typeof(op) == "bytes2",
	  identical(class(u), c("uint64", "bytes")),
	  is.atomic(u), is.vector(u), !is.numeric(u),
	  bytesWidth(u) == 8L, bytesKind(u) == "unsigned", bytesHasNA(u),
	  !bytesHasNA(nn),
	  length(u) == 3L,			# elements, not bytes
	  length(bytesRaw(u)) == 24L)

## the accessors are about 'bytes' vectors, and say so in R's terms
## rather than naming the C entry point
for (f in list(bytesWidth, bytesKind, bytesHasNA, bytesRaw))
    stopifnot(grepl("'x' must be a 'bytes' vector",
		    tryCatch(f(1L), error = conditionMessage)))

### subsetting and subassignment

stopifnot(identical(as.character(u[2:3]), c("2", "3")),
	  identical(as.character(rev(u)), c("3", "2", "1")),
	  as.character(u[[1L]]) == "1")

x <- u
x[2L] <- as.bytes("9", 8L, "unsigned")
stopifnot(identical(as.character(x), c("1", "9", "3")),
	  identical(as.character(u), c("1", "2", "3")))	# copy on write

## a >= 3-dimensional array must be readable, not only writable
a <- as.bytes(as.character(1:8), 8L, "unsigned")
dim(a) <- c(2L, 2L, 2L)
stopifnot(as.character(a[1, 1, 1]) == "1",
	  identical(as.character(a[1, , ]), c("1", "3", "5", "7")))
a[1, 1, 1] <- as.bytes("99", 8L, "unsigned")
stopifnot(as.character(a[1, 1, 1]) == "99")

### matrices, and the asplit() path behind unique.matrix()

m  <- u[c(1, 1, 2, 2)];	dim(m)  <- c(2L, 2L)
mi <- c(1L, 1L, 2L, 2L); dim(mi) <- c(2L, 2L)
stopifnot(nrow(unique(m)) == nrow(unique(mi)),
	  identical(duplicated(m), duplicated(mi)),
	  anyDuplicated(m) == anyDuplicated(mi),
	  length(asplit(m, 1L)) == 2L, is.bytes(asplit(m, 1L)[[1L]]))
d <- u[c(1, 2, 1, 2)]; dim(d) <- c(2L, 2L)
stopifnot(nrow(unique(d)) == 2L)

### combining

stopifnot(identical(as.character(c(u, u)), rep(c("1", "2", "3"), 2L)),
	  ## names get the seqno form every other atomic type uses ...
	  identical(names(c(a = u)), names(c(a = as.raw(1:3)))),
	  ## ... and a zero-length argument must not miscount the slots
	  identical(names(c(a = bytes(0L, 8L, "unsigned"), b = 1L)), "b"),
	  ## NULL contributes nothing, as it does for every other type
	  ncol(cbind(bytes(0L, 8L, "unsigned"), NULL)) ==
	  ncol(cbind(complex(0), NULL)))

## width and kind are part of the type: combining across them is an error,
## not a silent promotion
stopifnot(inherits(tryCatch(c(u, as.bytes("1", 4L, "unsigned")),
			    error = identity), "error"),
	  inherits(tryCatch(c(u, s), error = identity), "error"),
	  inherits(tryCatch(c(u, nn), error = identity), "error"))

### the empty vector

for (e in list(bytes(0L, 8L, "unsigned"), bytes(0L, 4L, "opaque"),
	       bytes(0L, 8L, "signed", na = FALSE))) {
    stopifnot(is.bytes(e), length(e) == 0L,
	      identical(unique(e), e), length(duplicated(e)) == 0L,
	      ## print() names the type it printed, as dput() does; "bytes(0)"
	      ## would be a valid call producing a different object
	      identical(capture.output(print(e)), capture.output(dput(e))))
}

### coercion

stopifnot(identical(as.integer(u), 1:3),
	  identical(as.double(u), c(1, 2, 3)),
	  identical(as.raw(u), as.raw(1:3)),
	  identical(as.character(u), c("1", "2", "3")),
	  identical(as.complex(u), complex(real = 1:3)),
	  identical(as.logical(as.bytes(c("0", "1"), 8L, "unsigned")),
		    c(FALSE, TRUE)),
	  is.na(as.logical(as.bytes(NA_character_, 8L, "unsigned"))),
	  ## as.logical is "any bit set", which needs no reading of the
	  ## bytes as a number and so works for the opaque kind too
	  identical(as.logical(op), c(TRUE, TRUE)),
	  length(as.expression(u)) == 3L, is.bytes(as.expression(u)[[1L]]),
	  length(as.list(u)) == 3L, is.bytes(as.list(u)[[1L]]),
	  length(as.pairlist(u)) == 3L, is.bytes(as.pairlist(u)[[1L]]))

## the opaque kind has no numeric reading, and says so rather than
## inventing one
for (e in list(quote(as.integer(op)), quote(as.double(op)),
	       quote(as.complex(op)), quote(seq_len(as.bytes("02", 1L, "opaque")))))
    stopifnot(inherits(tryCatch(eval(e), error = identity), "error"))

x <- u
storage.mode(x) <- "logical"
stopifnot(identical(x, c(TRUE, TRUE, TRUE)))

## as.bytes() and as.vector() drop attributes on every path, including the
## one where nothing needs converting; storage.mode<- keeps them
nx <- as.bytes(c(a = 1L, b = 2L), 8L, "unsigned")
names(nx) <- c("a", "b")
stopifnot(is.null(names(as.bytes(nx, 8L, "unsigned"))),	# no change needed
	  is.null(names(as.bytes(nx, 16L, "unsigned"))),	# widened
	  is.null(names(as.vector(nx, "uint64"))),
	  { y <- nx; storage.mode(y) <- "uint64"; identical(names(y), c("a","b")) })

### scalar coercion: control flow must not hit an internal error

stopifnot(if (b1) TRUE else FALSE,
	  identical(seq_len(as.bytes("4", 8L, "unsigned")), 1:4),
	  identical(rep(1L, as.bytes("2", 8L, "unsigned")), c(1L, 1L)))

### the missing-value predicates

stopifnot(identical(is.na(un), c(FALSE, TRUE)),
	  identical(is.finite(un), c(TRUE, FALSE)),
	  identical(is.nan(un), c(FALSE, FALSE)),
	  identical(is.infinite(un), c(FALSE, FALSE)),
	  all(is.finite(nn)),
	  identical(lengths(u), c(1L, 1L, 1L)))

### ordering

set.seed(1)
for (i in 1:50) {
    n <- sample(2:12, 1L)
    v <- as.bytes(as.character(sample(1000L, n)), 8L, "unsigned")
    k <- sample(seq_len(n), 1L)
    ## the partial sort must agree with the full one at the pivot
    stopifnot(identical(as.character(sort(v, partial = k)[k]),
			as.character(sort(v)[k])),
	      !is.unsorted(as.numeric(as.character(sort(v)))))
}
o <- as.bytes(c("05", "01", "04", "02", "03"), 1L, "opaque")
stopifnot(as.character(sort(o, partial = 2L)[2L]) == "02",
	  identical(as.character(sort(o)),
		    c("01", "02", "03", "04", "05")))	# lexicographic

## values beyond what a double can name still order correctly.  na = FALSE
## because the all-ones pattern is the reserved NA when NA is representable,
## so only there is the full range of the width available.
big <- as.bytes(c("18446744073709551615", "18446744073709551614"),
		8L, "unsigned", na = FALSE)
stopifnot(identical(as.character(sort(big)),
		    c("18446744073709551614", "18446744073709551615")),
	  big[1L] > big[2L],
	  ## and with NA representable the top of the range is reserved
	  is.na(suppressWarnings(
	      as.bytes("18446744073709551615", 8L, "unsigned"))))

### arithmetic keeps full precision

p <- as.bytes("9007199254740993", 8L, "unsigned")	# 2^53 + 1
stopifnot(as.character(p + 1L) == "9007199254740994",
	  as.character(p * 2L) == "18014398509481986",
	  ## the same value through a double would have lost the low bit
	  as.character(as.bytes(as.character(as.numeric(as.character(p))),
				8L, "unsigned")) != as.character(p))

stopifnot(as.character(sum(u)) == "6", as.character(prod(u)) == "6",
	  as.character(min(u)) == "1", as.character(max(u)) == "3",
	  as.character(min(s)) == "-1")

## min/max of nothing: every other type warns and returns +/-Inf.  There is
## no Inf here, so NA stands in where NA exists, and only where it does not
## is there nothing to return.
stopifnot(is.na(suppressWarnings(min(bytes(0L, 8L, "unsigned")))),
	  is.na(suppressWarnings(max(bytes(0L, 8L, "unsigned")))),
	  grepl("returning NA",
		tryCatch(min(bytes(0L, 8L, "unsigned")),
			 warning = conditionMessage)),
	  grepl("cannot represent NA",
		tryCatch(min(bytes(0L, 8L, "unsigned", na = FALSE)),
			 error = conditionMessage)))

### bitwise operations

h <- as.bytes("255", 8L, "unsigned")
stopifnot(as.character(bitwAnd(h, as.bytes("15", 8L, "unsigned"))) == "15",
	  as.character(bitwOr(as.bytes("240", 8L, "unsigned"),
			      as.bytes("15", 8L, "unsigned"))) == "255",
	  as.character(bitwShiftL(as.bytes("1", 8L, "unsigned"), 63L)) ==
	  "9223372036854775808",
	  ## as bitwShiftL(1L, 32L) is NA
	  is.na(bitwShiftL(as.bytes("1", 8L, "unsigned"), 64L)),
	  ## but a type with no NA has nothing to return, and says which
	  ## argument was at fault
	  grepl("shift out of range",
		tryCatch(bitwShiftL(nn, 64L), error = conditionMessage)))

### format() and printing

big2 <- as.bytes("1234567890123", 8L, "unsigned")
stopifnot(format(big2) == "1234567890123",
	  format(big2, big.mark = ",") == "1,234,567,890,123",
	  nchar(format(big2, width = 20L)) == 20L,
	  ## hex is not decimal digits, so it is left alone
	  identical(format(op, big.mark = ","), format(op)))

## cat() must not truncate a wide element
wide <- as.bytes(strrep("9", 600), 255L, "unsigned")
stopifnot(identical(capture.output(cat(wide)), as.character(wide)))

### round trips

## serialization version 4: no older R can read this type, and the
## header of a version 2 or 3 stream promises one that can.  A version
## the caller did not name is raised to 4, with a message; one they did
## name is an error if it cannot hold the vector.
for (v in list(u, s, op, un, nn, bytes(0L, 8L, "unsigned"))) {
    stopifnot(identical(suppressMessages(unserialize(serialize(v, NULL))), v),
	      identical(unserialize(serialize(v, NULL, version = 4)), v),
	      identical(eval(parse(text = paste(deparse(v), collapse = ""))), v),
	      identical(v[seq_along(v)], v),
	      inherits(tryCatch(serialize(v, NULL, version = 3),
				error = identity), "error"),
	      inherits(tryCatch(serialize(v, NULL), message = identity),
		       "message"))
    f <- tempfile()
    suppressMessages(saveRDS(v, f))
    stopifnot(identical(readRDS(f), v),
	      infoRDS(f)$version == 4L)
    unlink(f)
}

## and only when the object needs it: everything else keeps writing the
## version it wrote before
local({
    f <- tempfile()
    saveRDS(list(1:3, "a"), f)
    on.exit(unlink(f))
    stopifnot(infoRDS(f)$version == 3L)
})

## the vector is found wherever serialization would reach it
local({
    reach <- list(nested = list(list(u)),
		  attribute = structure(1:3, key = u),
		  frame = data.frame(k = u, n = 1:3),
		  closure = local({ hidden <- u; function() hidden }),
		  promise = (function(a = u) function() a)())
    for (nm in names(reach)) {
	got <- tryCatch(serialize(reach[[nm]], NULL), message = identity)
	if (!inherits(got, "message"))
	    stop("the version was not raised for a 'bytes' vector in a ", nm)
    }
})

### readBin()/writeBin()

f <- tempfile()
writeBin(as.bytes("4294967297", 8L, "signed"), f)
stopifnot(as.character(readBin(f, "int64", 1L)) == "4294967297",
	  ## a name of that shape but an unsupported width is an error
	  inherits(tryCatch(readBin(f, "int65", 1L), error = identity), "error"))
unlink(f)

## a length-one character vector that is not a mode name is a prototype,
## which is the documented "an object whose mode will give the mode" form
f <- tempfile()
writeBin(c("ab", "cd"), f)
stopifnot(identical(readBin(f, character(1), 2L), c("ab", "cd")),
	  identical(readBin(f, character(), 2L), c("ab", "cd")),
	  identical(readBin(f, "", 2L), c("ab", "cd")))
unlink(f)

### vector(), storage.mode<- and mode<-

stopifnot(identical(vector("uint64", 2L), bytes(2L, 8L, "unsigned")),
	  identical(as.vector(u, "int64"), as.bytes(c("1","2","3"), 8L, "signed")))

x <- 1:3
storage.mode(x) <- "uint64"
stopifnot(is.bytes(x), typeof(x) == "uint64")

x <- 1:3
mode(x) <- "uint64"			# no as.uint64(): goes to storage.mode<-
stopifnot(is.bytes(x), typeof(x) == "uint64")

x <- u
mode(x) <- storage.mode(x)		# must be a no-op, not a conversion
stopifnot(identical(x, u))

## "bytes" names no type -- as.bytes()'s defaults must not pick one
for (e in list(quote({x <- c("01","02"); mode(x) <- "bytes"}),
	       quote({x <- c("01","02"); storage.mode(x) <- "bytes"})))
    stopifnot(inherits(tryCatch(eval(e), error = identity), "error"))

### memory.profile() must have a slot for the type

stopifnot("bytes" %in% names(memory.profile()))

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
ny <- as.raw(1:4); names(ny) <- letters[1:4]
storage.mode(ny) <- "bytes1"
stopifnot(identical(names(ny), letters[1:4]))

### a 'bytes' right-hand side keeps the destination's attributes

## every other arm of SubassignTypeFix() coerces through coerceVector(),
## which carries them over; this one narrows into a fresh vector
b <- as.bytes(9L, 4L, "signed")
m <- matrix(1:4, 2, 2); m[1, 1] <- b
stopifnot(identical(dim(m), c(2L, 2L)))
v <- c(a = 1L, b = 2L); v[1] <- b
stopifnot(identical(names(v), c("a", "b")))
o <- structure(1:3, class = "myclass"); o[1] <- b
stopifnot(is.object(o), identical(class(o), "myclass"))

## a list right-hand side promotes the destination, as for every other
## atomic type, and a NULL one takes the width and kind from the value
z <- as.bytes(1:3, 8L, "unsigned"); z[1] <- list(1)
stopifnot(is.list(z), length(z) == 3L)
z <- NULL; z[1] <- as.bytes("7", 8L, "unsigned")
stopifnot(is.bytes(z), length(z) == 1L, as.character(z) == "7")

### raising the serialization version must not leave a truncated file

## the raise is announced with a message, which an exiting handler can
## unwind out of; settling it before anything is written is what keeps
## that from happening with a header already on the file
f <- tempfile()
r <- tryCatch(save(list = "b", file = f, compress = FALSE, envir = environment()),
	      message = function(m) "handled")
stopifnot(identical(r, "handled"), file.size(f) == 0L)
save(list = "b", file = f, envir = environment())
e <- new.env(); load(f, envir = e)
stopifnot(identical(e$b, b))
unlink(f)

## save.to.file() is reached from compiler::cmpfile() and from the
## embedding API, and was the one entry point that never raised it
f <- tempfile()
invisible(.Internal(save.to.file(list(b), f, FALSE, NULL)))
stopifnot(identical(.Internal(load.from.file(f))[[1L]], b))
unlink(f)

### comparison against a bound the type cannot hold

## it is not missing: it lies below or above every element, so the
## answer is determined and the filter idioms must not go quiet
u8 <- as.bytes(c("1", "2", "3"), 1L, "unsigned")
stopifnot(identical(u8 > -1L, rep(TRUE, 3)), identical(u8 < 1000L, rep(TRUE, 3)),
	  identical(-1L < u8, rep(TRUE, 3)),
	  identical(u8[u8 > -1L], u8), identical(which(u8 > -1L), 1:3),
	  ## the reserved NA value counts as out of range, in its direction
	  identical(as.bytes(c("1", "254"), 1L, "unsigned") < 255L, c(TRUE, TRUE)),
	  ## and a vector that reserves nothing no longer errors
	  identical(as.bytes(c("1", "2"), 1L, "unsigned", na = FALSE) < 1000L,
		    c(TRUE, TRUE)),
	  ## an element that really is missing still answers NA
	  identical(as.bytes(c("1", NA), 1L, "unsigned") > -1L, c(TRUE, NA)))

## min and max only compare, so a bound they cannot hold is ignored
## unless it wins outright, when the answer itself is out of range
stopifnot(as.character(min(u8, 1000L)) == "1", as.character(max(u8, -1L)) == "3",
	  identical(as.character(pmin(u8, 1000L)), c("1", "2", "3")),
	  is.na(suppressWarnings(max(u8, 1000L))),
	  all(is.na(suppressWarnings(pmax(u8, 1000L)))))

## a width is part of the type, so min() and max() refuse the pairs c()
## refuses -- range() goes through c() and must not fail where they work
a8 <- as.bytes("10", 8L, "unsigned"); s4 <- as.bytes("5", 4L, "unsigned")
for (e in list(quote(max(a8, s4)), quote(min(a8, s4)), quote(sum(a8, s4)),
	       quote(pmin(a8, s4)), quote(c(a8, s4))))
    stopifnot(inherits(tryCatch(eval(e), error = identity), "error"))
stopifnot(identical(as.character(range(a8, as.bytes("3", 8L, "unsigned"))),
		    c("3", "10")))

## max() saw two non-missing arguments here, whatever order they came in
stopifnot(is.na(max(as.bytes(c(NA, "1", "2"), 8L, "unsigned"))),
	  as.character(max(as.bytes(c(NA, "1", "2"), 8L, "unsigned"),
			   na.rm = TRUE)) == "2")
op <- options(warn = 2)
stopifnot(is.na(max(as.bytes(c(NA, "1"), 8L, "unsigned"))))
options(op)

### as.numeric() must give the nearest double

## accumulating byte by byte rounds at every step once the running total
## passes 2^53, and the errors compound to as much as a whole ulp
stopifnot(suppressWarnings(
	      as.numeric(as.bytes("3119042104763040036", 8L, "unsigned"))) ==
	  3119042104763040256,
	  as.numeric(as.bytes("12345", 8L, "unsigned")) == 12345,
	  as.numeric(as.bytes("0", 8L, "unsigned")) == 0,
	  as.numeric(as.bytes("-12345", 8L, "signed")) == -12345,
	  mean(as.bytes(c("1", "2", "3"), 8L, "unsigned")) == 2)

### all.equal() describes a difference, and never stops

## mode() is "bytes" for every width and kind, so data.class() cannot
## see the difference the comparison itself refuses to make
a <- as.bytes(1:3, 8L, "signed")
stopifnot(isTRUE(all.equal(a, a)),
	  is.character(all.equal(a, as.bytes(1:3, 4L, "signed"))),
	  is.character(all.equal(a, as.bytes(1:3, 8L, "unsigned"))),
	  is.character(all.equal(a, as.bytes(1:3, 8L, "signed", na = FALSE))),
	  is.character(all.equal(a, 1:3)),
	  is.character(all.equal(a, as.bytes(c(1L, 2L, 4L), 8L, "signed"))),
	  is.character(all.equal(list(k = a), list(k = as.bytes(1:3, 4L, "signed")))))

### match() narrows an integer operand, as == and c() do

x <- as.bytes(1:3, 8L, "signed")
stopifnot(identical(x %in% 1L, c(TRUE, FALSE, FALSE)), identical(1L %in% x, TRUE),
	  identical(match(x, 1L), c(1L, NA, NA)),
	  length(union(x, 1L)) == 3L, length(setdiff(x, 1L)) == 2L,
	  ## a value the width cannot hold matches nothing and is matched
	  ## by nothing; it is dropped rather than given a stand-in, every
	  ## bit pattern of the width being a value in its own right
	  identical(u8 %in% c(1L, 1000L), c(TRUE, FALSE, FALSE)),
	  identical(match(u8, c(1000L, 2L, 1L)), c(3L, 2L, NA)),
	  identical(match(u8, c(1000L, 2L, 1L), nomatch = 3L), c(3L, 2L, 3L)),
	  ## two 'bytes' vectors are still refused on a clash
	  inherits(tryCatch(as.bytes(1:2, 4L, "unsigned") %in%
			    as.bytes(1:2, 8L, "unsigned"),
			    error = identity), "error"))

### the constructors take scalars

for (e in list(quote(bytes(2, width = c(4L, 8L))), quote(bytes(c(2, 3))),
	       quote(bytes(2, 4L, "signed", na = c(FALSE, TRUE))),
	       quote(as.bytes("1", c(4L, 8L), "signed"))))
    stopifnot(inherits(tryCatch(eval(e), error = identity), "error"))

### mode(x) <- mode(x) and class(x) <- class(x) are identities

x <- as.bytes(1:3, 8L, "unsigned")
y <- x; mode(y) <- mode(y)
stopifnot(identical(x, y))
y <- x; class(y) <- class(y)
stopifnot(identical(x, y), !is.object(y), is.null(attributes(y)))
y <- matrix(x[1:4 %% 3 + 1], 2, 2); z <- y; class(z) <- class(z)
stopifnot(identical(y, z))
## and mode<- still converts away from the type by the ordinary route
y <- x; mode(y) <- "character"
stopifnot(identical(y, c("1", "2", "3")))

### rbind() with an argument that contributes no rows

z <- matrix(as.bytes(character(0), 8L, "signed"), nrow = 0, ncol = 2)
stopifnot(identical(dim(rbind(z, as.bytes(1:2, 8L, "signed"))), c(1L, 2L)))

### the opaque kind sorts by the same radix as the numeric ones

set.seed(1)
oo <- as.bytes(as.raw(sample(0:255, 600, replace = TRUE)), 8L, "opaque")
stopifnot(identical(as.character(sort(oo)), sort(as.character(oo))),
	  identical(order(oo), order(as.character(oo))),
	  identical(order(oo, decreasing = TRUE),
		    order(as.character(oo), decreasing = TRUE)))

### psort() places NA as it does for every other type

stopifnot(identical(as.character(.Internal(psort(
	      as.bytes(c("3", NA, "1"), 8L, "signed"), 2L))), c("1", "3", NA)))

### format() honours 'width' whatever 'trim' says

stopifnot(identical(format(x, trim = TRUE, width = 6), c("     1", "     2", "     3")),
	  identical(format(as.bytes(c("1", "22"), 8L, "unsigned"), trim = TRUE),
		    c("1", "22")))

### a list absorbs 'bytes' vectors that cannot be combined with each other

b4 <- as.bytes(1:2, 4L, "unsigned"); b8 <- as.bytes(1:2, 8L, "unsigned")
stopifnot(length(c(list(1), b4, b8)) == 5L,
	  inherits(tryCatch(c(b4, b8), error = identity), "error"))

### NA is the one right-hand side an opaque vector takes from a number

opq <- as.bytes(as.raw(1:16), 16L, "opaque")
stopifnot(all(is.na(bitwAnd(opq, NA))),
	  inherits(tryCatch(bitwAnd(opq, 1L), error = identity), "error"))

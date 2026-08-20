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

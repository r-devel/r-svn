% File src/library/base/man/bytes.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2026 R Core Team
% Distributed under GPL 2 or later

\name{bytes}
\alias{bytes}
\alias{as.bytes}
\alias{bytesHasNA}
\alias{bytesRaw}
\alias{bytesKind}
\alias{bytesWidth}
\alias{is.bytes}
\alias{is.fixedwidth}
\alias{as.int8}
\alias{as.uint8}
\alias{as.int16}
\alias{as.uint16}
\alias{as.int32}
\alias{as.uint32}
\alias{as.int64}
\alias{as.uint64}
\alias{as.int128}
\alias{as.uint128}
\title{Vectors of Fixed-Width Data}
\description{
  Vectors whose elements are a fixed number of bytes wide, for values
  the other atomic types cannot hold: 64-bit and wider integers,
  hashes, UUIDs, network addresses.
}
\usage{
bytes(length = 0L, width = 1L,
      kind = c("opaque", "unsigned", "signed"), na = TRUE)

as.bytes(x, width = 1L,
         kind = c("opaque", "unsigned", "signed"), na = TRUE)

as.int8(x, na = TRUE)
as.uint8(x, na = TRUE)
as.int16(x, na = TRUE)
as.uint16(x, na = TRUE)
as.int32(x, na = TRUE)
as.uint32(x, na = TRUE)
as.int64(x, na = TRUE)
as.uint64(x, na = TRUE)
as.int128(x, na = TRUE)
as.uint128(x, na = TRUE)

is.bytes(x)
is.fixedwidth(x)
bytesWidth(x)
bytesKind(x)
bytesHasNA(x)
bytesRaw(x)
}
\arguments{
  \item{length}{a non-negative integer giving the number of
    \emph{elements} (not bytes).}
  \item{width}{the size of one element in bytes, between 1 and 255.}
  \item{kind}{how the bytes of an element are to be read; see
    \sQuote{Details}.}
  \item{na}{logical: should one value of the width be reserved as the
    \code{NA} sentinel?}
  \item{x}{for \code{as.bytes}, a raw, character, integer or logical
    vector, or another \code{bytes} vector; elsewhere a \code{bytes}
    vector.}
}
\details{
  Unlike every other vector type, the element size of a \code{bytes}
  vector is a property of the vector rather than of the type.
  \code{\link{length}} counts elements, so a length-100 vector of width
  8 holds 800 bytes.  \code{\link{typeof}} reports a name built from the
  kind and the width, so that code can dispatch on what a vector is
  actually holding:

  \tabular{lll}{
    \code{kind} \tab \code{typeof} \tab holds \cr
    \code{"opaque"} \tab \code{"bytes16"} \tab a byte string of that
      width, never read as a number \cr
    \code{"unsigned"} \tab \code{"uint64"} \tab an unsigned integer of
      \code{8 * width} bits \cr
    \code{"signed"} \tab \code{"int64"} \tab a two's complement integer
      of \code{8 * width} bits
  }

  Three kinds rather than two because the width alone cannot say what
  the bytes mean: a width-16 UUID and a width-16 unsigned integer are
  the same size and must sort differently.  Opaque elements are ordered
  as byte strings and displayed as hex; the numeric kinds are ordered by
  value and displayed as decimal.

  The numeric kinds are stored in the platform's \emph{native} byte
  order, so reading them from another data source is a plain copy and
  \code{bytesRaw} round-trips exactly.  Because they are \emph{ordered}
  by value rather than by bytes, sorting nevertheless agrees across
  platforms, and \code{\link{serialize}} normalises the byte order so
  that saved files do too.  A stream holding one of these vectors needs
  workspace format \code{version = 4}: no \R that predates the type can
  read it, and the header of a version 2 or 3 stream names an \R that
  can.  \code{\link{saveRDS}}, \code{\link{serialize}} and
  \code{\link{save}} therefore write version 4 for such an object,
  saying so with a \code{\link{message}}, and leave every other object
  on the version they wrote before.  Asking for a version that cannot
  hold the vector is an error rather than a file whose header is untrue.

  With \code{na = TRUE} (the default) one value of the width is set aside
  as a sentinel and read as \code{\link{NA}}, as \code{NA_integer_} and
  \code{NA_real_} are for their own types: all bits set for the opaque
  and unsigned kinds, the most negative value for the signed kind.  Pass
  \code{na = FALSE} when every bit pattern in the data is a legitimate
  value -- at width 1 the sentinel costs 1 in 256, and at width 4
  unsigned it costs the IPv4 broadcast address.  Such a vector has no
  sentinel to spare, so every operation that would produce a missing
  value -- an out-of-range subscript, \code{length<-} growth, a join
  miss, arithmetic overflow -- raises an error instead.  Whether a
  sentinel is reserved is part of the type, as the kind and the width
  are, so vectors that disagree about it cannot be combined or compared.
  \code{\link{typeof}} names the width and the kind but not the
  sentinel, so a type name is not quite the whole type: interfaces that
  need all three, such as \code{\link{readBin}}, \code{\link{scan}}
  and \code{\link{vector}}, accept an existing vector in place of a
  name.  That is what lets the \code{vector(typeof(x), n)} idiom behind
  \code{\link{tapply}}, \code{\link{apply}} and \code{\link{diag}}
  reproduce a vector created with \code{na = FALSE}.

  There is no literal for a missing \code{bytes} value, since the
  sentinel's bit pattern depends on the width and the kind:
  \code{as.bytes(NA, width, kind)} writes one, and \code{\link{rep}}
  makes a vector of them.

  Arithmetic (\code{+}, \code{-}, \code{*}, \code{\%/\%}, \code{\%\%} and
  unary minus) is defined for the numeric kinds at widths 1, 2, 4, 8 and
  16; wider elements are storage only.  Two \code{bytes} operands must
  agree on the width, the kind and the sentinel, exactly as they must
  for \code{\link{c}} and comparison: nothing is promoted to the wider
  of the two.  Integer and logical operands narrow in, with a value that
  does not fit becoming \code{NA} with a warning, as integer overflow
  does.  A double or complex operand instead promotes a numeric
  fixed-width vector to that ordinary numeric type.  Conversion to
  double warns only when an actual value loses precision (or is beyond
  the range of a double).  Division by zero is \code{NA} without a
  warning, as it is for integer.  The bitwise operations
  (\code{\link{bitwAnd}} and friends) apply to every kind and every
  width, and are what the opaque kind is for.

  Comparison, \code{\link{sort}}, \code{\link{order}},
  \code{\link{match}}, \code{\link{unique}}, \code{\link{table}} and
  \code{\link{factor}} work for all kinds and widths.  An integer or
  logical operand narrows in here as it does in arithmetic, but a value
  the width cannot hold is not treated as missing: for a comparison it
  lies below or above every element, so \code{x > -1L} is \code{TRUE}
  for an unsigned vector rather than \code{NA}, and for \code{match}
  it is neither present nor a match.  Two \code{bytes} vectors must
  agree on the width, the kind and the sentinel, and \code{\link{min}},
  \code{\link{max}} and \code{\link{pmin}} refuse exactly the pairs
  \code{\link{c}} refuses -- so that \code{\link{range}}, whose answer
  goes through \code{c}, cannot fail on arguments they accept.

  \code{\link{as.integer}}, \code{\link{as.numeric}} and
  \code{\link{as.raw}} convert the numeric kinds and refuse the opaque
  one, each behaving as it does for an integer vector when the value
  will not fit.  \code{\link{mean}} is a double like every other mean
  in \R, but is formed from an exact total rather than element by
  element, so it agrees with \code{sum(x)/length(x)}.

  At the R interface, the signed and unsigned kinds are numeric:
  \code{is.numeric} is true, \code{mode} is \code{"numeric"}, and their
  implicit \code{\link{class}} is the semantic type name such as
  \code{"uint64"}.  Opaque vectors remain nonnumeric, have mode
  \code{"bytes"}, and have implicit class
  \code{c("bytes16", "bytes")}.  A package wanting semantics of its own layers a
  further class on top -- \code{class(x) <- c("myint", class(x))} --
  which dispatches ahead of the methods for the type and leaves the
  vector working underneath.  S4 dispatch needs the implicit class
  registered first; a type name is derived from the vector rather than
  fixed, so \pkg{methods} cannot know them all in advance.
}
\value{
  \code{bytes} returns a zero-filled vector of \code{length} elements.

  \code{as.bytes} converts \code{x}.  A raw vector is reinterpreted
  verbatim, so its length must be a multiple of \code{width}.  A
  character vector is parsed -- decimal for the numeric kinds, hex for
  the opaque one, the inverse of \code{\link{as.character}} -- with
  \code{NA_character_} giving \code{NA}, and text that is not a number
  or does not fit giving \code{NA} with a warning.  Integer and logical
  vectors narrow, except into the opaque kind, which takes no number:
  there \code{NA} converts and everything else is an error.

  Another \code{bytes} vector is converted between widths and between
  the two numeric kinds, preserving the value: a value that does not fit
  the target becomes \code{NA} with a warning, as it does everywhere
  else.  The opaque kind takes no part in that, since its elements are
  byte strings rather than values and there is no answer to which end of
  a short one to pad; only its \code{na} setting can be changed.
  \code{\link{as.vector}(x, "int64")} and \code{\link{storage.mode}(x)
  <- "int64"} do the same thing, except that the mode name cannot say
  whether \code{NA} is reserved, so those two keep whatever \code{x}
  already had.

  \code{as.bytes} and \code{as.vector} drop attributes, as the rest of
  the \code{as.*} family does, including when \code{x} already has the
  requested width and kind; \code{storage.mode<-} keeps them.

  \code{is.bytes} is semantic and returns \code{TRUE} only for the opaque
  kind.  \code{is.fixedwidth} tests the shared \code{BYTESXP} storage
  representation and therefore also returns \code{TRUE} for numeric
  fixed-width vectors.
  \code{bytesWidth} returns the element size in bytes, \code{bytesKind}
  the kind as a string, and \code{bytesHasNA} whether the vector has an
  \code{NA} sentinel.

  \code{bytesRaw} returns the whole payload as a \code{\link{raw}}
  vector, \code{length(x) * bytesWidth(x)} bytes in storage order.
}
\seealso{
  \code{\link{readBin}} and \code{\link{scan}} to read these from a
  file, \code{\link{read.table}} for the \code{colClasses} form,
  \code{\link{bitwAnd}} for masking, \code{\link{vector}} and
  \code{\link{storage.mode}} for the type names.

  \code{\link{raw}} for byte vectors whose length is a byte count.
}
\examples{
## a 64-bit key read from text, at full precision
id <- as.bytes(c("9223372036854775807", "-9007199254740993"), 8L, "signed")
id
typeof(id)
as.numeric("9223372036854775807")   # what a double would have made of it

sort(id)
id[1] \%in\% id

## an opaque byte string: compared and masked, never read as a number
digest <- as.bytes(c("9e107d9d372bb6826bd81d3542a419d6",
                     "e4d909c290d0fb1ca068ffaddf22cbd0"), 16L)
digest
bytesWidth(digest)
bytesKind(digest)
bitwAnd(digest, as.bytes("ff000000000000000000000000000000", 16L))

## widening and narrowing preserve the value
as.vector(id, "int128")
as.vector(id, "int32")              # neither fits: NA, with a warning

## Knuth's multiplicative hash for bucketing: multiply by 2^64/phi and
## keep the low 64 bits.  That product needs 128 bits to hold it, so
## widen first and chop the top half off afterwards.
key  <- as.bytes(c("1", "2", "3", "9223372036854775807"), 8L, "unsigned")
phi  <- as.bytes("11400714819323198485", 16L, "unsigned")
prod <- as.vector(key, "uint128") * phi
h    <- as.vector(bitwShiftR(bitwShiftL(prod, 64L), 64L), "uint64")
h
bitwShiftR(h, 61L)                  # the top 3 bits, so 8 buckets

## every bit pattern is a value, so nothing is reserved for NA
ip <- as.bytes(as.raw(c(255, 255, 255, 255)), 4L, "unsigned", na = FALSE)
ip
bytesHasNA(ip)

## reading a column of them
tf <- tempfile()
writeLines(c("id,name", "9223372036854775807,a", "1,b"), tf)
str(read.csv(tf, colClasses = c(id = "int64")))
unlink(tf)
}
\keyword{classes}

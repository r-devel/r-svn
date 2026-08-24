% File src/library/base/man/xinteger.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2026 R Core Team
% Distributed under GPL 2 or later

\name{xinteger}
\alias{xinteger}
\alias{as.xinteger}
\alias{xintegerHasNA}
\alias{xintegerRaw}
\alias{xintegerKind}
\alias{xintegerWidth}
\alias{is.xinteger}
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
\title{Vectors of Fixed-Width Integers}
\description{
  Vectors of signed and unsigned fixed-width integers.  The width is a
  property of each vector rather than of the underlying atomic type.
}
\usage{
xinteger(length = 0L, width = 1L,
      kind = c("unsigned", "signed"), na = TRUE)

as.xinteger(x, width = 1L,
         kind = c("unsigned", "signed"), na = TRUE)

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

is.xinteger(x)
xintegerWidth(x)
xintegerKind(x)
xintegerHasNA(x)
xintegerRaw(x)
}
\arguments{
  \item{length}{a non-negative integer giving the number of
    \emph{elements} (not bytes).}
  \item{width}{the size of one element in bytes: 1, 2, 4, 8 or 16,
    that is, 8, 16, 32, 64 or 128 bits.}
  \item{kind}{how the bytes of an element are to be read; see
    \sQuote{Details}.}
  \item{na}{logical: should one value of the width be reserved as the
    \code{NA} sentinel?}
  \item{x}{for \code{as.xinteger}, a raw, character, integer, logical or
    double vector, or another \code{xinteger} vector; for the predicates,
    any object; for the accessors, an \code{xinteger} vector.}
}
\details{
  Unlike every other vector type, the element size of an \code{xinteger}
  vector is a property of the vector rather than of the type.
  \code{\link{length}} counts elements, so a length-100 vector of width
  8 holds 800 bytes.  \code{\link{typeof}} reports \code{"xinteger"} for
  every such vector, closely following the underlying \code{XINTSXP}.
  \code{\link{storage.mode}} reports the detailed representation built
  from the kind and width:

  \tabular{lll}{
    \code{kind} \tab \code{storage.mode} \tab holds \cr
    \code{"unsigned"} \tab \code{"uint64"} \tab an unsigned integer of
      \code{8 * width} bits \cr
    \code{"signed"} \tab \code{"int64"} \tab a two's complement integer
      of \code{8 * width} bits
  }

  The two kinds use the platform's \emph{native} byte order, so native
  data can be copied directly and \code{xintegerRaw} round-trips exactly.
  Sorting is nevertheless portable because it uses value order, and
  \code{\link{serialize}} normalises byte order.  Serialized vectors
  require workspace format version 4; the serialization functions
  select it automatically when no version is specified.

  With \code{na = TRUE} (the default) one value of the width is set aside
  as a sentinel and read as \code{\link{NA}}, as \code{NA_integer_} and
  \code{NA_real_} are for their own types: all bits set for the unsigned
  kind, the most negative value for the signed kind.  Pass
  \code{na = FALSE} when every bit pattern in the data is a legitimate
  value -- at width 1 the sentinel costs 1 in 256, and at width 4
  unsigned it costs the maximum unsigned value.  Such a vector has no
  sentinel to spare, so every operation that would produce a missing
  value -- an out-of-range subscript, \code{length<-} growth, a join
  miss, arithmetic overflow -- raises an error instead.  Whether a
  sentinel is reserved, together with the width and kind, forms the
  element type.  Two \code{xinteger} vectors must agree on all three to be
  combined, compared or matched unless one is converted explicitly.
  A storage-mode name identifies the width and kind but not the
  sentinel, so it is not quite the whole type.  Interfaces that need
  all three, such as \code{\link{readBin}}, \code{\link{scan}} and
  \code{\link{vector}}, accept an existing vector as a prototype.
  Thus \code{vector(x, n)} reproduces a vector created with
  \code{na = FALSE}; \code{vector(storage.mode(x), n)} uses the same
  width and kind with the default sentinel policy.  The incomplete
  structural name \code{vector("xinteger", n)} is an error.

  There is no literal for a missing \code{xinteger} value, since the
  sentinel's bit pattern depends on the width and the kind:
  \code{as.xinteger(NA, width, kind)} writes one, and \code{\link{rep}}
  makes a vector of them.

  Arithmetic (\code{+}, \code{-}, \code{*}, \code{\%/\%}, \code{\%\%} and
  unary minus) is defined at every width.  Integer and logical operands
  narrow in, with a value that
  does not fit becoming \code{NA} with a warning, as integer overflow
  does.  A double or complex operand instead promotes a numeric
  fixed-width vector to that ordinary numeric type.  Conversion to
  double warns only when an actual value loses precision.  Division by
  zero is \code{NA} without a
  warning, as it is for integer.  The bitwise operations
  (\code{\link{bitwAnd}} and friends) apply to both kinds and every
  width.

  Comparison, \code{\link{sort}}, \code{\link{order}},
  \code{\link{match}}, \code{\link{unique}}, \code{\link{table}} and
  \code{\link{factor}} work for both kinds and all widths.  An integer or
  logical operand narrows in here as it does in arithmetic, but a value
  the width cannot hold is not treated as missing: for a comparison it
  lies below or above every element, so \code{x > -1L} is \code{TRUE}
  for an unsigned vector rather than \code{NA}, and for \code{match}
  it is neither present nor a match.  \code{\link{min}},
  \code{\link{max}} and \code{\link{pmin}} refuse the same pairs
  \code{\link{c}} refuses -- so that \code{\link{range}}, whose answer
  goes through \code{c}, cannot fail on arguments they accept.
  A numeric fixed-width vector is compared with a double using the
  double's exact binary value, without first rounding the integer;
  matching likewise succeeds only for an exactly integral double.
  A character operand promotes an \code{xinteger} operand to character,
  in comparison and matching as in \code{c} and \code{unlist}: the
  character form is exact and reversible at every width, so nothing is
  lost.  What such a comparison inherits is string collation, exactly as
  \code{1L == "1"} does, so \code{as.uint64(10) < "9"} is \code{TRUE}.

  Unit-step \code{\link{seq}} and \code{:} sequences between two scalar
  endpoints of the same fixed-width type retain that type and remain
  exact above \eqn{2^{53}}.  The \code{\link{Math}} group accepts
  fixed-width vectors: \code{abs} is exact in the fixed-width
  type, \code{sign} is exact, and double-valued functions use the checked
  conversion described above.

  \code{\link{as.integer}}, \code{\link{as.numeric}} and
  \code{\link{as.raw}} convert each kind, behaving as for an integer
  vector when the value
  will not fit.  \code{\link{mean}} is a double like every other mean
  in \R, but is formed from an exact total rather than element by
  element.  Where that total is itself a double, the single division
  left is correctly rounded and nothing has been lost to the fixed
  width: the mean of 1, 2 and 4 is 7/3 whatever the values were stored
  in.  It warns only where the total needs more than a double's 53
  significant bits \emph{and} the value returned is then not the exact
  rational mean.

  At the R interface, both kinds are numeric:
  \code{is.numeric} is true, \code{mode} is \code{"numeric"}, and their
  implicit \code{\link{class}} is the semantic type name such as
  \code{"uint64"}.  \code{\link{get}}, \code{\link{exists}} and
  \code{\link{mget}} follow that: \code{mode = "numeric"} finds any of
  them, as it finds integer and double vectors, while a detailed
  storage-mode name matches the width and kind, with either sentinel
  policy.  A package wanting its own semantics
  can add a class -- \code{class(x) <- c("myint", class(x))} --
  which dispatches ahead of the methods for the type and leaves the
  vector working underneath.  S4 dispatch needs the implicit class
  registered first; a semantic class is derived from the vector rather
  than fixed, so \pkg{methods} cannot know them all in advance.
}
\value{
  \code{xinteger} returns a zero-filled vector of \code{length} elements.

  \code{as.xinteger} converts \code{x}.  A raw vector is reinterpreted
  verbatim, so its length must be a multiple of \code{width}.  A
  character vector is parsed as decimal, the inverse of
  \code{\link{as.character}}, with
  \code{NA_character_} giving \code{NA}, and text that is not a number
  or does not fit giving \code{NA} with a warning.  Integer and logical
  vectors narrow.

  A double is taken where it is exactly the integer it appears to be:
  a finite value with no fractional part, within the range of the width
  and kind, converts exactly, at any width.  A fractional value or an
  infinity gives \code{NA} with a warning, as does one the type cannot
  hold, exactly as for \code{\link{as.integer}}.  Note that this can
  only judge the double it is given: a literal beyond \eqn{2^{53}}{2^53}
  has already been rounded before \code{as.xinteger} sees it, and
  \code{as.xinteger(9007199254740993)} is indistinguishable from
  \code{as.xinteger(9007199254740992)}.  Write such a value as text, which
  is exact at every magnitude.

  Another \code{xinteger} vector is converted between widths and kinds,
  preserving the value: a value that does not fit
  the target becomes \code{NA} with a warning, as it does everywhere
  else.
  \code{\link{as.vector}(x, "int64")} and \code{\link{storage.mode}(x)
  <- "int64"} do the same thing, except that the mode name cannot say
  whether \code{NA} is reserved, so those two keep whatever \code{x}
  already had.

  \code{as.xinteger} and \code{as.vector} drop attributes, as the rest of
  the \code{as.*} family does, including when \code{x} already has the
  requested width and kind; \code{storage.mode<-} keeps them.

  \code{is.xinteger} tests the shared \code{XINTSXP} storage
  representation, as \code{\link{typeof}} reports it, and is therefore
  \code{TRUE} for both kinds.
  \code{xintegerWidth} returns the element size in bytes, \code{xintegerKind}
  the kind as a string, and \code{xintegerHasNA} whether the vector has an
  \code{NA} sentinel.

  \code{xintegerRaw} returns the whole payload as a \code{\link{raw}}
  vector, \code{length(x) * xintegerWidth(x)} bytes in storage order.
}
\seealso{
  \code{\link{readBin}} and \code{\link{scan}} to read these from a
  file, \code{\link{read.table}} for the \code{colClasses} form,
  \code{\link{bitwAnd}} for masking, \code{\link{vector}} and
  \code{\link{storage.mode}} for the detailed storage names.

  \code{\link{raw}} for raw vectors whose length is a byte count.
}
\examples{
## a 64-bit key read from text, at full precision
id <- as.xinteger(c("9223372036854775807", "-9007199254740993"), 8L, "signed")
id
typeof(id)
storage.mode(id)
as.numeric("9223372036854775807")   # what a double would have made of it

sort(id)
id[1] \%in\% id

## widening and narrowing preserve the value
as.vector(id, "int128")
as.vector(id, "int32")              # neither fits: NA, with a warning

## Knuth's multiplicative hash for bucketing: multiply by 2^64/phi and
## keep the low 64 bits.  That product needs 128 bits to hold it, so
## widen first and chop the top half off afterwards.
key  <- as.xinteger(c("1", "2", "3", "9223372036854775807"), 8L, "unsigned")
phi  <- as.xinteger("11400714819323198485", 16L, "unsigned")
prod <- as.vector(key, "uint128") * phi
h    <- as.vector(bitwShiftR(bitwShiftL(prod, 64L), 64L), "uint64")
h
bitwShiftR(h, 61L)                  # the top 3 bits, so 8 buckets

## every bit pattern is a value, so nothing is reserved for NA
ip <- as.xinteger(as.raw(c(255, 255, 255, 255)), 4L, "unsigned", na = FALSE)
ip
xintegerHasNA(ip)

## reading a column of them
tf <- tempfile()
writeLines(c("id,name", "9223372036854775807,a", "1,b"), tf)
str(read.csv(tf, colClasses = c(id = "int64")))
unlink(tf)
}
\keyword{classes}

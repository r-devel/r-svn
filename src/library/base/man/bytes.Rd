% File src/library/base/man/bytes.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2026 R Core Team
% Distributed under GPL 2 or later

\name{bytes}
\alias{bytes}
\alias{as.bytes}
\alias{bytesNA}
\alias{bytesHasNA}
\alias{bytesRaw}
\alias{bytesKind}
\alias{bytesWidth}
\alias{is.bytes}
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

bytesNA(length = 1L, width = 1L,
        kind = c("opaque", "unsigned", "signed"))

is.bytes(x)
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
  \item{na}{logical: should one value of the width be reserved to mean
    \code{NA}?}
  \item{x}{for \code{as.bytes}, a raw, character, integer or logical
    vector; elsewhere a \code{bytes} vector.}
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
  that saved files do too.

  With \code{na = TRUE} (the default) one value of the width is reserved
  to mean \code{NA}: all bits set for the opaque and unsigned kinds, the
  most negative value for the signed kind.  Pass \code{na = FALSE} when
  every bit pattern in the data is a legitimate value -- at width 1 the
  reservation costs 1 in 256, and at width 4 unsigned it costs the IPv4
  broadcast address.  Such a vector cannot represent a missing value, so
  every operation that would produce one -- an out-of-range subscript,
  \code{length<-} growth, a join miss, arithmetic overflow -- raises an
  error instead.  Whether a value is reserved is part of the type, as
  the kind and the width are, so vectors that disagree about it cannot
  be combined or compared.

  Arithmetic (\code{+}, \code{-}, \code{*}, \code{\%/\%}, \code{\%\%} and
  unary minus) is defined for the numeric kinds at widths 1, 2, 4, 8 and
  16; wider elements are storage only.  Integer and logical operands
  narrow in, with a value that does not fit becoming \code{NA} with a
  warning, as integer overflow does.  A double operand is an error
  rather than a silent loss of precision.  The bitwise operations
  (\code{\link{bitwAnd}} and friends) apply to every kind and every
  width, and are what the opaque kind is for.

  Comparison, \code{\link{sort}}, \code{\link{order}},
  \code{\link{match}}, \code{\link{unique}}, \code{\link{table}} and
  \code{\link{factor}} work for all kinds and widths.
}
\value{
  \code{bytes} and \code{bytesNA} return a vector of \code{length}
  elements, zero-filled and all-\code{NA} respectively.

  \code{as.bytes} converts \code{x}.  A raw vector is reinterpreted
  verbatim, so its length must be a multiple of \code{width}.  A
  character vector is parsed -- decimal for the numeric kinds, hex for
  the opaque one, the inverse of \code{\link{as.character}} -- with
  \code{NA_character_} giving \code{NA}, and text that is not a number
  or does not fit giving \code{NA} with a warning.  Integer and logical
  vectors narrow.

  \code{is.bytes} returns \code{TRUE} for a \code{bytes} vector.
  \code{bytesWidth} returns the element size in bytes, \code{bytesKind}
  the kind as a string, and \code{bytesHasNA} whether a value is
  reserved for \code{NA}.

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

## an opaque byte string: compared and hashed, never read as a number
uid <- as.bytes(c("20010db8000000000000000000000001",
                  "20010db8000000000000000000000002"), 16L)
uid
bytesWidth(uid)
bitwAnd(uid[1], as.bytes(paste0(strrep("ff", 6), strrep("00", 10)), 16L))

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

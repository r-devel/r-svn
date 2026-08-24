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
\alias{as.int64}
\alias{as.uint64}
\title{64-Bit Integer Vectors Backed by ALTSXP}
\description{
  Vectors of signed and unsigned 64-bit integers implemented as built-in
  classes of the opaque \code{ALTSXP} ALTREP type.
}
\usage{
xinteger(length = 0L, width,
      kind = c("unsigned", "signed"), na = TRUE)

as.xinteger(x, width,
         kind = c("unsigned", "signed"), na = TRUE)

as.int64(x, na = TRUE)
as.uint64(x, na = TRUE)

is.xinteger(x)
xintegerWidth(x)
xintegerKind(x)
xintegerHasNA(x)
xintegerRaw(x)
}
\arguments{
  \item{length}{a non-negative integer giving the number of elements.}
  \item{width}{the size of one element in bytes.  This prototype supports
    only 8.}
  \item{kind}{whether the elements are unsigned or signed two's-complement
    integers.}
  \item{na}{logical: should one 64-bit value be reserved as the \code{NA}
    sentinel?}
  \item{x}{for \code{as.xinteger}, a raw, character, integer, logical or
    double vector, or another built-in 64-bit integer vector; for the
    predicate, any object; for the accessors, such a vector.}
}
\details{
  \code{int64} and \code{uint64} are two built-in ALTREP classes whose base
  \code{SEXPTYPE} is \code{ALTSXP}.  Consequently \code{typeof} reports
  \code{"alt"}; \code{storage.mode} and the implicit class report
  \code{"int64"} or \code{"uint64"}.  \code{is.xinteger} recognizes these
  two classes, rather than every package-defined \code{ALTSXP} class.

  The exact native-endian bytes live in the class's private raw backing
  store.  Element access, arithmetic, comparison, hashing, formatting,
  summaries and coercion are supplied by ALTSXP methods.  Generic
  \code{DATAPTR} access to the outer object is intentionally unavailable.

  With \code{na = TRUE} (the default), all bits set is reserved for unsigned
  \code{NA}, and the most negative value is reserved for signed \code{NA}.
  With \code{na = FALSE}, every bit pattern is a value; an operation that
  would need to produce a missing value raises an error.  The kind and
  sentinel policy must agree when two such vectors are combined, compared
  or matched.

  Arithmetic \code{+}, \code{-}, \code{*}, \code{\%/\%}, \code{\%\%} and
  unary minus retain the 64-bit class.  Integer and logical operands narrow
  into it.  Double or complex operands promote it to the corresponding
  ordinary numeric type.  Comparison uses the exact integer value, including
  above \eqn{2^{53}}.  Sorting, matching, hashing, formatting and summaries
  use the same exact representation.

  The class serializes its own portable, big-endian ALTREP state.  This uses
  the existing serialization format version 3; no standard ALTSXP payload
  has been added to the stream format.
}
\value{
  \code{xinteger} returns a zero-filled vector.  \code{as.xinteger} converts
  \code{x}; raw input is reinterpreted verbatim and character input is parsed
  as exact decimal text.  A double is accepted only when its represented
  value is finite, integral and in range.  For values above \eqn{2^{53}}, use
  character input to avoid prior rounding by the double parser.

  \code{xintegerWidth} returns 8, \code{xintegerKind} returns the kind,
  \code{xintegerHasNA} reports the sentinel policy, and \code{xintegerRaw}
  returns the native-endian backing bytes.
}
\seealso{
  \code{\link{readBin}} and \code{\link{scan}} can take \code{"int64"},
  \code{"uint64"}, or a prototype as appropriate.
}
\examples{
id <- as.int64(c("9223372036854775807", "-9007199254740993"))
id
typeof(id)
storage.mode(id)
sort(id)
id[1] \%in\% id

key <- as.uint64(c("1", "2", "18446744073709551614"))
format(key)
sum(as.int64(c("-2", "3")))

## every bit pattern is a value, so nothing is reserved for NA
all.bits <- as.uint64("18446744073709551615", na = FALSE)
xintegerHasNA(all.bits)

## reading a column of them
tf <- tempfile()
writeLines(c("id,name", "9223372036854775807,a", "1,b"), tf)
str(read.csv(tf, colClasses = c(id = "int64")))
unlink(tf)
}
\keyword{classes}

% File src/library/base/man/int64.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2026 R Core Team
% Distributed under GPL 2 or later

\name{int64}
\alias{int64}
\alias{uint64}
\alias{as.int64}
\alias{as.uint64}
\alias{is.int64}
\alias{is.uint64}
\title{64-Bit Integer Vectors}
\description{
  Create, coerce to, and test for vectors of signed or unsigned 64-bit
  integers.
}
\usage{
int64(length = 0L)
uint64(length = 0L)

as.int64(x, na = TRUE)
as.uint64(x, na = TRUE)

is.int64(x)
is.uint64(x)
}
\arguments{
  \item{length}{a non-negative integer giving the length of the vector.}
  \item{x}{object to be coerced or tested.}
  \item{na}{logical: should the vector be able to represent \code{NA}?  See
    \sQuote{Details}.}
}
\details{
  These vectors hold exact 64-bit integers, a range that a \code{double}
  cannot represent without rounding and that an \code{integer} cannot hold
  at all.

  They are \abbr{ALTREP} objects of the opaque \code{ALTSXP} type, so
  \code{\link{typeof}} reports \code{"int64"} or \code{"uint64"} rather
  than any of \R's built-in storage types.  Code that has not been taught
  about 64-bit integers therefore fails rather than silently reading the
  payload as something else.

  \code{na = TRUE}, the default, reserves one bit pattern for \code{NA}:
  \code{-2^63} for \code{int64} and \code{2^64 - 1} for \code{uint64}.
  \code{na = FALSE} gives that pattern up and makes the whole 64-bit range
  available for data, which is what a column read from a source with no
  concept of a missing value wants.  Whether a vector can be \code{NA} is a
  property of the object, not of the type: two vectors of the same type may
  differ, and they interoperate.  \R{} reports an error rather than
  introducing an \code{NA} into a vector that has no room for one.

  Each vector is read in its own domain, so a whole-range vector's extreme
  value is data even when it is compared with a vector that reserves that
  pattern.  An operation with a single result, such as \code{+} or
  \code{sum}, needs one domain for its answer: it uses the wider one, which
  fails if the whole-range operand actually holds the reserved value, just
  as \code{\link{c}} does.

  Both types are reachable by name, so \code{\link{vector}("int64", n)},
  \code{\link{as.vector}(x, "int64")} and \code{\link{storage.mode}(x) <-
  "int64"} work as they do for the built-in types, for any \code{x} the
  class has a conversion from.  A name gives the default form, which
  reserves a pattern for \code{NA}; \code{as.int64(x, na = FALSE)} is the
  only way to ask for the other one, since whether a vector can be
  \code{NA} is a property of the object rather than of the type.

  \code{\link{typeof}} is also the implicit class, so \code{print.int64}
  or \code{format.uint64} methods are dispatched to as usual.
  \code{\link{deparse}} and \code{\link{dput}} write a call to
  \code{as.int64} or \code{as.uint64} that reproduces the vector,
  including its \code{na} setting; values outside the range of an
  \code{integer} are written as character, which is exact.

  Arithmetic between a 64-bit integer vector and a \code{logical},
  \code{raw} or \code{integer} vector is exact and 64-bit.  A
  \code{double} operand, and the \code{/} and \code{^} operators, promote
  the result to \code{double} as they do for \code{integer} arithmetic.
  \code{int64} and \code{uint64} have no representation in common and
  cannot be mixed.

  Overflow gives \code{NA} with a warning, or an error for a vector built
  with \code{na = FALSE}.
}
\value{
  \code{int64} and \code{uint64} return a vector of the given length with
  every element zero.

  \code{as.int64} and \code{as.uint64} return a 64-bit integer vector.
  \code{x} may be logical, raw, integer, double, character, or another
  64-bit integer vector; values outside the representable range become
  \code{NA} with a warning.  Reading from character is how a value beyond
  the exact range of a \code{double} is entered.

  Converting between \code{int64} and \code{uint64} is available only as
  an explicit request: \code{as.int64(x)}, \code{as.uint64(x)} and
  \code{mode(x) <- "int64"} check each value against the target range,
  whereas \code{as.vector(x, "int64")} and
  \code{storage.mode(x) <- "int64"} report an error.  Those two ask the
  class for a conversion, and the two classes deliberately have none
  between them: it is the same answer that makes \code{c(x, y)} a list
  and refuses to mix them in arithmetic.

  \code{is.int64} and \code{is.uint64} return \code{TRUE} or \code{FALSE}.
}
\seealso{
  \code{\link{integer}}, \code{\link{double}},
  \code{\link{.Machine}} for the range of \R's other numeric types.

  \code{\link{scan}} and \code{\link{readBin}} read into these vectors
  directly, as does \code{read.table(colClasses = "int64")}: reading
  through \code{double} would round the values first.
}
\examples{
x <- as.int64(c(1, 2, 3))
typeof(x)
x * as.int64(1000000000L)

## a value no double can hold exactly
big <- as.int64("9007199254740993")
as.character(big)
as.character(big + as.int64(1L))
print(2^53 + 1, digits = 17)   # the same value as a double

## other integer types promote, a double demotes the result
typeof(x + 1L)
typeof(x + 1.5)

## giving up NA buys the whole range
n <- as.int64("-9223372036854775808", na = FALSE)
as.character(n)
is.na(n)

## read without going through a double first
tf <- tempfile()
writeLines(c("id", "9007199254740992", "9007199254740993"), tf)
read.csv(tf, colClasses = "int64")$id   # distinct
read.csv(tf)$id                         # both round to 2^53
unlink(tf)
}
\keyword{classes}
\keyword{arith}

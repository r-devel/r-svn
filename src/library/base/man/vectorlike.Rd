% File src/library/base/man/vectorlike.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2026 R Core Team
% Distributed under GPL 2 or later

\name{vector_like}
\alias{.vectorlike}
\alias{.arraylike}
\title{Vectors and Arrays of the Element Type of an Object}
\description{
  Create a vector or an array whose element type is that of an existing
  object.
}
\usage{
.vectorlike(x, length = 0L)
.arraylike(x, dim = length(x), dimnames = NULL)
}
\arguments{
  \item{x}{an atomic, list or expression vector, taken for its element
    type.  Attributes on the vector are allowed.}
  \item{length}{a non-negative integer specifying the desired length, as
    for \code{\link{vector}}.}
  \item{dim, dimnames}{the shape of the result, as for
    \code{\link{array}}.}
}
\details{
  \code{\link{vector}} and \code{\link{array}} are told an element type
  by name.  Not every element type has one: a \code{\link{xinteger}}
  vector's storage mode gives its width and its kind, but whether it
  reserves a sentinel for \code{NA} is a property of the vector that no
  name states.  \code{.vectorlike} and \code{.arraylike} take the type
  from an object instead, so \code{.vectorlike(x, n)} reproduces the
  element type of \code{x} whatever \code{x} is, where
  \code{vector(typeof(x), n)} reproduces only what a name can express.

  Only the element type comes from \code{x}; its contents and attributes
  are not used.  The one exception is the default \code{dim} of
  \code{.arraylike}, which is \code{length(x)}, as it is for
  \code{\link{array}}.  The result is filled as \code{vector} fills one
  -- zero for the atomic types, \code{NULL} for a list or an expression
  -- and carries no attributes beyond \code{dim} and \code{dimnames}.
  Use \code{array(x, dim)} to carry the contents of \code{x} over instead.

  These functions are dot-prefixed because they are low-level allocation
  infrastructure.  They are documented so package code that must preserve a
  complete element type can use them, but ordinary user code will normally
  use \code{vector}, \code{array} or an \code{xinteger} constructor.

  \code{x} must have an atomic, list or expression vector type, so
  \code{.vectorlike(NULL, n)} and \code{.vectorlike(pairlist(), n)} are
  errors.  A character vector is an object like any other and never a
  name, so \code{.vectorlike("integer", 2)} is a character vector of
  length two.
}
\value{
  For \code{.vectorlike}, a vector of the element type of \code{x} and of
  length \code{length}.

  For \code{.arraylike}, an array of that element type with dimensions
  \code{dim}.
}
\seealso{
  \code{\link{vector}} and \code{\link{array}}, which take the name of a
  mode; \code{\link{typeof}} and \code{\link{storage.mode}} for the
  names there are; \code{\link{xinteger}} for the type that needs more
  than one.
}
\examples{
.vectorlike(1:3, 2L)		# integer(2)
.arraylike(letters, c(2L, 2L))	# a 2 by 2 character matrix

## a name cannot say 'na = FALSE', so an object has to
x <- as.uint64(c(1, 2), na = FALSE)
storage.mode(x)
xintegerHasNA(.vectorlike(x, 2L))
xintegerHasNA(vector(storage.mode(x), 2L))
}
\keyword{classes}

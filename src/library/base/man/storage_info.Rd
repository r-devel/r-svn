% File src/library/base/man/storage_info.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2026 R Core Team
% Distributed under GPL 2 or later

\name{storage_info}
\alias{.storage_info}
\title{Information About Vector Storage}
\description{
  Report properties of the storage representation of a vector.
}
\usage{
.storage_info(x)
}
\arguments{
  \item{x}{an atomic, list or expression vector.}
}
\details{
  This function describes the underlying vector storage; classes and other
  attributes do not affect its result.

  Here \code{nullable} means that the element representation has a sentinel
  value which is interpreted as \code{NA}.  It does not say whether \code{x}
  currently contains an \code{NA}; use \code{\link{anyNA}} for that.  For an
  \code{\link{xinteger}} vector the sentinel policy is a property of the
  vector, so otherwise identical vectors can differ in \code{nullable}.

  \code{NA} and \code{NULL} are distinct.  An \code{NA} represents a missing
  element within a vector.  \code{NULL} is a separate, length-zero object,
  generally used to represent the absence of an object, and is not an
  \code{NA} sentinel.  A list can contain \code{NULL} as an ordinary
  referenced object, but this does not make its storage nullable.  A list
  slot can likewise refer to an atomic \code{NA} object, without reserving
  an \code{NA} sentinel in the list storage itself; consequently
  \code{anyNA(list(NA))} is true even though the list's reported
  \code{nullable} property is false.

  The reported element size is the number of bytes occupied by an element in
  the corresponding C vector storage.  For character, list and expression
  vectors this is the size of a \code{SEXP} pointer slot, not the memory used
  by the referenced object.  For an \code{xinteger} it is the width of its
  byte representation; this does not imply that the elements can safely be
  accessed by casting the byte payload to a C integer pointer.
}
\value{
  A named list with components:
  \item{typeof}{the value returned by \code{\link{typeof}}.}
  \item{storage_mode}{the value returned by \code{\link{storage.mode}}.}
  \item{element_size}{the element size in bytes.}
  \item{signed}{\code{TRUE} for signed integer storage, \code{FALSE} for
    unsigned integer storage, and \code{NA} for other representations.}
  \item{nullable}{whether the element representation encodes an \code{NA}
    sentinel.}
}
\examples{
.storage_info(1L)
.storage_info(as.uint64(1))
.storage_info(as.uint64(1, na = FALSE))

## NULL in a list is not a missing element.
.storage_info(list(NULL))$nullable
anyNA(list(NULL))
is.null(list(NULL)[[1L]])
}
\seealso{
  \code{\link{typeof}}, \code{\link{storage.mode}},
  \code{\link{xinteger}}, and \code{\link{anyNA}}.
}

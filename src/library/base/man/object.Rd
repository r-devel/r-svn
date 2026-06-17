% File src/library/base/man/object.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2026 R Core Team
% Distributed under GPL 2 or later

\name{object}
\alias{object}

\title{Create a Bare Object}
\description{
  Creates a \dQuote{bare} object, i.e., an object of internal type
  \code{"object"}, setting its \code{class} (and optionally further
  attributes).  This is the internal type also underlying S4 objects,
  but without the S4 bit set, and is used, e.g., for instances of
  \pkg{S7} classes.

  This is a low-level facility intended for expert use only, e.g., by
  packages implementing class systems; it is not needed for ordinary
  programming with S4 or \pkg{S7} classes, which provide their own
  ways to create objects.
}
\usage{
object(class, ...)
}
\arguments{
  \item{class}{the \code{\link{class}} of the object, typically a
    \link{character} vector.}
  \item{\dots}{further named attributes to set on the object.}
}
\value{
  An object of type \code{"object"} (see \code{\link{typeof}}) whose
  attributes are \code{list(class = class, \dots)}, for which
  \code{\link{isS4}} is false.
}
\details{
  Such objects carry all their information in attributes.  Beyond those
  set by \code{object()}, more can be added in the usual ways, e.g., via
  \code{\link{structure}} or \code{\link{attr<-}}.  \code{\link{deparse}()},
  \code{\link{dput}()} and \code{\link{dump}()} use \code{object()},
  passing the attributes as named arguments, when deparsing objects of
  this type.
}
\seealso{
  \code{\link{isS4}}, \code{\link{asS3}}, \code{\link{typeof}}.
}
\examples{
x <- object(class = "S7_object")
typeof(x)    # "object"
is.object(x) # TRUE
isS4(x)      # FALSE

## the structure underlying a (minimal) S7 object :
dput(object(class = "S7_object"))
}
\keyword{programming}
\keyword{internal}

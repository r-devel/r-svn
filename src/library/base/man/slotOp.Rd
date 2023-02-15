% File src/library/base/man/slotOp.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{slotOp}
\title{Extract or Replace A Slot}
\alias{@}
\alias{@<-}
\description{
  Extract or replace the contents of a slot or property of an object.
}
\usage{
object@name
object@name <- value
}
\arguments{
  \item{object}{An object from a formally defined (S4) class, or 
      otherwise an class object with an `@` or `@<-` S3 or S7 method.}
  \item{name}{The name of the slot, supplied as a character string or 
      unquoted symbol. If \code{object} is S4, \code{name} must be the 
      name of a slot in the definition of the class of \code{object}.}
  \item{value}{A suitable replacement value for the slot or property.
}
\details{
        
  if \code{object} is S4, these operators are for slot access, and 
  are enabled only when package \pkg{methods} is loaded (as per default). 
  The slot must be formally defined. (There is an exception 
  for name \code{.Data}, intended for internal use only.) 
  The replacement operator checks that the slot
  already exists on the object (which it should if the object is really
  from the class it claims to be).
  See \code{\link{slot}} for further details, in particular
  for the differences between \code{slot()} and the \code{@} operator.

  If \code{object} is not S4, a suitable S3 method for `@` or `@<-` is 
  searched for. If no method is found, an error is signaled.

  These are internal generic operators: see \link{InternalMethods}.
}
\value{
  The current contents of the slot.
}
\seealso{
  \code{\link{Extract}}, \code{\link{slot}}
}
\keyword{manip}

% File src/library/base/man/replace.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{replace}
\title{Replace Values in a Vector}
\usage{
replace(x, list, values)
}
\alias{replace}
\description{
\code{replace} replaces the values in \code{x}
with indices given in \code{list} by those given in \code{values}.
If necessary, the values in \code{values} are recycled.
}
\arguments{
  \item{x}{a vector.}
  \item{list}{an index \link{vector}.}
  \item{values}{replacement values.}
}
\value{
  A vector with the values replaced.
}
\references{
  \bibshow{R:Becker+Chambers+Wilks:1988}
}
\note{
  \code{x} is unchanged: remember to assign the result.
}
\seealso{
  \link{Extract} to replace in objects. 
}
\value{
  x <- 1:5
  (y <- replace(x, c(2, 5), c(8, 9)))
  x # Not modified

   # Works with any vector
  z <- list(A = "a", B = 2, C = diag(5))
  replace(z, c(1, 2, 3), list(5, diag(3), "A"))
}
\keyword{manip}

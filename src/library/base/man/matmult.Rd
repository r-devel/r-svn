% File src/library/base/man/matmult.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{matmult}
\alias{\%*\%}
\alias{matmult}
\title{Matrix Multiplication}
\description{
  Multiplies two matrices, if they are conformable.  If one argument is
  a vector, it will be promoted to either a row or column matrix to make
  the two arguments conformable.  If both are vectors of the same
  length, it will return the inner product (as a matrix).
}
\usage{
x \%*\% y
}
\arguments{
  \item{x, y}{numeric or complex matrices or vectors.}
}
\details{
  When a vector is promoted to a matrix, its names are not
  promoted to row or column names, unlike \code{\link{as.matrix}}.

  Promotion of a vector to a 1-row or 1-column matrix happens when one
  of the two choices allows \code{x} and \code{y} to get conformable
  dimensions.

  This operator is a generic function: methods can be written for it
  individually or via the \code{\link[=S3groupGeneric]{matOps}} group
  generic function; it dispatches to S3 and S4 methods.  Methods need to be
  written for a function that takes two arguments named \code{x} and \code{y}.
}
\value{
  A double or complex matrix product.  Use \code{\link{drop}} to remove
  dimensions which have only one level.
}
\note{
  The propagation of NaN/Inf values, precision, and performance of matrix
  products can be controlled by \code{\link{options}("matprod")}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  For matrix \emph{cross} products, \code{\link{crossprod}()} and
  \code{tcrossprod()} are typically preferable.
  \code{\link{matrix}}, \code{\link{Arithmetic}}, \code{\link{diag}}.
}
\examples{
x <- 1:4
(z <- x \%*\% x)    # scalar ("inner") product (1 x 1 matrix)
drop(z)             # as scalar

y <- diag(x)
z <- matrix(1:12, ncol = 3, nrow = 4)
y \%*\% z
y \%*\% x
x \%*\% z
}
\keyword{array}
\keyword{arith}

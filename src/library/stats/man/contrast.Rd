% File src/library/stats/man/contrast.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{contrast}
\alias{contr.helmert}
\alias{contr.poly}
\alias{contr.sum}
\alias{contr.treatment}
\alias{contr.SAS}
\title{(Possibly Sparse) Contrast Matrices}
\description{
  Return a matrix of contrasts.
}
\usage{
contr.helmert(n, contrasts = TRUE, sparse = FALSE)
contr.poly(n, scores = 1:n, contrasts = TRUE, sparse = FALSE)
contr.sum(n, contrasts = TRUE, sparse = FALSE)
contr.treatment(n, base = 1, contrasts = TRUE, sparse = FALSE)
contr.SAS(n, contrasts = TRUE, sparse = FALSE)
}
\arguments{
  \item{n}{a vector of levels for a factor, or the number of levels.}
  \item{contrasts}{a logical indicating whether contrasts should be
    computed.}
  \item{sparse}{logical indicating if the result should be sparse
    (of class \code{\linkS4class[Matrix]{dgCMatrix}}), using
    package \CRANpkg{Matrix}.}
  \item{scores}{the set of values over which orthogonal polynomials are
    to be computed.}
  \item{base}{an integer specifying which group is considered the
    baseline group. Ignored if \code{contrasts} is \code{FALSE}.}
}
\details{
  These functions are used for creating contrast matrices for use in
  fitting analysis of variance and regression models.  The columns of
  the resulting matrices contain contrasts which can be used for coding
  a factor with \code{n} levels.  The returned value contains the
  computed contrasts.  If the argument \code{contrasts} is \code{FALSE}
  a square indicator matrix (the dummy coding) is returned \bold{except}
  for \code{contr.poly} (which includes the 0-degree, i.e.\sspace{}constant,
  polynomial when \code{contrasts = FALSE}).

  \code{contr.helmert} returns \I{Helmert} contrasts, which contrast the
  second level with the first, the third with the average of the first
  two, and so on.  \code{contr.poly} returns contrasts based on
  orthogonal polynomials. \code{contr.sum} uses \sQuote{sum to zero
  contrasts}.

  \code{contr.treatment} contrasts each level with the baseline level
  (specified by \code{base}): the baseline level is omitted.  Note that
  this does not produce \sQuote{contrasts} as defined in the standard
  theory for linear models as they are not orthogonal to the intercept.

  \code{contr.SAS} is a wrapper for \code{contr.treatment} that sets
  the base level to be the last level of the factor.  The coefficients
  produced when using these contrasts should be equivalent to those
  produced by many (but not all) SAS procedures.

  For consistency, \code{sparse} is an argument to all these contrast
  functions, however \code{sparse = TRUE} for \code{contr.poly}
  is typically pointless and is rarely useful for
  \code{contr.helmert}.
}
\value{
  A matrix with \code{n} rows and \code{k} columns, with \code{k=n-1} if
  \code{contrasts} is \code{TRUE} and \code{k=n} if \code{contrasts} is
  \code{FALSE}.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical models.}
  Chapter 2 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{contrasts}},
  \code{\link{C}},% ./zC.Rd
  and
  \code{\link{aov}},
  \code{\link{glm}},
  \code{\link{lm}}.
}
\examples{
(cH <- contr.helmert(4))
apply(cH, 2, sum) # column sums are 0
crossprod(cH) # diagonal -- columns are orthogonal
contr.helmert(4, contrasts = FALSE) # just the 4 x 4 identity matrix

(cT <- contr.treatment(5))
all(crossprod(cT) == diag(4)) # TRUE: even orthonormal

(cT. <- contr.SAS(5))
all(crossprod(cT.) == diag(4)) # TRUE

zapsmall(cP <- contr.poly(3)) # Linear and Quadratic
zapsmall(crossprod(cP), digits = 15) # orthonormal up to fuzz
}
\keyword{design}
\keyword{regression}
\keyword{array}

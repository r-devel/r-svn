% File src/library/base/man/jitter.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{jitter}
\alias{jitter}
\title{\sQuote{Jitter} (Add Noise) to Numbers}
\usage{
jitter(x, factor = 1, amount = NULL)
}
\arguments{
  \item{x}{numeric vector to which \emph{jitter} should be added.}
  \item{factor}{numeric; value used in calculation of a when \code{amount}
                is not positive.}
  \item{amount}{numeric; When positive, used to set the maximum and minimum 
    for the randomization). See Details for other cases.
}
\description{
  Add a small amount of noise to a numeric vector.
}
\value{
  \code{jitter(x, \dots)} returns a numeric of the same length as
  \code{x}, but with an \code{amount} of noise added in order to break
  ties.
}
\details{
  The result of \code{r <- x + runif(n, -amount, amount)}
  where \code{n <- length(x)}.

  When \code{amount} is \code{NULL} (the default) 
  \code{amount <- factor * d/5} where \emph{d} is the smallest
  difference between adjacent unique \code{x} values is used.

  If \code{amount == 0}, \code{amount <- factor * z/50} which is 
  the same as in S.
}
\references{
  Chambers, J. M., Cleveland, W. S., Kleiner, B. and Tukey, P.A. (1983)
  \emph{Graphical Methods for Data Analysis.} Wadsworth; figures 2.8,
  4.22, 5.4.

  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S.}
  Wadsworth & Brooks/Cole.
}
\author{Werner Stahel and Martin Maechler, ETH Zurich}
\seealso{
  \code{\link{rug}} which you may want to combine with \code{jitter}.
}
\examples{
round(jitter(c(rep(1, 3), rep(1.2, 4), rep(3, 3))), 3)
## These two 'fail' with S-plus 3.x:
jitter(rep(0, 7))
jitter(rep(10000, 5))
}
\keyword{dplot}
\keyword{utilities}

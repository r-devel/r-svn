% File src/library/stats/man/lowess.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{lowess}
\title{Scatter Plot Smoothing}
\description{
  This function performs the computations for the
  \emph{LOWESS} smoother which uses locally-weighted polynomial
  regression (see the references).
}
\usage{
lowess(x, y = NULL, f = 2/3, iter = 3, delta = 0.01 * diff(range(x)))
}
\alias{lowess}
\arguments{
  \item{x, y}{vectors giving the coordinates of the points in the scatter plot.
    Alternatively a single plotting structure can be specified -- see
    \code{\link{xy.coords}}.}
  \item{f}{the smoother span. This gives the proportion of points in
    the plot which influence the smooth at each value.
    Larger values give more smoothness.}
  \item{iter}{the number of \sQuote{robustifying} iterations which should be
    performed.
    Using smaller values of \code{iter} will make \code{lowess} run faster.}
  \item{delta}{See \sQuote{Details}.  Defaults to 1/100th of the range
    of \code{x}.}
}
\details{
  \code{lowess} is defined by a complex algorithm, the \I{Ratfor} original
  of which (by W. S. Cleveland) can be found in the \R sources as file
  \file{src/library/stats/src/lowess.doc}.  Normally a local linear polynomial fit is
  used, but under some circumstances (see the file) a local constant fit
  can be used.  \sQuote{Local} is defined by the distance to the
  \code{floor(f*n)}-th nearest neighbour, and tricubic weighting is used
  for \code{x} which fall within the neighbourhood.

  The initial fit is done using weighted least squares.  If
  \code{iter > 0}, further weighted fits are done using the product of
  the weights from the proximity of the \code{x} values and case weights
  derived from the residuals at the previous iteration.  Specifically,
  the case weight is Tukey's biweight, with cutoff 6 times the MAD of the
  residuals.  (The current \R implementation differs from the original
  in stopping iteration if the MAD is effectively zero since the
  algorithm is highly unstable in that case.)

  \code{delta} is used to speed up computation: instead of computing the
  local polynomial fit at each data point it is not computed for points
  within \code{delta} of the last computed point, and linear
  interpolation is used to fill in the fitted values for the skipped
  points.
}
\value{
  \code{lowess} returns a list containing components
  \code{x} and \code{y} which give the coordinates of the smooth.
  The smooth can be added to a plot of the original
  points with the function \code{lines}: see the examples.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988).
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Cleveland, W. S. (1979).
  Robust locally weighted regression and smoothing scatterplots.
  \emph{Journal of the American Statistical Association}, \bold{74},
  829--836.
  \doi{10.1080/01621459.1979.10481038}.

  Cleveland, W. S. (1981)
  LOWESS: A program for smoothing scatterplots by robust locally
  weighted regression.
  \emph{The American Statistician}, \bold{35}, 54.
  \doi{10.2307/2683591}.
}
\seealso{\code{\link{loess}}, a newer
  formula based version of \code{lowess} (with different defaults!).
}
\examples{
require(graphics)

plot(cars, main = "lowess(cars)")
lines(lowess(cars), col = 2)
lines(lowess(cars, f = .2), col = 3)
legend(5, 120, c(paste("f = ", c("2/3", ".2"))), lty = 1, col = 2:3)
}
\keyword{smooth}

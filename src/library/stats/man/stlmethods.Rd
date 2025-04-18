% File src/library/stats/man/stlmethods.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{stlmethods}
\alias{plot.stl}
\alias{summary.stl}
\title{Methods for \abbr{STL} Objects}
\description{
  Methods for objects of class \code{stl}, typically the result of
  \code{\link{stl}}.  The \code{plot} method does a multiple figure plot
  with some flexibility.

  There are also (non-visible) \code{print} and \code{summary} methods.
}
\usage{
\method{plot}{stl}(x, labels = colnames(X),
     set.pars = list(mar = c(0, 6, 0, 6), oma = c(6, 0, 4, 0),
                     tck = -0.01, mfrow = c(nplot, 1)),
     main = NULL, range.bars = TRUE, \dots,
     col.range = "light gray")

\method{summary}{stl}(object, digits = max(3L, getOption("digits") - 3L), \dots)
}
\arguments{
  \item{x, object}{\code{\link{stl}} object.}
  \item{labels}{character of length 4 giving the names of the component
    time-series.}
  \item{set.pars}{settings for \code{\link{par}(.)} when setting up the plot.}
  \item{main}{plot main title.}
  \item{range.bars}{logical indicating if each plot should have a bar at
    its right side which are of equal heights in user coordinates.}
  \item{col.range}{colour to be used for the range bars, if plotted.
    Note this appears after \code{\dots} and so cannot be abbreviated.}
  \item{\dots}{further arguments passed to or from other methods; for
    \code{summary()}, e.g., \code{zdigits} passed to the default method.}% format.summaryDefault
  \item{digits}{passed to the default \code{\link{summary}} method (for
    \code{time.series} and \code{weights} components).}
}
\seealso{
  \code{\link{plot.ts}} and \code{\link{stl}}, particularly for examples;
  \code{\link{summary.default}}.
}
\keyword{ts}

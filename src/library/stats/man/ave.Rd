% File src/library/stats/man/ave.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{ave}
\title{Apply Function Over Level Combinations of Factors}
\usage{
ave(x, \dots, FUN = mean)
}
\alias{ave}
\arguments{
  \item{x}{A vector containing values to be divided into groups.}
  \item{\dots}{Grouping variables, typically factors, all of the same
    length as \code{x}.}
  \item{FUN}{Function to apply for each factor level combination. 
  N.B. this argument must be named.  It cannot be matched by
  position, since it follows \code{\dots}}
}
\description{
  A specified function (defaulting to \code{mean}) is applied to subsets of
  \code{x[]}, where each subset consist of those observations with the same factor levels.
}
\value{
  A vector \code{y} of the same length as \code{x}.
  If \code{\dots} is missing, all elements of \code{y} are equal
  to \code{FUN(x)}. If \code{\dots} consists of one or more grouping variables 
  e.g. \code{g1, g2}, then \code{y[i]} is equal to \code{FUN(x[j])}, for all 
  \code{j} with \code{g1[j] == g1[i]} and \code{g2[j] == g2[i])}.
}

\seealso{\code{\link{mean}}, \code{\link{median}}, \code{\link{split}}.}
\examples{
require(graphics)

ave(1:3)  # no grouping -> grand mean

attach(warpbreaks)
ave(breaks, wool)
ave(breaks, tension)
ave(breaks, tension, FUN = function(x) mean(x, trim = 0.1))
plot(breaks, main =
     "ave( Warpbreaks )  for   wool  x  tension  combinations")
lines(ave(breaks, wool, tension              ), type = "s", col = "blue")
lines(ave(breaks, wool, tension, FUN = median), type = "s", col = "green")
legend(40, 70, c("mean", "median"), lty = 1,
      col = c("blue","green"), bg = "gray90")
detach()

# Sequence of occurrence of values
xts <- sample(LETTERS[1:3], 20, replace = TRUE)
xseq <- ave(xts, xts, FUN = seq_along) 
}
\keyword{univar}

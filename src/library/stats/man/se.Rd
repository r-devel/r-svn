\name{se}
\alias{se}
\alias{se.default}
\title{Extract Standard Errors}
\description{
Extract standard errors from a fitted model or another object with a
variance-covariance method.
}
\usage{
se(object, ...)

\method{se}{default}(object, ...)
}
\arguments{
  \item{object}{
    A fitted model object or another object for which a
    \code{\link{vcov}} method is available.
  }
  \item{\dots}{
    Additional arguments passed to methods and, for the default method,
    to \code{\link{vcov}}.
  }
}
\details{
The default method calculates standard errors as the square roots of
the diagonal elements of the variance-covariance matrix returned by
\code{vcov(object, ...)}.
}
\value{
A numeric vector containing the standard errors.
}
\seealso{
\code{\link{vcov}}, \code{\link{coef}}
}
\examples{
fit <- lm(mpg ~ wt + cyl, data = mtcars)
se(fit)

sqrt(diag(vcov(fit)))
}
\keyword{models}
\keyword{regression}

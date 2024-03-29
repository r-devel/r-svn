% File src/library/base/man/NA.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2021 R Core Team
% Distributed under GPL 2 or later

\name{NA}
\alias{NA}
\alias{NA_integer_}
\alias{NA_real_}
\alias{NA_complex_}
\alias{NA_character_}
\alias{is.na}
\alias{is.na.data.frame}
\alias{is.na<-}
\alias{is.na<-.default}
\alias{anyNA}
\alias{anyNA.data.frame}
\alias{anyMissing}% an alternative name (in Biobase and S+ as of ~2006)
\title{\sQuote{Not Available} / Missing Values}
\description{
  \code{NA} is a logical constant of length 1 which contains a missing
  value indicator.  \code{NA} can be coerced to any other vector
  type except raw.  There are also constants \code{NA_integer_},
  \code{NA_real_}, \code{NA_complex_} and \code{NA_character_} of the
  other atomic vector types which support missing values: all of these
  are \link{reserved} words in the \R language.

  The generic function \code{is.na} indicates which elements are missing.

  The generic function \code{is.na<-} sets elements to \code{NA}.

  The generic function \code{anyNA} implements \code{any(is.na(x))} in a
  possibly faster way (especially for atomic vectors).
}
\usage{
NA
is.na(x)
anyNA(x, recursive = FALSE)

\method{is.na}{data.frame}(x)

is.na(x) <- value
}
\arguments{
  \item{x}{an \R object to be tested: the default method for
    \code{is.na} and \code{anyNA} handle atomic vectors, lists,
    pairlists, and \code{NULL}.}
  \item{recursive}{logical: should \code{anyNA} be applied recursively
    to lists and pairlists?}
  \item{value}{a suitable index vector for use with \code{x}.}
}

\details{
  The \code{NA} of character type is distinct from the string
  \code{"NA"}.  Programmers who need to specify an explicit missing
  string should use \code{NA_character_} (rather than \code{"NA"}) or set
  elements to \code{NA} using \code{is.na<-}.

  \code{is.na} and \code{anyNA} are generic: you can write
  methods to handle specific classes of objects, see
  \link{InternalMethods}.

  Function \code{is.na<-} may provide a safer way to set missingness.
  It behaves differently for factors, for example.

  Numerical computations using \code{NA} will normally result in
  \code{NA}: a possible exception is where \code{\link{NaN}} is also
  involved, in which case either might result (which may depend on
  the \R platform).  However, this is not guaranteed and future CPUs
  and/or compilers may behave differently. Dynamic binary translation may
  also impact this behavior (with \I{valgrind}, computations using \code{NA}
  may result in \code{NaN} even when no \code{NaN} is involved).

  Logical computations treat \code{NA} as a missing \code{TRUE/FALSE}
  value, and so may return \code{TRUE} or \code{FALSE} if the expression
  does not depend on the \code{NA} operand.

  The default method for \code{anyNA} handles atomic vectors without a
  class and \code{NULL}.  It calls \code{any(is.na(x))} on objects with
  classes and for \code{recursive = FALSE}, on lists and pairlists.
}
\value{
  The default method for \code{is.na} applied to an atomic vector
  returns a logical vector of the same length as its argument \code{x},
  containing \code{TRUE} for those elements marked \code{NA} or, for
  numeric or complex vectors, \code{\link{NaN}}, and \code{FALSE}
  otherwise. (A complex value is regarded as \code{NA} if either its
  real or imaginary part is \code{NA} or \code{\link{NaN}}.)
  \code{dim}, \code{dimnames} and \code{names} attributes are copied to
  the result.

  The default methods also work for lists and pairlists:\cr
  For \code{is.na}, elementwise the result is false unless that element
  is a length-one atomic vector and the single element of that vector is
  regarded as \code{NA} or \code{NaN} (note that any \code{is.na}
  method for the class of the element is ignored).\cr
  \code{anyNA(recursive = FALSE)} works the same way as \code{is.na};
  \code{anyNA(recursive = TRUE)} applies \code{anyNA} (with method
  dispatch) to each element.

  The data frame method for \code{is.na} returns a logical matrix
  with the same dimensions as the data frame, and with dimnames taken
  from the row and column names of the data frame.

  \code{anyNA(NULL)} is false; \code{is.na(NULL)} is \code{logical(0)}
  (no longer warning since \R version 3.5.0).
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Chambers, J. M. (1998)
  \emph{Programming with Data.  A Guide to the S Language}.
  Springer.
}
\seealso{
  \code{\link{NaN}}, \code{\link{is.nan}}, etc.,
  and the utility function \code{\link{complete.cases}}.

  \code{\link{na.action}}, \code{\link{na.omit}}, \code{\link{na.fail}}
  on how methods can be tuned to deal with missing values.
}
\examples{
is.na(c(1, NA))        #> FALSE  TRUE
is.na(paste(c(1, NA))) #> FALSE FALSE

(xx <- c(0:4))
is.na(xx) <- c(2, 4)
xx                     #> 0 NA  2 NA  4
anyNA(xx) # TRUE

# Some logical operations do not return NA
c(TRUE, FALSE) & NA
c(TRUE, FALSE) | NA

\donttest{
## Measure speed difference in a favourable case:
## the difference depends on the platform, on most ca 3x.
x <- 1:10000; x[5000] <- NaN  # coerces x to be double
if(require("microbenchmark")) { # does not work reliably on all platforms
  print(microbenchmark(any(is.na(x)), anyNA(x)))
} else {
  nSim <- 2^13
  print(rbind(is.na = system.time(replicate(nSim, any(is.na(x)))),
              anyNA = system.time(replicate(nSim, anyNA(x)))))
}
}

## anyNA() can work recursively with list()s:
LL <- list(1:5, c(NA, 5:8), c("A","NA"), c("a", NA_character_))
L2 <- LL[c(1,3)]
sapply(LL, anyNA); c(anyNA(LL), anyNA(LL, TRUE))
sapply(L2, anyNA); c(anyNA(L2), anyNA(L2, TRUE))

## ... lists, and hence data frames, too:
dN <- dd <- USJudgeRatings; dN[3,6] <- NA
anyNA(dd) # FALSE
anyNA(dN) # TRUE
}
\keyword{NA}
\keyword{logic}
\keyword{manip}

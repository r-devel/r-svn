% File src/library/utils/man/head.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{head}
\title{Return the First or Last Parts of an Object}
\alias{head}
\alias{head.data.frame}
\alias{head.default}
\alias{head.function}
\alias{head.ftable}
\alias{head.matrix}
\alias{tail}
\alias{tail.data.frame}
\alias{tail.default}
\alias{tail.function}
\alias{tail.ftable}
\alias{tail.table}
\alias{tail.matrix}
\alias{tail.array}
\alias{.checkHT}
\description{
  Returns the first or last parts of a vector, matrix, array, table, data frame
  or function.  Since \code{head()} and \code{tail()} are generic
  functions, they have been extended to other classes, including
  \code{"\link[stats]{ts}"} from \pkg{stats}.
}
\usage{
head(x, \dots)
\method{head}{default}(x, n = 6L, \dots)
\method{head}{matrix}(x, n = 6L, \dots) # is exported as head.matrix()
## NB: The methods for 'data.frame' and 'array'  are identical to the 'matrix' one

\method{head}{ftable}(x, n = 6L, \dots)
\method{head}{function}(x, n = 6L, \dots)


tail(x, \dots)
\method{tail}{default}(x, n = 6L, keepnums = FALSE, addrownums, \dots)
\method{tail}{matrix}(x, n = 6L, keepnums = TRUE, addrownums, \dots) # exported as tail.matrix()
## NB: The methods for 'data.frame', 'array', and 'table'
##     are identical to the  'matrix'  one

\method{tail}{ftable}(x, n = 6L, keepnums = FALSE, addrownums, \dots)
\method{tail}{function}(x, n = 6L, \dots)

.checkHT(n, d) 
}
\arguments{
  \item{x}{an object}
  \item{n}{an integer vector of length up to \code{dim(x)} (or 1,
    for non-dimensioned objects).  A \code{logical} is silently coerced to
    integer.  Values specify the indices to be
    selected in the corresponding dimension (or along the length) of the
    object. A positive value of \code{n[i]} includes the first/last
    \code{n[i]} indices in that dimension, while a negative value
    excludes the last/first \code{abs(n[i])}, including all remaining
    indices. \code{NA} or non-specified values (when \code{length(n) <
      length(dim(x))}) select all indices in that dimension. Must
    contain at least one non-missing value.}
  \item{keepnums}{in each dimension, if no names in that dimension are
    present, create them using the indices included in that dimension.
    Ignored if \code{dim(x)} is \code{NULL} or its length 1.}
  \item{addrownums}{deprecated - \code{keepnums} should be used
    instead. Taken as the value of \code{keepnums} if it is explicitly
    set when \code{keepnums} is not.}
  \item{\dots}{arguments to be passed to or from other methods.}
  \item{d}{typically \code{dim(x)}, hence \code{NULL} or a (typically integer, short)
    vector.}

}
\details{
  For vector/array based objects, \code{head()} (\code{tail()}) returns
  a subset of the same dimensionality as \code{x}, usually of
  the same class. For historical reasons, by default they select the
  first (last) 6 indices in the first dimension ("rows") or along the
  length of a non-dimensioned vector, and the full extent (all indices)
  in any remaining dimensions. \code{head.matrix()} and
  \code{tail.matrix()} are exported.

  The default and array(/matrix) methods for \code{head()} and
  \code{tail()} are quite general. They will work as is for any class
  which has a \code{dim()} method, a \code{length()} method (only
  required if \code{dim()} returns \code{NULL}), and a \code{[} method
  (that accepts the \code{drop} argument and can subset in all
  dimensions in the dimensioned case).

  For functions, the lines of the deparsed function are returned as
  character strings.

  When \code{x} is an array(/matrix) of dimensionality two and more,
  \code{tail()} will add dimnames similar to how they would appear in a
  full printing of \code{x} for all dimensions \code{k} where
  \code{n[k]} is specified and non-missing and \code{dimnames(x)[[k]]}
  (or \code{dimnames(x)} itself) is \code{NULL}.  Specifically, the
  form of the added dimnames will vary for different dimensions as follows:
  \describe{
    \item{\code{k=1} (rows): }{\code{"[n,]"} (right justified with
      whitespace padding)}
    \item{\code{k=2} (columns): }{\code{"[,n]"} (with \emph{no} whitespace
      padding)}
    \item{\code{k>2} (higher dims): }{\code{"n"}, i.e., the indices as
      \emph{character} values}
  }
  Setting \code{keepnums = FALSE} suppresses this behaviour.

  As \code{\link{data.frame}} subsetting (\sQuote{indexing}) keeps
  \code{\link{attributes}}, so do the \code{head()} and \code{tail()}
  methods for data frames.

  The auxiliary function \code{.checkHT(d, n)} is useful in \code{head(x, n)} or
  \code{tail(x, n)} methods, checking validity of \code{d <- dim(x)} and \code{n}.
}
\value{
  An object (usually) like \code{x} but generally smaller.  Hence, for
  \code{\link{array}}s, the result corresponds to \code{x[.., drop=FALSE]}.
  For \code{\link{ftable}} objects \code{x}, a transformed \code{format(x)}.
}
\note{
  For array inputs the output of \code{tail} when \code{keepnums} is \code{TRUE},
  any dimnames vectors added for dimensions \code{>2} are the original
  numeric indices in that dimension \emph{as character vectors}.  This
  means that, e.g., for 3-dimensional array \code{arr},
  \code{tail(arr, c(2,2,-1))[ , , 2]} and
  \code{tail(arr, c(2,2,-1))[ , , "2"]} may both be valid but have
  completely different meanings.
}

\author{
  Patrick Burns, improved and corrected by R-Core. Negative argument
  added by Vincent Goulet.  Multi-dimension support added by Gabriel Becker.
}

\examples{
head(letters)
head(letters, n = -6L)

head(freeny.x, n = 10L)
head(freeny.y)

head(gait) # 3d array
head(gait, c(6L, 2L))
head(gait, c(6L, 2L, -1L))

tail(letters)
tail(letters, n = -6L)

tail(freeny.x)
## the bottom-right "corner" :
tail(freeny.x, n = c(4, 2))
tail(freeny.y)

tail(gait)
tail(gait, c(6L, 2L))
tail(gait, c(6L, 2L, -1L))

## gait without dimnames --> keepnums showing original row/col numbers
a3 <- gait ; dimnames(a3) <- NULL
tail(a3, c(6, 2, -1))# keepnums = TRUE is default here!
tail(a3, c(6, 2, -1),  keepnums = FALSE)

## data frame w/ a (non-standard) attribute:
treeS <- structure(trees, foo = "bar")
(n <- nrow(treeS))
stopifnot(exprs = { # attribute is kept
    identical(htS <- head(treeS), treeS[1:6, ])
    identical(attr(htS, "foo") , "bar")
    identical(tlS <- tail(treeS), treeS[(n-5):n, ])
    ## BUT if I use "useAttrib(.)", this is *not* ok, when n is of length 2:
    ## --- because [i,j]-indexing of data frames *also* drops "other" attributes ..
    identical(tail(treeS, 3:2), treeS[(n-2):n, 2:3] )
})

tail(library) # last lines of function

head(stats::ftable(Titanic))

## 1d-array (with named dim) :
a1 <- array(1:7, 7); names(dim(a1)) <- "O2"
stopifnot(exprs = {
  identical( tail(a1, 10), a1)
  identical( head(a1, 10), a1)
  identical( head(a1, 1), a1 [1 , drop=FALSE] ) # was a1[1] in R <= 3.6.x
  identical( tail(a1, 2), a1[6:7])
  identical( tail(a1, 1), a1 [7 , drop=FALSE] ) # was a1[7] in R <= 3.6.x
})
}
\keyword{manip}

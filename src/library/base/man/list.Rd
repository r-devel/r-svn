% File src/library/base/man/list.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
% Distributed under GPL 2 or later

\name{list}
\alias{list}
\alias{pairlist}
\alias{alist}
\alias{as.list}
\alias{as.list.default}
\alias{as.list.data.frame}
\alias{as.list.environment}
\alias{as.list.factor}
\alias{as.list.function}
\alias{as.pairlist}
\alias{is.list}
\alias{is.pairlist}
\title{Lists -- Generic and Dotted Pairs}
\description{
  Functions to construct, coerce and check for both kinds of \R lists.
}
\usage{
list(\dots)
pairlist(\dots)

as.list(x, \dots)
\method{as.list}{environment}(x, all.names = FALSE, sorted = FALSE, \dots)
as.pairlist(x)

is.list(x)
is.pairlist(x)

alist(\dots)
}
\arguments{
  \item{\dots}{objects, possibly named.}
  \item{x}{object to be coerced or tested.}
  \item{all.names}{a logical indicating whether to copy all values or
    (default) only those whose names do not begin with a dot.}
  \item{sorted}{a logical indicating whether the \code{\link{names}} of
    the resulting list should be sorted (increasingly).  Note that this
    is somewhat costly, but may be useful for comparison of environments.}
}
\details{
  Almost all lists in \R internally are \emph{Generic Vectors}, whereas
  traditional \emph{dotted pair} lists (as in LISP) remain available but
  rarely seen by users (except as \code{\link{formals}} of functions).

  The arguments to \code{list} or \code{pairlist} are of the form
  \code{value} or \code{tag = value}.  The functions return a list or
  dotted pair list composed of its arguments with each value either
  tagged or untagged, depending on how the argument was specified.

  \code{alist} handles its arguments as if they described function
  arguments.  So the values are not evaluated, and tagged arguments with
  no value are allowed whereas \code{list} simply ignores them.
  \code{alist} is most often used in conjunction with \code{\link{formals}}.

  \code{as.list} attempts to coerce its argument to a list.  For
  functions, this returns the concatenation of the list of formal
  arguments and the function body.  For expressions, the list of
  constituent elements is returned.  \code{as.list} is generic, and as
  the default method calls \code{\link{as.vector}(mode = "list")} for a
  non-list, methods for \code{as.vector} may be invoked.  \code{as.list}
  turns a factor into a list of one-element factors, keeping
  \code{\link{names}}.  Other attributes may
  be dropped unless the argument already is a list or expression.  (This
  is inconsistent with functions such as \code{\link{as.character}}
  which always drop attributes, and is for efficiency since lists can be
  expensive to copy.)

  \code{is.list} returns \code{TRUE} if and only if its argument
  is a \code{list} \emph{or} a \code{pairlist} of \code{length} \eqn{> 0}.
  \code{is.pairlist} returns \code{TRUE} if and only if the argument
  is a pairlist (or \code{NULL} which \emph{is} a pairlist, see below).

  The \code{"\link{environment}"} method for \code{as.list} copies the
  name-value pairs (for names not beginning with a dot) from an
  environment to a named list.  The user can request that all named
  objects are copied.  Unless \code{sorted = TRUE}, the list is in no
  particular order (the order
  depends on the order of creation of objects and whether the
  environment is hashed).  No enclosing environments are searched.
  (Objects copied are duplicated so this can be an expensive operation.)
  Note that there is an inverse operation, the
  \code{\link{as.environment}()} method for list objects.

  An empty pairlist, \code{pairlist()} is the same as
  \code{\link{NULL}}.  This is different from \code{list()}: some but
  not all operations will promote an empty pairlist to an empty list.

  \code{as.pairlist} is implemented as \code{\link{as.vector}(x,
  "pairlist")}, and hence will dispatch methods for the generic function
  \code{as.vector}.  Lists are copied element-by-element into a pairlist
  and the names of the list used as tags for the pairlist: the return
  value for other types of argument is undocumented.

  \code{list}, \code{is.list} and \code{is.pairlist} are
  \link{primitive} functions.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{vector}("list", length)} for creation of a list with empty
  components; \code{\link{c}}, for concatenation; \code{\link{formals}}.
  \code{\link{unlist}} is an approximate inverse to \code{as.list()}.

  \sQuote{\link{plotmath}} for the use of \code{list} in plot annotation.
}
\examples{
require(graphics)

# create a plotting structure
pts <- list(x = cars[,1], y = cars[,2])
plot(pts)

is.pairlist(.Options)  # a user-level pairlist

## "pre-allocate" an empty list of length 5
vector("list", 5)

# Argument lists
f <- function() x
# Note the specification of a "..." argument:
formals(f) <- al <- alist(x = , y = 2+3, \dots = )
f
al

## environment->list coercion

e1 <- new.env()
e1$a <- 10
e1$b <- 20
as.list(e1)
}
\keyword{list}
\keyword{manip}

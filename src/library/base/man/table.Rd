% File src/library/base/man/table.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2021 R Core Team
% Distributed under GPL 2 or later

\name{table}
\title{Cross Tabulation and Table Creation}
\alias{table}
\alias{summary.table}
\alias{print.summary.table}%% {print.table} is in ./print.Rd
\alias{as.data.frame.table}
\alias{as.table}
\alias{as.table.default}
\alias{is.table}
\alias{[.table}

\concept{counts}
\concept{frequencies}
\concept{occurrences}
\concept{contingency table}

\description{
  \code{table} uses cross-classifying factors to build a contingency
  table of the counts at each combination of factor levels.
}
\usage{
table(\dots,
      exclude = if (useNA == "no") c(NA, NaN),
      useNA = c("no", "ifany", "always"),
      dnn = list.names(\dots), deparse.level = 1)

as.table(x, \dots)
is.table(x)

\method{as.data.frame}{table}(x, row.names = NULL, \dots,
              responseName = "Freq", stringsAsFactors = TRUE,
              sep = "", base = list(LETTERS))
}
\arguments{
  \item{\dots}{one or more objects which can be interpreted as factors
    (including numbers or character strings), or a \code{\link{list}} (such
    as a data frame) whose
    components can be so interpreted.  (For \code{as.table}, arguments
    passed to specific methods; for \code{as.data.frame}, unused.)}
  \item{exclude}{levels to remove for all factors in \code{\dots}.  If
    it does not contain \code{\link{NA}} and \code{useNA} is not
    specified, it implies \code{useNA = "ifany"}.  See
    \sQuote{Details} for its interpretation for non-factor arguments.}
  \item{useNA}{whether to include \code{NA} values in the table.
    See \sQuote{Details}.  Can be abbreviated.}
  \item{dnn}{the names to be given to the dimensions in the result (the
    \emph{dimnames names}).}
  \item{deparse.level}{controls how the default \code{dnn} is
    constructed.  See \sQuote{Details}.}
  \item{x}{an arbitrary \R object, or an object inheriting from class
    \code{"table"} for the \code{as.data.frame} method. Note that
    \code{as.data.frame.table(x, *)} may be called explicitly for
    non-table \code{x} for \dQuote{reshaping} \code{\link{array}}s.}
  \item{row.names}{a character vector giving the row names for the data
    frame.}
  \item{responseName}{the name to be used for the column of table
    entries, usually counts.}
  \item{stringsAsFactors}{logical: should the classifying factors be
    returned as factors (the default) or character vectors?}
  \item{sep, base}{passed to \code{\link{provideDimnames}}.}
}
\details{
  If the argument \code{dnn} is not supplied, the internal function
  \code{list.names} is called to compute the \sQuote{\I{dimname} names} as
  follows:
  If \code{\dots} is one \code{list} with its own \code{\link{names}()},
  these \code{names} are used.  Otherwise, if the
  arguments in \code{\dots} are named, those names are used.  For the
  remaining arguments, \code{deparse.level = 0} gives an empty name,
  \code{deparse.level = 1} uses the supplied argument if it is a symbol,
  and \code{deparse.level = 2} will deparse the argument.

  Only when \code{exclude} is specified (i.e., not by default) and
  non-empty, will \code{table} potentially drop levels of factor
  arguments.

  \code{useNA} controls if the table includes counts of \code{NA}
  values: the allowed values correspond to never (\code{"no"}), only if the count is
  positive (\code{"ifany"}) and even for zero counts (\code{"always"}).
  Note the somewhat \dQuote{pathological} case of two different kinds of
  \code{NA}s which are treated differently, depending on both
  \code{useNA} and \code{exclude}, see \code{d.patho} in the
  \sQuote{Examples:} below.

  Both \code{exclude} and \code{useNA} operate on an \dQuote{all or none}
  basis.  If you want to control the dimensions of a multiway table
  separately, modify each argument using \code{\link{factor}} or
  \code{\link{addNA}}.

  Non-factor arguments \code{a} are coerced via \code{factor(a,
    exclude=exclude)}.  Since \R 3.4.0, care is taken \emph{not} to
  count the excluded values (where they were included in the \code{NA}
  count, previously).

  The \code{summary} method for class \code{"table"} (used for objects
  created by \code{table} or \code{\link{xtabs}}) which gives basic
  information and performs a chi-squared test for independence of
  factors (note that the function \code{\link{chisq.test}} currently
  only handles 2-d tables).
}
\value{
  \code{table()} returns a \emph{contingency table}, an object of
  class \code{"table"}, an array of integer values.
  Note that unlike S the result is always an \code{\link{array}}, a 1D
  array if one factor is given.

  \code{as.table} and \code{is.table} coerce to and test for contingency
  table, respectively.

  The \code{as.data.frame} method for objects inheriting from class
  \code{"table"} can be used to convert the array-based representation
  of a contingency table to a data frame containing the classifying
  factors and the corresponding entries (the latter as component
  named by \code{responseName}).  This is the inverse of \code{\link{xtabs}}.
}
\seealso{
  \code{\link{tabulate}} is the underlying function and allows finer
  control.

  Use \code{\link{ftable}} for printing (and more) of
  multidimensional tables.  \code{\link{margin.table}},
  \code{\link{prop.table}}, \code{\link{addmargins}}.

  \code{\link{addNA}} for constructing factors with \code{\link{NA}} as
  a level.

  \code{\link{xtabs}} for cross tabulation of data frames with a
  formula interface.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\examples{
require(stats) # for rpois and xtabs
## Simple frequency distribution
table(rpois(100, 5))
## Check the design:
with(warpbreaks, table(wool, tension))
table(state.division, state.region)

# simple two-way contingency table
with(airquality, table(cut(Temp, quantile(Temp)), Month))

a <- letters[1:3]
table(a, sample(a))                    # dnn is c("a", "")
table(a, sample(a), dnn = NULL)        # dimnames() have no names
table(a, sample(a), deparse.level = 0) # dnn is c("", "")
table(a, sample(a), deparse.level = 2) # dnn is c("a", "sample(a)")

## xtabs() <-> as.data.frame.table() :
UCBAdmissions ## already a contingency table
DF <- as.data.frame(UCBAdmissions)
class(tab <- xtabs(Freq ~ ., DF)) # xtabs & table
## tab *is* "the same" as the original table:
all(tab == UCBAdmissions)
all.equal(dimnames(tab), dimnames(UCBAdmissions))

a <- rep(c(NA, 1/0:3), 10)
table(a)                 # does not report NA's
table(a, exclude = NULL) # reports NA's
b <- factor(rep(c("A","B","C"), 10))
table(b)
table(b, exclude = "B")
d <- factor(rep(c("A","B","C"), 10), levels = c("A","B","C","D","E"))
table(d, exclude = "B")
print(table(b, d), zero.print = ".")

## NA counting:
is.na(d) <- 3:4
d. <- addNA(d)
d.[1:7]
table(d.) # ", exclude = NULL" is not needed
## i.e., if you want to count the NA's of 'd', use
table(d, useNA = "ifany")

## "pathological" case:
d.patho <- addNA(c(1,NA,1:2,1:3))[-7]; is.na(d.patho) <- 3:4
d.patho
## just 3 consecutive NA's ? --- well, have *two* kinds of NAs here :
as.integer(d.patho) # 1 4 NA NA 1 2
##
## In R >= 3.4.0, table() allows to differentiate:
table(d.patho)                   # counts the "unusual" NA
table(d.patho, useNA = "ifany")  # counts all three
table(d.patho, exclude = NULL)   #  (ditto)
table(d.patho, exclude = NA)     # counts none

## Two-way tables with NA counts. The 3rd variant is absurd, but shows
## something that cannot be done using exclude or useNA.
with(airquality,
   table(OzHi = Ozone > 80, Month, useNA = "ifany"))
with(airquality,
   table(OzHi = Ozone > 80, Month, useNA = "always"))
with(airquality,
   table(OzHi = Ozone > 80, addNA(Month)))
}
\keyword{category}

% File src/library/utils/man/debugger.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{debugger}
\title{\I{Post-Mortem} Debugging}
\alias{debugger}
\alias{dump.frames}
\alias{limitedLabels}
\description{
  Functions to dump the evaluation environments (frames) and to examine
  dumped frames.
}
\usage{
dump.frames(dumpto = "last.dump", to.file = FALSE,
            include.GlobalEnv = FALSE)
debugger(dump = last.dump)

limitedLabels(value, maxwidth = getOption("width") - 5L)
}
\arguments{
  \item{dumpto}{a character string. The name of the object or file to
    dump to.}
  \item{to.file}{logical.  Should the dump be to an \R object or to a
    file?}
  \item{include.GlobalEnv}{logical indicating if a \emph{copy} of the
    \code{\link{.GlobalEnv}} environment should be included in addition
    to the \code{\link{sys.frames}()}.  Will be particularly useful when
    used in a batch job.}
  \item{dump}{an \R dump object created by \code{dump.frames}.}
  %% limitedLabels():
  \item{value}{a \code{\link{list}} of \code{\link{call}}s to be formatted,
    e.g., for user menus.}
  \item{maxwidth}{optional length to which to trim the result of \code{limitedLabels()};
    values smaller than 40 or larger than 1000 are winsorized.}
}
\details{
  To use post-mortem debugging, set the option \code{error} to be a call
  to \code{dump.frames}.  By default this dumps to an \R object
  \code{last.dump} in the workspace, but it can be set to dump to a
  file (a dump of the object produced by a call to \code{\link{save}}).
  The dumped object contains the call stack, the active environments and
  the last error message as returned by \code{\link{geterrmessage}}.

  When dumping to file, \code{dumpto} gives the name of the dumped
  object and the file name has \file{.rda} appended.

  A dump object of class \code{"dump.frames"} can be examined by calling
  \code{debugger}.  This will give the error message and a list of
  environments from which to select repeatedly.  When an environment is
  selected, it is copied and the \code{\link{browser}} called from
  within the copy.  Note that not all the information in the original
  frame will be available, e.g.\sspace{}promises which have not yet been
  evaluated and the contents of any \code{\dots} argument.

  If \code{dump.frames} is installed as the error handler, execution
  will continue even in non-interactive sessions.  See the examples for
  how to dump and then quit.

  \code{limitedLabels(v)} takes a \code{\link{list}} of calls whose
  elements may have a \code{srcref} attribute and returns a vector that
  pastes a formatted version of those attributes onto the formatted version
  of the elements, all finally \code{\link{strtrim}()}med to \code{maxwidth}.
}
\value{
  Invisible \code{NULL}.
}
\note{
  Functions such as \code{\link{sys.parent}} and
  \code{\link{environment}} applied to closures will not work correctly
  inside \code{debugger}.

  If the error occurred when computing the default value of a formal
  argument the debugger will report \dQuote{recursive default argument
  reference} when trying to examine that environment.

  Of course post-mortem debugging will not work if \R is too damaged to
  produce and save the dump, for example if it has run out of workspace.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{browser}} for the actions available at the \code{Browse}
  prompt.

  \code{\link{options}} for setting \code{error} options;
  \code{\link{recover}} is an interactive debugger working similarly to
  \code{debugger} but directly after the error occurs.
}
\examples{
op <- options(error = dump.frames)

f <- function() {
    g <- function() stop("test dump.frames")
    g()
}
f()   # will generate a dump in object "last.dump"
str(last.dump)

options(op)  # reset (error = NULL)

\dontrun{
### Example of debugger() interaction with 'last.dump'

> debugger()
Available environments had calls:
1: f()
2: g()
3: stop("test dump.frames")

Enter an environment number, or 0 to exit
Selection: 1
Browsing in the environment with call:
   f()
Called from: debugger.look(ind)
Browse[1]> ls()
[1] "g"
Browse[1]> g
function() stop("test dump.frames")
<environment: 759818>
Browse[1]>
Available environments had calls:
1: f()
2: g()
3: stop("test dump.frames")

Enter an environment number, or 0 to exit
Selection: 0
}

\dontrun{
## A possible setting for non-interactive sessions
options(error = quote({dump.frames(to.file = TRUE); q(status = 1)}))
f()   # will generate a dump on file "last.dump.rda" and exit
## then, in another R session
load("last.dump.rda")
debugger()
}}
\keyword{utilities}
\keyword{error}

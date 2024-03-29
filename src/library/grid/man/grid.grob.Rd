% File src/library/grid/man/grid.grob.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2020 R Core Team
% Distributed under GPL 2 or later

\name{grid.grob}
\title{Create Grid Graphical Objects, aka "Grob"s}
\alias{grob}
\alias{gTree}
\alias{grobTree}
\alias{childNames}
\alias{gList}
\alias{is.grob}
\description{
  Creating grid graphical objects, short (\dQuote{grob}s).

  \code{grob()} and \code{gTree()} are the basic creators,
  \code{grobTree()} and \code{gList()} take several grobs to build a new
  one.
}
\usage{
## Grob Creation:

grob (..., name = NULL, gp = NULL, vp = NULL, cl = NULL)
gTree(..., name = NULL, gp = NULL, vp = NULL, children = NULL,
      childrenvp = NULL, cl = NULL)
grobTree(..., name = NULL, gp = NULL, vp = NULL,
         childrenvp = NULL, cl = NULL)
gList(...)

## Grob Properties:
childNames(gTree)
is.grob(x)

}
\arguments{
  \item{...}{For \code{grob} and \code{gTree}, the
    named slots describing important features of
    the graphical object.  For \code{gList} and \code{grobTree},
    a series of grob objects. }
  \item{name}{a character identifier for the grob.  Used to find the
    grob on the display list and/or as a child of another grob. }
  \item{children}{a \code{"gList"} object. }
  \item{childrenvp}{a \code{\link{viewport}} object (or \code{NULL}).}
  \item{gp}{A \code{"gpar"} object, typically the output
    from a call to the function \code{\link{gpar}}.  This is basically
    a list of graphical parameter settings.}
  \item{vp}{a \code{\link{viewport}} object (or \code{NULL}).}
  \item{cl}{string giving the class attribute for the new class.}
  \item{gTree}{a \code{"gTree"} object.}
  \item{x}{ An \R object. }
}
\details{
  These functions can be used to create a basic \code{"grob"},
  \code{"gTree"}, or \code{"gList"} object, or a new class derived from
  one of these.

  A grid graphical object (\dQuote{grob}) is a description of a graphical
  item.  These basic classes provide default behaviour for
  validating, drawing,
  and modifying graphical objects.  Both \code{grob()} and
  \code{gTree()} call the function \code{\link{validDetails}} to check
  that the object returned is internally coherent.

  A \code{"gTree"} can have other grobs as children;  when a \I{gTree} is drawn, it
  draws all of its children.  Before drawing its children, a \I{gTree}
  pushes its \code{childrenvp} slot and then navigates back up (calls
  \code{\link{upViewport}}) so that the children can specify their location
  within the \code{childrenvp} via a \code{\link{vpPath}}.

  Grob names need not be unique in general, but all children of a
  \I{gTree} must have different names.
  A grob name can be any string, though it is not
  advisable to use the \code{\link{gPath}} separator (currently
  \code{::}) in grob names.

  The function \code{childNames} returns the names of the grobs
  which are children of a \I{gTree}.

  All grid primitives (\code{\link{grid.lines}}, \code{grid.rect}, ...)
  and some higher-level grid components (e.g., \code{\link{grid.xaxis}}
  and \code{grid.yaxis}) are derived from these classes.

  \code{grobTree} is just a convenient wrapper for \code{gTree}
  when the only components of the \I{gTree} are grobs (so all
  unnamed arguments become children of the \I{gTree}).

  The \code{grid.grob} function is defunct.
}
\value{
  An \R object of class \code{"grob"}, a \bold{gr}aphical \bold{ob}ject.
}
\author{Paul Murrell}
\seealso{
  \code{\link{grid.draw}},
  \code{\link{grid.edit}},
  \code{\link{grid.get}}.
}
\keyword{dplot}

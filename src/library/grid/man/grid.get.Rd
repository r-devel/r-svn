% File src/library/grid/man/grid.get.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{grid.get}
\alias{grid.get}
\alias{grid.gget}
\alias{getGrob}
\title{Get a Grid Graphical Object}
\description{
  Retrieve a grob or a descendant of a grob.
}
\usage{
grid.get(gPath, strict = FALSE, grep = FALSE, global = FALSE,
         allDevices = FALSE)

grid.gget(\dots, grep = TRUE, global = TRUE)

getGrob(gTree, gPath, strict = FALSE, grep = FALSE, global = FALSE)
}
\arguments{
  \item{gTree}{A \I{gTree} object.}
  \item{gPath}{ A \I{gPath} object. For \code{grid.get} this
    specifies a grob on the display list.  For \code{getGrob} this
    specifies a descendant of the specified \I{gTree}. }
  \item{strict}{ A boolean indicating whether the \I{gPath} must be
    matched exactly. }
  \item{grep}{A boolean indicating whether the \code{gPath} should
    be treated as a regular expression.  Values are recycled across
    elements of the \code{gPath} (e.g., \code{c(TRUE, FALSE)} means
    that every odd element of the \code{gPath} will be treated as
    a regular expression).
  }
  \item{global}{ A boolean indicating whether the function should affect
    just the first match of the \code{gPath}, or whether all matches
    should be affected.
  }
  \item{allDevices}{ A boolean indicating whether all open devices
    should
    be searched for matches, or just the current device.
    NOT YET IMPLEMENTED.
  }
  \item{\dots}{ Arguments that are passed to \code{grid.get}. }
}
\details{
  \code{grid.gget} (\code{g} for global) is just a convenience wrapper for
  \code{grid.get} with different defaults.
}
\value{
  A grob object.
}
\author{Paul Murrell}
\seealso{
  \code{\link{grob}},
  \code{\link{addGrob}}, \code{\link{removeGrob}}.
}
\examples{
grid.xaxis(name="xa")
grid.get("xa")
grid.get(gPath("xa", "ticks"))

grid.draw(gTree(name="gt", children=gList(xaxisGrob(name="axis"))))
grid.get(gPath("gt", "axis", "ticks"))
}
\keyword{dplot}

% File src/library/grid/man/gPath.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{gPath}
\alias{gPath}
\title{ Concatenate Grob Names }
\description{
  This function can be used to generate a grob path for use
  in \code{grid.edit} and friends.

  A grob path is a list of nested grob names.
}
\usage{
gPath(...)
}
\arguments{
  \item{\dots}{ Character values which are grob names. }
}
\details{
  Grob names must only be unique amongst grobs which
  share the same parent in a \I{gTree}.

  This function can be used to generate a specification for
  a grob that includes the grob{}'s parent's name
  (and the name of its parent and so on).

  For interactive use, it is possible to directly specify
  a path, but it is strongly recommended that this function
  is used otherwise in case the path separator is changed
  in future versions of grid.
}
\value{
  A \code{ gPath } object.
}
\seealso{
  \code{\link{grob}},
  \code{\link{editGrob}},
  \code{\link{addGrob}},
  \code{\link{removeGrob}},
  \code{\link{getGrob}},
  \code{\link{setGrob}}
}
\examples{
gPath("g1", "g2")
}
\keyword{ dplot }

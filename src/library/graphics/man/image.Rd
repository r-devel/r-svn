% File src/library/graphics/man/image.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{image}
\alias{image}
\alias{image.default}
\title{Display a Color Image}
\usage{
image(x, \dots)

\method{image}{default}(x, y, z, zlim, xlim, ylim,
      col = hcl.colors(12, "YlOrRd", rev = TRUE),
      add = FALSE, xaxs = "i", yaxs = "i", xlab, ylab,
      breaks, oldstyle = FALSE, useRaster, \dots)
}
\arguments{
  \item{x, y}{locations of grid lines at which the values in \code{z} are
    measured.  These must be finite, non-missing and in (strictly)
    ascending order.  By default, equally
    spaced values from 0 to 1 are used.  If \code{x} is a \code{list},
    its components \code{x$x} and \code{x$y} are used for \code{x}
    and \code{y}, respectively. If the list has component \code{z} this
    is used for \code{z}.}
  \item{z}{a numeric or logical matrix containing the values to be plotted
    (\code{NA}s are allowed).  Note that \code{x} can be used instead
    of \code{z} for convenience.}
  \item{zlim}{the minimum and maximum \code{z} values for which colors
    should be plotted, defaulting to the range of the finite values of
    \code{z}. Each of the given colors will be used to color an
    equispaced interval of this range. The \emph{midpoints} of the
    intervals cover the range, so that values just outside the range
    will be plotted.}
  \item{xlim, ylim}{ranges for the plotted \code{x} and \code{y} values,
    defaulting to the ranges of \code{x} and \code{y}.}
  \item{col}{a list of colors such as that generated by
    \code{\link{hcl.colors}}, \code{\link{gray.colors}} or similar
    functions.}
  \item{add}{logical; if \code{TRUE}, add to current plot (and disregard
    the following four arguments).  This is rarely useful because
    \code{image} \sQuote{paints} over existing graphics.}
  \item{xaxs, yaxs}{style of x and y axis.  The default \code{"i"} is
    appropriate for images.  See \code{\link{par}}.}
  \item{xlab, ylab}{each a character string giving the labels for the x and
    y axis.  Default to the \sQuote{call names} of \code{x} or \code{y}, or to
    \code{""} if these were unspecified.}
  \item{breaks}{a set of finite numeric breakpoints for the colours:
    must have one more breakpoint than colour and be in increasing
    order.  Unsorted vectors will be sorted, with a warning.}
  \item{oldstyle}{logical. If true the midpoints of the colour intervals
    are equally spaced, and \code{zlim[1]} and \code{zlim[2]} were taken
    to be midpoints.  The default is to have colour intervals of equal
    lengths between the limits.}
  \item{useRaster}{logical; if \code{TRUE} a bitmap raster is used to
    plot the image instead of polygons. The grid must be regular in that
    case, otherwise an error is raised.   For the behaviour when this is
    not specified, see \sQuote{Details}.}
  \item{\dots}{\link{graphical parameters} for \code{\link{plot}} may also be
    passed as arguments to this function, as can the plot aspect ratio
    \code{asp} and \code{axes} (see \code{\link{plot.window}}).}
}
\description{
  Creates a grid of colored or gray-scale rectangles with colors
  corresponding to the values in \code{z}.  This can be used to display
  three-dimensional or spatial data aka \emph{images}.
  This is a generic function.

  \emph{NOTE:} the grid is drawn as a set of rectangles by default;
  see the \code{useRaster} argument to draw the grid as a raster image.

  The function \code{\link{hcl.colors}} provides a broad range of sequential
  color palettes that are suitable for displaying ordered data, with
  \code{n} giving the number of colors desired.
}
\details{
  The length of \code{x} should be equal to the \code{nrow(z)+1} or
  \code{nrow(z)}.  In the first case \code{x} specifies the boundaries
  between the cells: in the second case \code{x} specifies the midpoints
  of the cells.  Similar reasoning applies to \code{y}.  It probably
  only makes sense to specify the midpoints of an equally-spaced
  grid.  If you specify just one row or column and a length-one \code{x}
  or \code{y}, the whole user area in the corresponding direction is
  filled. For logarithmic \code{x} or \code{y} axes the boundaries between
  cells must be specified.

  Rectangles corresponding to missing values are not plotted (and so are
  transparent and (unless \code{add = TRUE}) the default background
  painted in \code{par("bg")} will show through and if that is
  transparent, the canvas colour will be seen).

  If \code{breaks} is specified then \code{zlim} is unused and the
  algorithm used follows \code{\link{cut}}, so intervals are closed on
  the right and open on the left except for the lowest interval which is
  closed at both ends.

  The axes (where plotted) make use of the classes of \code{xlim} and
  \code{ylim} (and hence by default the classes of \code{x} and
  \code{y}): this will mean that for example dates are labelled as
  such.

  Notice that \code{image} interprets the \code{z} matrix as a table of
  \code{f(x[i], y[j])} values, so that the x axis corresponds to row
  number and the y axis to column number, with column 1 at the bottom,
  i.e.\sspace{}a 90 degree counter-clockwise rotation of the conventional
  printed layout of a matrix.

  Images for large \code{z} on a regular grid are rendered more
  efficiently with \code{useRaster = TRUE} and can prevent rare
  anti-aliasing artifacts, but may not be supported by all graphics
  devices.  Some devices (such as \code{postscript} and \code{X11(type =
  "Xlib")}) which do not support semi-transparent colours may emit
  missing values as white rather than transparent, and there may be
  limitations on the size of a raster image.  (Problems with the
  rendering of raster images have been reported by users of
  \code{windows()} devices under Remote Desktop, at least under its
  default settings.)

  The graphics files in PDF and PostScript can be much smaller under
  this option.

  If \code{useRaster} is not specified, raster images are used when the
  \code{\link{getOption}("preferRaster")} is true, the grid is regular
  and either \code{\link{dev.capabilities}("rasterImage")$rasterImage}
  is \code{"yes"} or it is \code{"non-missing"} and there are no missing
  values.
}

\note{
  Originally based on a function by \I{Thomas Lumley}.
}

\seealso{
  \code{\link{filled.contour}} or \code{\link{heatmap}} which can
  look nicer (but are less modular),
  \code{\link{contour}};
  The \CRANpkg{lattice} equivalent of \code{image} is
  \code{\link[lattice]{levelplot}}.

  \code{\link{hcl.colors}}, \code{\link{gray.colors}},
  \code{\link{hcl}}, \code{\link{hsv}}, \code{\link{par}}.

  \code{\link{dev.capabilities}} to see if \code{useRaster = TRUE} is
  supported on the current device.
}

\examples{
require("grDevices") # for colours
x <- y <- seq(-4*pi, 4*pi, length.out = 27)
r <- sqrt(outer(x^2, y^2, `+`))
image(z = z <- cos(r^2)*exp(-r/6), col = gray.colors(33))
image(z, axes = FALSE, main = "Math can be beautiful ...",
      xlab = expression(cos(r^2) * e^{-r/6}))
contour(z, add = TRUE, drawlabels = FALSE)

# Visualize as matrix.  Need to transpose matrix and then flip it horizontally:
tf <- function(m) t(m)[, nrow(m):1]
imageM <- function(m, grid = max(dim(m)) <= 25, asp = (nrow(m)-1)/(ncol(m)-1), ...) {
    image(tf(m), asp=asp, axes = FALSE, ...)
    mAxis <- function(side, at, ...) # using 'j'
        axis(side, at=at, labels=as.character(j+1L), col="gray", col.axis=1, ...)
    n <- ncol(m); n1 <- n-1L; j <- 0L:n1; mAxis(1, at= j/n1)
    if(grid) abline(v = (0:n - .5)/n1, col="gray77", lty="dotted")
    n <- nrow(m); n1 <- n-1L; j <- 0L:n1; mAxis(2, at=1-j/n1, las=1)
    if(grid) abline(h = (0:n - .5)/n1, col="gray77", lty="dotted")
}
(m <- outer(1:5, 1:14))
imageM(m, main = "image(<5 x 14 matrix>)   with rows and columns")
imageM(volcano)

# A prettier display of the volcano
x <- 10*(1:nrow(volcano))
y <- 10*(1:ncol(volcano))
image(x, y, volcano, col = hcl.colors(100, "terrain"), axes = FALSE)
contour(x, y, volcano, levels = seq(90, 200, by = 5),
        add = TRUE, col = "brown")
axis(1, at = seq(100, 800, by = 100))
axis(2, at = seq(100, 600, by = 100))
box()
title(main = "Maunga Whau Volcano", font.main = 4)
}
\keyword{hplot}
\keyword{aplot}

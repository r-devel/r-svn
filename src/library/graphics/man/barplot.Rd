% File src/library/graphics/man/barplot.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{barplot}
\alias{barplot}
\alias{barplot.default}
\alias{barplot.formula}
\title{Bar Plots}
\description{
  Creates a bar plot with vertical or horizontal bars.
}
\usage{
barplot(height, \dots)

\method{barplot}{default}(height, width = 1, space = NULL,
        names.arg = NULL, legend.text = NULL, beside = FALSE,
        horiz = FALSE, density = NULL, angle = 45,
        col = NULL, border = par("fg"),
        main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
        xlim = NULL, ylim = NULL, xpd = TRUE, log = "",
        axes = TRUE, axisnames = TRUE,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
        inside = TRUE, plot = TRUE, axis.lty = 0, offset = 0,
        add = FALSE, ann = !add && par("ann"), args.legend = NULL, \dots)

\method{barplot}{formula}(formula, data, subset, na.action,
        horiz = FALSE, xlab = NULL, ylab = NULL, \dots)
}
\arguments{
  \item{height}{either a vector or matrix of values describing the
    bars which make up the plot.  If \code{height} is a vector, the
    plot consists of a sequence of rectangular bars with heights
    given by the values in the vector.  If \code{height} is a matrix
    and \code{beside} is \code{FALSE} then each bar of the plot
    corresponds to a column of \code{height}, with the values in the
    column giving the heights of stacked sub-bars making up the
    bar.  If \code{height} is a matrix and \code{beside} is
    \code{TRUE}, then the values in each column are juxtaposed
    rather than stacked.}
  \item{width}{optional vector of bar widths. Re-cycled to length the
    number of bars drawn.  Specifying a single value will have no
    visible effect unless \code{xlim} is specified.}
  \item{space}{the amount of space (as a fraction of the average bar
    width) left before each bar.  May be given as a single number or
    one number per bar.  If \code{height} is a matrix and
    \code{beside} is \code{TRUE}, \code{space} may be specified by
    two numbers, where the first is the space between bars in the
    same group, and the second the space between the groups.  If not
    given explicitly, it defaults to \code{c(0,1)} if \code{height}
    is a matrix and \code{beside} is \code{TRUE}, and to 0.2
    otherwise.}
  \item{names.arg}{a vector of names to be plotted below each bar or
    group of bars.  If this argument is omitted, then the names are
    taken from the \code{names} attribute of \code{height} if this
    is a vector, or the column names if it is a matrix.}
  \item{legend.text}{a vector of text used to construct a legend for
    the plot, or a logical indicating whether a legend should be
    included.  This is only useful when \code{height} is a matrix.
    In that case given legend labels should correspond to the rows of
    \code{height}; if \code{legend.text} is true, the row names of
    \code{height} will be used as labels if they are non-null.}
  \item{beside}{a logical value.  If \code{FALSE}, the columns of
    \code{height} are portrayed as stacked bars, and if \code{TRUE}
    the columns are portrayed as juxtaposed bars.}
  \item{horiz}{a logical value.  If \code{FALSE}, the bars are drawn
    vertically with the first bar to the left.  If \code{TRUE}, the
    bars are drawn horizontally with the first at the bottom.}
  \item{density}{a vector giving the density of shading lines, in
    lines per inch, for the bars or bar components.
    The default value of \code{NULL} means that no shading lines
    are drawn. Non-positive values of \code{density} also inhibit the
    drawing of shading lines.}
  \item{angle}{the slope of shading lines, given as an angle in
    degrees (counter-clockwise), for the bars or bar components.}
  \item{col}{a vector of colors for the bars or bar components.
    By default, \code{"grey"} is used if \code{height} is a vector, and a
    gamma-corrected grey palette if \code{height} is a matrix;
    see \code{\link{grey.colors}}.}
  \item{border}{the color to be used for the border of the bars.
    Use \code{border = NA} to omit borders.  If there are shading
    lines, \code{border = TRUE} means use the same colour for
    the border as for the shading lines.}
  \item{main,sub}{main title and subtitle for the plot.}
  \item{xlab}{a label for the x axis.}
  \item{ylab}{a label for the y axis.}
  \item{xlim}{limits for the x axis.}
  \item{ylim}{limits for the y axis.}
  \item{xpd}{logical. Should bars be allowed to go outside region?}
  \item{log}{string specifying if axis scales should be logarithmic; see
    \code{\link{plot.default}}.}
  \item{axes}{logical.  If \code{TRUE}, a vertical (or horizontal, if
    \code{horiz} is true) axis is drawn.}
  \item{axisnames}{logical.  If \code{TRUE}, and if there are
    \code{names.arg} (see above), the
    other axis is drawn (with \code{lty = 0}) and labeled.}
  \item{cex.axis}{expansion factor for numeric axis labels (see
    \code{\link{par}('cex')}).}
  \item{cex.names}{expansion factor for axis names (bar labels).}
  \item{inside}{logical.  If \code{TRUE}, the lines which divide
    adjacent (non-stacked!) bars will be drawn.  Only applies when
    \code{space = 0} (which it partly is when \code{beside = TRUE}).}
%MM: we still have "NotYetUsed" -- the above describes S-plus 6.1 (not
% precisely documented!) behavior!
  \item{plot}{logical.  If \code{FALSE}, nothing is plotted.}
  \item{axis.lty}{the graphics parameter \code{lty} (see
    \code{\link{par}('lty')}) applied to the axis
    and tick marks of the categorical (default horizontal) axis.  Note
    that by default the axis is suppressed.}
  \item{offset}{a vector indicating how much the bars should be shifted
    relative to the x axis.}
  \item{add}{logical specifying if bars should be added to an already
    existing plot; defaults to \code{FALSE}.}
  \item{ann}{logical specifying if the default annotation (\code{main},
    \code{sub}, \code{xlab}, \code{ylab}) should appear on the plot, see
    \code{\link{title}}.}
  \item{args.legend}{list of additional arguments to pass to
    \code{\link{legend}()}; names of the list are used as argument
    names.  Only used if \code{legend.text} is supplied.}
  \item{formula}{a formula where the \code{y} variables are numeric data
    to plot against the categorical \code{x} variables.  The formula
    can have one of three forms:
    \preformatted{
      y ~ x
      y ~ x1 + x2
      cbind(y1, y2) ~ x
    }
    (see the examples).}
  \item{data}{a data frame (or list) from which the variables in formula
    should be taken.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used.}
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{\link{NA}} values.  The default is to ignore missing
    values in the given variables.}
  \item{\dots}{arguments to be passed to/from other methods.  For the
    default method these can include further arguments (such as
    \code{axes}, \code{asp} and \code{main}) and
    \link{graphical parameters} (see \code{\link{par}}) which are passed to
    \code{\link{plot.window}()}, \code{\link{title}()} and
    \code{\link{axis}}.}
}
\value{
  A numeric vector (or matrix, when \code{beside = TRUE}), say
  \code{mp}, giving the coordinates of \emph{all} the bar midpoints
  drawn, useful for adding to the graph.

  If \code{beside} is true, use \code{colMeans(mp)} for the
  midpoints of each \emph{group} of bars, see example.
}
\author{R Core, with a contribution by Arni Magnusson.}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Murrell, P. (2005) \emph{R Graphics}. Chapman & Hall/CRC Press.
}
\seealso{
  \code{\link{plot}(\dots, type = "h")}, \code{\link{dotchart}};
  \code{\link{hist}} for bars of a \emph{continuous} variable.
  \code{\link{mosaicplot}()}, more sophisticated to visualize
  \emph{several} categorical variables.
}
\examples{
# Formula method
barplot(GNP ~ Year, data = longley)
barplot(cbind(Employed, Unemployed) ~ Year, data = longley)

## 3rd form of formula - 2 categories :
op <- par(mfrow = 2:1, mgp = c(3,1,0)/2, mar = .1+c(3,3:1))
summary(d.Titanic <- as.data.frame(Titanic))
barplot(Freq ~ Class + Survived, data = d.Titanic,
        subset = Age == "Adult" & Sex == "Male",
        main = "barplot(Freq ~ Class + Survived, *)", ylab = "# {passengers}", legend.text = TRUE)
# Corresponding table :
(xt <- xtabs(Freq ~ Survived + Class + Sex, d.Titanic, subset = Age=="Adult"))
# Alternatively, a mosaic plot :
mosaicplot(xt[,,"Male"], main = "mosaicplot(Freq ~ Class + Survived, *)", color=TRUE)
par(op)


# Default method
require(grDevices) # for colours
tN <- table(Ni <- stats::rpois(100, lambda = 5))
## barplot() returns x-locations:
r <- barplot(tN, col = rainbow(20))
lines(r, tN, type = "h", col = 2, lwd = 7) # type "h" plot *is* 'bar'plot

## draw axis separately  using x-locations from barplot():
barplot(tN, space = 1.5, axisnames = FALSE,
        sub = "barplot(..., space= 1.5, axisnames = FALSE)"
       ) |> axis(1, at = _, labels = paste("# :", tN), cex.axis = 3/4)

barplot(VADeaths, plot = FALSE)
barplot(VADeaths, plot = FALSE, beside = TRUE)

mp <- barplot(VADeaths) # default
tot <- colMeans(VADeaths)
text(mp, tot + 3, format(tot), xpd = TRUE, col = "blue")
barplot(VADeaths, beside = TRUE,
        col = c("lightblue", "mistyrose", "lightcyan",
                "lavender", "cornsilk"),
        legend.text = rownames(VADeaths), ylim = c(0, 100))
title(main = "Death Rates in Virginia", font.main = 4)

hh <- t(VADeaths)[, 5:1]
mybarcol <- "gray20"
mp <- barplot(hh, beside = TRUE,
        col = c("lightblue", "mistyrose",
                "lightcyan", "lavender"),
        legend.text = colnames(VADeaths), ylim = c(0,100),
        main = "Death Rates in Virginia", font.main = 4,
        sub = "Faked upper 2*sigma error bars", col.sub = mybarcol,
        cex.names = 1.5)
segments(mp, hh, mp, hh + 2*sqrt(1000*hh/100), col = mybarcol, lwd = 1.5)
stopifnot(dim(mp) == dim(hh))  # corresponding matrices
mtext(side = 1, at = colMeans(mp), line = -2,
      text = paste("Mean", formatC(colMeans(hh))), col = "red")

# Bar shading example
barplot(VADeaths, angle = 15+10*1:5, density = 20, col = "black",
        legend.text = rownames(VADeaths))
title(main = list("Death Rates in Virginia", font = 4))

# Border color
barplot(VADeaths, border = "dark blue") % lwd = 2 << not passed
%notyet barplot(VADeaths, inside = FALSE, main = "barplot(*, inside = FALSE)")

# Log scales (not much sense here)
barplot(tN, col = heat.colors(12), log = "y")
barplot(tN, col = gray.colors(20), log = "xy")

# Legend location
barplot(height = cbind(x = c(465, 91) / 465 * 100,
                       y = c(840, 200) / 840 * 100,
                       z = c(37, 17) / 37 * 100),
        beside = FALSE,
        width = c(465, 840, 37),
        col = c(1, 2),
        legend.text = c("A", "B"),
        args.legend = list(x = "topleft"))
}
\keyword{hplot}

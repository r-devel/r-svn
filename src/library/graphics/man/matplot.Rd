% File src/library/graphics/man/matplot.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{matplot}
\alias{matplot}
\alias{matpoints}
\alias{matlines}
\title{Plot Columns of Matrices}
\description{
  Plot the columns of one matrix against the columns of another (which
  often is just a vector treated as 1-column matrix).
}
\usage{
matplot(x, y, type = "p", lty = 1:5, lwd = 1, lend = par("lend"),
        pch = NULL,
        col = 1:6, cex = NULL, bg = NA,
        xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL,
        log = "", \dots, add = FALSE, verbose = getOption("verbose"))

matpoints(x, y, type = "p", lty = 1:5, lwd = 1, pch = NULL,
          col = 1:6, \dots)

matlines (x, y, type = "l", lty = 1:5, lwd = 1, pch = NULL,
          col = 1:6, \dots)
}
\arguments{
  \item{x,y}{vectors or matrices of data for plotting.  The number of
    rows should match.  If one of them are missing, the other is taken
    as \code{y} and an \code{x} vector of \code{1:n} is used.
    Missing values (\code{NA}s) are allowed. Typically,
    \code{\link{class}(.)}es of \code{x} and \code{y} such as
    \code{"\link{Date}"} are preserved.}
  \item{type}{character string (length 1 vector) or vector of 1-character
    strings indicating the type of plot for each
    column of \code{y}, see \code{\link{plot}} for all possible
    \code{type}s.  The first character of \code{type}
    defines the first plot, the second character the second, etc.
    Characters in \code{type} are cycled through; e.g., \code{"pl"}
    alternately plots points and lines.}
  \item{lty,lwd,lend}{vector of line types, widths, and end styles.
    The first element is for the first column, the second element for
    the second column, etc., even if lines are not plotted for all
    columns. Line types will be used cyclically until all plots are
    drawn.}
  \item{pch}{character string or vector of 1-characters or integers for
    plotting characters, see \code{\link{points}} for details.
    The first character is the plotting-character for the first plot,
    the second for the second, etc.  The default is the digits (1
    through 9, 0) then the lowercase and uppercase letters.
  }
  \item{col}{vector of colors.  Colors are used cyclically.}
  \item{cex}{vector of character expansion sizes, used cyclically.
    This works as a multiple of \code{\link{par}("cex")}.  \code{NULL} is
    equivalent to \code{1.0}. }
  \item{bg}{vector of background (fill) colors for the open plot
    symbols given by \code{pch = 21:25} as in \code{\link{points}}.  The
    default \code{NA} corresponds to the one of the underlying function
    \code{\link{plot.xy}}.}
  \item{xlab, ylab}{titles for x and y axes, as in \code{\link{plot}}.}
  \item{xlim, ylim}{ranges of x and y axes, as in \code{\link{plot}}.}
  \item{log, \dots}{Graphical parameters (see \code{\link{par}}) and any further
    arguments of \code{plot}, typically \code{\link{plot.default}}, may also be
    supplied as arguments to this function; even \code{panel.first} etc
    now work.  Hence, the high-level
    graphics control arguments described under \code{\link{par}} and the
    arguments to \code{\link{title}} may be supplied to this function.}
  \item{add}{logical.  If \code{TRUE}, plots are added to current one,
    using \code{\link{points}} and \code{\link{lines}}.}
  \item{verbose}{logical.  If \code{TRUE}, write one line of what is
    done.}
}
\section{Side Effects}{Function \code{matplot} generates a new plot;
  \code{matpoints} and \code{matlines} add to the current one.}
\details{
  \code{matplot(x,y, ..)} is basically a wrapper for
  \enumerate{
    \item calling (the generic function) \code{\link{plot}(x[,1], y[,1], ..)}
    for the first columns (only if \code{add = TRUE}).
    \item calling (the generic) \code{\link{lines}(x[,j], y[,j], ..)} for
    subsequent columns.
  }
  Care is taken to keep the \code{\link{class}(.)} of
  \code{x} and \code{y}, such that the corresponding \code{plot()} and
  \code{lines()} \emph{methods} will be called.

  Points involving missing values are not plotted.

  The first column of \code{x} is plotted against the first column of
  \code{y}, the second column of \code{x} against the second column of
  \code{y}, etc.  If one matrix has fewer columns, plotting will cycle
  back through the columns again.  (In particular, either \code{x} or
  \code{y} may be a vector, against which all columns of the other
  argument will be plotted.)

  The first element of \code{col, cex, lty, lwd} is used to plot the axes
  as well as the first line.

  Because plotting symbols are drawn with lines and because these
  functions may be changing the line style, you should probably specify
  \code{lty = 1} when using plotting symbols.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{plot}}, \code{\link{points}}, \code{\link{lines}},
  \code{\link{matrix}}, \code{\link{par}}.
}
\examples{
require(grDevices)
matplot((-4:5)^2, main = "Quadratic") # almost identical to plot(*)
sines <- outer(1:20, 1:4, function(x, y) sin(x / 20 * pi * y))
matplot(sines, pch = 1:4, type = "o", col = rainbow(ncol(sines)))
matplot(sines, type = "b", pch = 21:23, col = 2:5, bg = 2:5,
        main = "matplot(...., pch = 21:23, bg = 2:5)")

x <- 0:50/50
matplot(x, outer(x, 1:8, function(x, k) sin(k*pi * x)),
        ylim = c(-2,2), type = "plobcsSh",
        main= "matplot(,type = \"plobcsSh\" )")
## pch & type =  vector of 1-chars :
matplot(x, outer(x, 1:4, function(x, k) sin(k*pi * x)),
        pch = letters[1:4], type = c("b","p","o"))

lends <- c("round","butt","square")
matplot(matrix(1:12, 4), type="c", lty=1, lwd=10, lend=lends)
text(cbind(2.5, 2*c(1,3,5)-.4), lends, col= 1:3, cex = 1.5)

table(iris$Species) # is data.frame with 'Species' factor
iS <- iris$Species == "setosa"
iV <- iris$Species == "versicolor"
op <- par(bg = "bisque")
matplot(c(1, 8), c(0, 4.5), type =  "n", xlab = "Length", ylab = "Width",
        main = "Petal and Sepal Dimensions in Iris Blossoms")
matpoints(iris[iS,c(1,3)], iris[iS,c(2,4)], pch = "sS", col = c(2,4))
matpoints(iris[iV,c(1,3)], iris[iV,c(2,4)], pch = "vV", col = c(2,4))
legend(1, 4, c("    Setosa Petals", "    Setosa Sepals",
               "Versicolor Petals", "Versicolor Sepals"),
       pch = "sSvV", col = rep(c(2,4), 2))

matplot(iris3[, "Petal L.",], iris3[, "Petal W.",], pch = "SCV",
        col = rainbow(3, start = 0.8, end = 0.1),
        sub = paste(c("S", "C", "V"), dimnames(iris3)[[3]],
                    sep = "=", collapse= ",  "),
        main = "Fisher's Iris Data")
par(op)

## 'x' a "Date" vector :
nd <- length(dv <- seq(as.Date("1959-02-21"), by = "weeks", length.out = 100))
mSC <- cbind(I=1, sin=sin(pi*(1:nd)/8), cos=cos(pi*(1:nd)/8))
matplot(dv, mSC, type = "b", main = "matplot(<Date>, y)")

## 'x' a "POSIXct" date-time vector :
ct <- seq(c(ISOdate(2000,3,20)), by = "15 mins", length.out = 100)
matplot(ct, mSC, type = "b", main = "matplot(<POSIXct>, y)")
## or the same with even more axis flexibility:
matplot(ct, mSC, type = "b", main = "matplot(<POSIXct>, y)", xaxt="n")
Axis(ct, side=1, at = ct[1+4*(0:24)])

iS <- 1:3 # indices of subset
matplot(gait[, iS, 1], gait[, iS, 2], pch = "123", type = "b",
        col = rainbow(3, start = 0.8, end = 0.1),
        sub = paste(iS, dimnames(gait)[[2]][iS],
                    sep = "=", collapse= ",  "),
        xlab = "Hip angle", ylab = "Knee angle",
        main = "Hip and knee angle while walking")

## Also works for data frame columns:
matplot(iris[1:50,1:4])
}
\keyword{hplot}
\keyword{aplot}
\keyword{array}

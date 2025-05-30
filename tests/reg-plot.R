#### Regression tests for GRAPHICS & PLOTS -- requiring strict PDF equality

pdf("reg-plot.pdf", paper="a4r", encoding ="ISOLatin1.enc", compress = FALSE,
    useDingbats = TRUE)

## since we supply the font metrics, the results depend only on
## the encoding used: Windows is different from Unix by default.

options(warn = 1) # print as they occur

plot(0) # this should remain constant
str(par(c("usr","xaxp","yaxp")))
stopifnot(all.equal(
    par(c("usr","xaxp","yaxp"))
   ,
    list(usr = c(0.568, 1.432, -1.08, 1.08),
         xaxp = c(0.6, 1.4, 4), yaxp = c(-1, 1, 4))))


### Test for centring of chars.  All the chars which are plotted should
### be centred, and there should be no warnings about
### font metrics unknown for character `?'

par(pty="s")
plot(c(-1,16), c(-1,16), type="n", xlab="", ylab="", xaxs="i", yaxs="i")
title("Centred chars in default char set (ISO Latin1)")
grid(17, 17, lty=1)
known <- c(32:126, 160:255)

for(i in known) {
    x <- i %% 16
    y <- i %/% 16
    points(x, y, pch=-i)
}

par(pty="m")

## PR 816 (label sizes in dotchart)

### Prior to 1.2.2, the label sizes were unaffected by cex.

dotchart(VADeaths, main = "Death Rates in Virginia - 1940", cex = 0.5)
dotchart(VADeaths, main = "Death Rates in Virginia - 1940", cex = 1.5)

## killed by 0 prior to 1.4.0 and in 1.4.1:
t1 <- ts(0:100)
## only warnings about values <= 0
plot(t1, log = "y")
plot(cbind(t1, 10*t1, t1 - 4), log="y", plot.type = "single")
stopifnot(par("usr")[4] > 3) # log10: ylim[2] = 1000


## This one needs to be looked at.
## lty = "blank" killed the fill colour too.
plot(1:10, type="n")
polygon(c(1, 3, 3, 1), c(1, 1, 3, 3), col="yellow", border="red", lty="blank")
rect(6, 6, 10, 10,  col="blue", border="red", lty="blank")
## in 1.5.0 all omit the fill colours.
with(trees, symbols(Height, Volume, circles=Girth/24, inches=FALSE,
                    lty="blank", bg="blue"))
## in 1.5.0 ignored the lty.

## axis() and par(mgp < 0)
lt <-"31" # in R
x <- seq(-2,3, len=1001)
op <- par(tck= +0.02, mgp = -c(3,2,0))
plot(x, x^2 - 1.2, xaxt = "n", xlab="", type ='l', col = 2,
     main = "mgp < 0: all ticks and labels inside `frame'")
x <- -2:3
lines(x, x^2 - 1.2, type ="h", col = 3, lwd=3)
axis(1, pos = 0, at=-1:1, lty = lt, col=4)## col & lty work only from R 1.6
par(op)
axis(1, pos = 0, at=c(-2,2,3), lty = lt, col=4)
mtext(side=1,"note the x-ticks on the other side of the bars")

## plot.table(): explicit xlab and ylab for non-1D
plot(UCBAdmissions)# default x- and y-lab
plot(UCBAdmissions, xlab = "x label", ylab = "YY")# wrong in 1.5.1
##   axis suppression
plot(tt <- table(c(rep(0,7), rep(1,4), rep(5, 3))), axes = FALSE)
plot(tt, xaxt = "n")
## wrong till (incl.) 1.6.x

## legend with call
lo <- legend(2,2, substitute(hat(theta) == that, list(that= pi)))
stopifnot(length(lo$text$x) == 1)
## length() was 3 till 1.7.x

plot(ecdf(c(1:4,8,12)), ylab = "ECDF", main=NULL)
## ylab didn't work till 1.8.0

plot(1:10, pch = NA) # gave error till 1.9.0
points(1:3, pch=c("o",NA,"x"))# used "N"
try(points(4, pch=c(NA,FALSE)))# still give an error

## 'lwd' should transfer to plot symbols
legend(1,10, c("A","bcd"), lwd = 2:3, pch= 21:22, pt.bg="skyblue",
       col = 2:3, bg = "thistle")
## (gave an error for 2 days in "2.0.0 unstable")

x <- 2^seq(1,1001, length=20)
plot(x, x^0.9, type="l", log="xy")
## gave error 'Infinite axis extents [GEPretty(1.87013e-12,inf,5)]' for R 2.0.1

plot(as.Date("2001/1/1") + 12*(1:9), 1:9)
## used bad 'xlab/ylab' in some versions of R 2.2.0(unstable)

## dotchart() restoring par()
Opar <- par(no.readonly=TRUE) ; dotchart(1:4, cex= 0.7)
Npar <- par(no.readonly=TRUE)
ii <- c(37, 50:51, 58:59, 63)
stopifnot(identical(names(Opar)[ii],
                    c("mai","pin","plt","usr","xaxp","yaxp")),
          identical(Opar[-ii], Npar[-ii]))
## did not correctly restore par("mar") up to (incl) R 2.4.0

## plot.function()     [n=11, ... : since we store and diff PS file !]
plot(cos,       xlim=c(-5,5), n=11, axes=FALSE); abline(v=0)
## did *not* plot for negative x up to R 2.5.1
plot(sin, -2,3, xlim=c(-5,5), n=11, axes=FALSE, xlab="")# plot from -2
axis(1, at=c(-2,3), tcl=-1); axis(1, at=c(-5,5))
## (from,to) & xlim  should work simultaneously

plot(cos, -7,7, n=11, axes=FALSE)
## gave wrong ylab in R 2.6.0
plot(cos, -7,7, ylab = "Cosine  cos(x)", n=11, axes=FALSE)
## partial matching of 'ylab'; mapping  [0,1] (not [-7.7]):
## margins chosen to avoid rounding error showing to 2dp.
op <- par(mar=c(5,4.123,4,2)+0.1)
plot(gamma, yla = expression(Gamma(x)), n=11, yaxt="n")
par(op)

## plot.ts(x, y) could get the labels wrong in R <= 2.6.0:
x <- ts(1:5);x1 <- lag(x, 2); plot(x1, x, axes=FALSE)

# adding a curve in log scale :
curve(5*exp(-x), 0.1, 100, n = 3, log="x", ylab="", axes=FALSE)
curve(5*exp(-x), add=TRUE, n = 3, col=2,lwd=3)
## should fully overplot; wrong default xlim in 2.6.1
## (and *slightly* wrong up to 2.6.0)

## Axis() calls via plot()  {[xy]axt to keep *.ps small}
x <- as.Date("2008-04-22 09:45") + (i <- c(0,4))
plot(x,    xaxt="n")# not ok in 2.6.2, nor 2.7.0
plot(x, i, yaxt="n")# ok in 2.6.2  and 2.7.0
plot(i, x, xaxt="n")# ok in 2.6.2 and not in 2.7.0

## table methods should be bypassed:
dotchart(table(infert$education))
## failed in 2.12.[12]

## cex as "..."  in "high level" function
hc <- hclust(dst <- dist(c(1:2, 5)), method="ave")
plot(hc, cex = 2, axes=FALSE, ann=FALSE)
## cex was not used in 3.0.[01]

## axis.Date() and axis.POSIXct() with reversed 'xlim'
toD <- as.Date("2016-08-19"); dates <- c(toD - 10, toD)
plot(dates, 1:2, xlim = rev(dates),
     ann=FALSE, yaxt="n", frame.plot=FALSE)
## failed to label the dates in R <= 3.3.1

## axis.Date() with various data types:
x <- seq(as.Date("2022-01-20"), as.Date("2023-03-21"), by = "days")
plot(data.frame(x, y = 1), xaxt = "n")
axis.Date(1)
axis.Date(3, at = "2022-04-01")
axis.Date(3, at = as.Date("2022-07-01"))
axis.Date(3, at = as.POSIXct(as.Date("2022-10-01")))
axis.Date(3, at = as.POSIXlt(as.Date("2023-01-01")))
axis.Date(3, at = as.integer(as.Date("2023-04-01")))
## automatically extends the format:
axis.Date(1, at = "2022-02-15", tck = -0.05, mgp = c(3,2,0))

## axis.POSIXct() with various data types (2 minutes)
x <- as.POSIXct("2022-10-01") + c(0, 60, 120)
plot(data.frame(x, y = 1), xaxt="n")
axis.POSIXct(1)
axis.POSIXct(3, at = "2022-10-01 00:01")
axis.POSIXct(3, at = as.Date("2022-10-01"))
axis.POSIXct(3, at = as.POSIXct("2022-10-01 00:01:30"))
axis.POSIXct(3, at = as.POSIXlt("2022-10-01 00:02"))
axis.POSIXct(3, at = as.numeric(as.POSIXct("2022-10-01 00:00:30")))
## automatically extends format (here: subseconds):
axis.POSIXct(3, at = "2022-10-01 00:00:30.25", mgp = c(3,2,0))

## axis.POSIXct: a few days, extending the format
days <- seq(as.Date("2022-10-01"), as.Date("2022-12-21"), by="days")
x <- as.POSIXct(as.character(days))
plot(data.frame(x, y = 1), xaxt="n")
axis.POSIXct(1, x)
axis.POSIXct(1, x, at = as.Date("2022-10-12"), mgp = c(3,2,0), tck = -0.04)
axis.POSIXct(3, x, at = as.POSIXct("2022-10-15"))
axis.POSIXct(3, x, at = as.POSIXlt("2022-10-15"), mgp = c(3,2,0))
axis.POSIXct(1, x, at = "2022-11-01 23:00", mgp = c(3,2,0), tck = -0.04)
axis.POSIXct(3, x, at = "2022-11-01 06:00")
axis.POSIXct(3, x, at = as.numeric(as.POSIXct("2022-12-01")))

## axis() -- labels only written when there's room
plot2 <- function(at, wait=FALSE) {
    plot1 <- function(x,y) plot(x,y, type="n", xlab="", ylab="", tck=0, frame.plot=FALSE)
    plot1( at, at); axis(3, at= at, tck=0)
    plot1(-at,-at); axis(3, at=-at, tck=0)
    if(wait) { mtext("Click here to advance!", line=-2, cex=1.5, col=2); locator(1) }
}
at <- c(7:15, 2*(8:15), 5*(7:15), 10*(8:15))
op <- par(mfrow=2:1, mgp = c(1.5, 0.6, 0), mar = .1+c(2,2,2,1),
          lab = c(20,20,7), las = 1) # las=1: all horizontal => y-axis perpendicular
interAct <- dev.interactive()

if(interAct) { xMar <- c(1,3)/2  ; nP <- 10
} else {       xMar <- c(1,3)*3/2; nP <-  3 }
## Now increasing margins ==> decreasing plot area ==> shrinking plot
## (*is* device dependent [here have "a4r"-pdf]) :
for(n in 1:nP) {
    par(mar = par("mar") + xMar)
    plot2(at, n < nP && interAct)
}
par(op)


##----- pairs() using verInd & horInd ----------
mpairs <- function(..., p=3, npp=2, main) { # p=4, npp=7 was Chris Andrews' example
    x <- matrix((1:(p*npp))/npp, ncol=p, dimnames=list(NULL, paste0("x", 1:p)))
    ## -> x \in [1/npp,.., p]  <==> ceiling(x) \in {1,2,...,p}
    ## the panel function
    mP <- function(x,y, ...) { ## really made for the 'x = ...' above
        j <- ceiling(mean(y)) # in 1:p
        points(x,y, col=ceiling(mean(x)), pch= 3*j, cex=((p-1.5)*j+1)/p, ...)
        g <- 1:(p-1); abline(h=g, v=g, lty=3, col="gray33")
    }
    if(missing(main))
        main <- if(...length()) sub("mpairs", '', deparse(sys.call()))
    lim <- range(x)
    k <- max(2, npp-2) ## bug in R(?): par(lab = c(1,1,7)) giving nonsense!
    op <- par(lab = c(k,k,7), oma=c(1,0,0,0)); on.exit(par(op))
    pairs(x, panel=mP, xlim=lim, ylim=lim, main=main, cex.main=1, ...)
}

mpairs() # 4x4 matrix of scatterplots
if(interAct) dev.set(if(length(dev.list()) < 2) dev.new() else dev.prev())
mpairs(horInd=1:2,verInd=  1:3 ) # 3x2 matrix: upper left [WRONG in R <= 3.5.0]
mpairs(horInd=3:2)               # 2x4: middle rows swapped
mpairs(           verInd=c(3,1)) # 4x2: cols 3,1
##
## now all with  'row1attop=FALSE'=======
if(interAct) dev.set(dev.prev())
mpairs(row1attop=FALSE) # 4x4 matrix of scatterplots -- perfect in R <= 3.5.0
if(interAct) dev.set(dev.next())
## a version of those above, with 'row1attop = FALSE'
mpairs(horInd=2:1,verInd=3:1   , row1attop=FALSE) # 3x2: *swapped* upper left
mpairs(horInd=c(3,1)           , row1attop=FALSE) # 2x3: swapped outer rows
mpairs(           verInd=c(3,2), row1attop=FALSE) # 3x2: cols 3,2


## axis() when in subnormal range (x is subnormal if 0 < x < .Machine$double.xmin):
(.min.exp.subnormal <- with(.Machine, double.min.exp + double.ulp.digits)) # -1074
(ry <- range(y74 <- 2^(.min.exp.subnormal + 0:50))) # all subnormal
plotNchk <- function(y) {
    plot(y, log = "y", xlab="", ylab="", xaxt="n") # (as small pdf as possible)
    ry <- range(y)
    xp <- par("yaxp")
    ## and indeed yaxp do cover the range(y) :
    stopifnot(xp[1] <= ry[1], ry[2] <= xp[2], xp[3] == 1)
    invisible(xp)
}
if(interactive()) # not regularly, where pdf is stored
plotNchk(y74) # gives 3 warnings; 1. from pretty(): "very small range"
plotNchk(y74[1:8]) # 3 warnings *and* no error anymore
plotNchk(y74[1:2]) #    (ditto)


## dotchart(*, pch=., groups=*) -- PR#16953
## dotchart(*, ylab=.) for groups;
g <- rep(1:3, each=2)
dotchart(VADeaths[1:2, 1:3], color=g, pch=g,
         ylab = "Grouping:  {Urbanity . Gender} x Age",
         xaxt="n", frame.plot=FALSE)
## now pch and colors match groups;
## ylab placement; group (row) labels show again


## non-integer mgp[3] -- PR#18194
par(mar = c(5, 5, 5, 5))
plot.new()
plot.window(xlim = c(0, 5), ylim = 0:1)
if(dev.interactive()) {
    box(lty = 3)
    ## 'axis' puts line and labels in right place when 'mgp[3]' is integer
    mgp_bottom <- c(4, 2.5, 1) # 'mgp[1]' is arbitrary
    axis(1, at=c(1,4), mgp = mgp_bottom)
    mtext(c("labels", "line"), side = 1, line = mgp_bottom[2:3])
}
## Now, 'axis' puts line & labels in right place also when 'mgp[3]' is noninteger:
mgp_top <- c(4, 2.5, 0.9)
axis(3, at=c(1,4), mgp = mgp_top)
mtext(c("labels are here", "line"), line = mgp_top[2:3])
## They were one line too high in R <= 4.1.1

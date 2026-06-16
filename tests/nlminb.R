f <- function(x) sum( log(diff(x)^2+.01) + (x[1]-1)^2 )
opt <- nlminb(rep(0, 10), f, lower=-1, upper=3)
xhat <- rep(1, 10)
ok <- abs( opt$objective - f(xhat) ) < 1e-4 ## Must be TRUE
stopifnot(ok)

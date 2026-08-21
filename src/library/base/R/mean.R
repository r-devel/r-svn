#  File src/library/base/R/mean.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2022 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

mean <- function(x, ...) UseMethod("mean")

mean.bytes <- function(x, ...)
{
    ## The numeric kinds are numbers, so a mean of them means something;
    ## the opaque kind is a byte string and falls through to the default,
    ## which warns and returns NA as it does for a factor.  is.numeric()
    ## is FALSE for every kind (the factor precedent), which is what
    ## sends mean() down that path for all of them without this.
    ## R's mean is a double whatever it is handed, and as.numeric() is
    ## correctly rounded, so nothing is given up by going through it --
    ## beyond the warning a value above 2^53 earns anywhere else.
    if(bytesKind(x) == "opaque") mean.default(x, ...)
    else mean.default(as.numeric(x), ...)
}

mean.default <- function(x, trim = 0, na.rm = FALSE, ...)
{
    if(!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
        warning("argument is not numeric or logical: returning NA")
        return(NA_real_)
    }
    if (isTRUE(na.rm))
	x <- x[!is.na(x)]
    if(!is.numeric(trim) || length(trim) != 1L)
        stop("'trim' must be numeric of length one")
    n <- length(x)
    if(trim > 0 && n) {
	if(is.complex(x))
	    stop("trimmed means are not defined for complex data")
        if(anyNA(x)) return(NA_real_)
	if(trim >= 0.5) return(stats::median(x, na.rm=FALSE))
	lo <- floor(n*trim)+1
	hi <- n+1-lo
	x <- sort.int(x, partial = unique(c(lo, hi)))[lo:hi]
    }
    .Internal(mean(x))
}

#  File src/library/stats/R/fft.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

fft <- function(z, inverse=FALSE) .Call(C_fft, z, inverse)

mvfft <- function(z, inverse=FALSE) .Call(C_mvfft, z, inverse)

nextn <- function(n, factors=c(2,3,5)) .Call(C_nextn, n, factors)

convolve <- function(x, y, conj=TRUE, type=c("circular","open","filter"))
{
    type <- match.arg(type)
    nx <- length(x)
    ny <- length(y)
    Real <- is.numeric(x) && is.numeric(y)
    ## switch(type, circular = ..., )
    if(type == "circular") {
        if(ny != nx)
            stop("length mismatch in convolution")
    }
    else { ## "open" or "filter": Pad with zeros
        if (type == "filter" && ny>nx) return(numeric())
        n1 <- ny - 1
        x <- c(rep.int(0, n1), x)
        n <- length(y <- c(y, rep.int(0, nx - 1)))# n = nx+ny-1
    }
    x <- fft(fft(x)* (if(conj)Conj(fft(y)) else fft(y)), inverse=TRUE)
    if(type == "filter")
        (if(Real) Re(x) else x)[ny:nx]/n
    else
        (if(Real) Re(x) else x)/n
}


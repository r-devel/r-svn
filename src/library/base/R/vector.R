#  File src/library/base/R/vector.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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

vector <- function(mode = "logical", length = 0L) .Internal(vector(mode, length))
logical <- function(length = 0L) .Internal(vector("logical", length))
character <- function(length = 0L) .Internal(vector("character", length))
integer <- function(length = 0L) .Internal(vector("integer", length))
numeric <- double <-
    function(length = 0L) .Internal(vector("double", length))

complex <- function(length.out = 0L,
		    real = numeric(), imaginary = numeric(),
		    modulus = 1, argument = 0) {
    if(missing(modulus) && missing(argument)) {
	## assume 'real' and 'imaginary'
	.Internal(complex(length.out, real, imaginary))
    } else {
	n <- max(length.out, length(argument), length(modulus))
	rep_len(modulus, n) * exp(1i * rep_len(argument, n))
    }
}

single <- function(length = 0L)
    structure(vector("double", length), Csingle=TRUE)

## A vector of the same element type as 'x'.  A storage-mode name is not
## always enough to say what that type is: it carries an 'xinteger'
## vector's width and kind but not its sentinel policy, so an existing
## vector has to stand in for the type no name spells.
.vectorlike <- function(x, length = 0L)
{
    ## is.vector() is too narrow here: an atomic vector with attributes,
    ## such as a matrix, is still a valid element-type donor.  Pairlists,
    ## NULL and the other non-vector SEXPTYPEs are not.
    if(is.null(x) || is.pairlist(x) ||
       !(is.atomic(x) || is.list(x) || is.expression(x)))
	stop("'x' must be a vector")

    if(is.xinteger(x))
	xinteger(length, xintegerWidth(x), xintegerKind(x),
		 xintegerHasNA(x))
    else
	vector(typeof(x), length)
}

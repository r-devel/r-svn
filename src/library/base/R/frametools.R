#  File src/library/base/R/frametools.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2023 The R Core Team
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

subset.data.frame <- function (x, subset, select, drop = FALSE, ...)
{
    chkDots(...)
    r <- if(missing(subset))
	rep_len(TRUE, nrow(x)) # cannot rely on recycling in 0-row case
    else {
	e <- substitute(subset)
	r <- eval(e, x, parent.frame())
        if(!is.logical(r)) stop("'subset' must be logical")
	r & !is.na(r)
    }
    vars <- if(missing(select))
	rep_len(TRUE, ncol(x)) # don't rely on recycling here either
    else {
	nl <- as.list(seq_along(x))
	names(nl) <- names(x)
	eval(substitute(select), nl, parent.frame())
    }
    ## PR#15823 suggested that sometimes which(r) would be faster,
    ## but this is not intended for programmatic use and the
    ## difference is tens of ms on a 1 million-row data frame.
    x[r, vars, drop = drop]
}

subset <- function(x, ...) UseMethod("subset")

subset.default <- function(x, subset, ...) {
    if(!is.logical(subset)) stop("'subset' must be logical")
    x[subset & !is.na(subset)]
}

subset.matrix <- function(x, subset, select, drop = FALSE, ...)
{
    if(missing(select))
	vars <- TRUE
    else {
	nl <- as.list(1L:ncol(x))
	names(nl) <- colnames(x)
	vars <- eval(substitute(select), nl, parent.frame())
    }
    if(missing(subset)) subset <- TRUE
    else if(!is.logical(subset)) stop("'subset' must be logical")
    x[subset & !is.na(subset), vars, drop = drop]
}

### Notice use of non-syntactic variable name for the first argument
### This used to be "x", but then you couldn't create a variable
### called "x"...

transform.data.frame <- function (`_data`, ...)
{
    e <- eval(substitute(list(...)), `_data`, parent.frame())
    tags <- names(e)
    inx <- match(tags, names(`_data`))
    matched <- !is.na(inx)
    if (any(matched)) {
	`_data`[inx[matched]] <- e[matched]
	`_data` <- data.frame(`_data`, check.names = FALSE)
    }
    if (!all(matched)) { # add as separate arguments to get replication
	args <- e[!matched]
	args[["check.names"]] <- FALSE # PR#17890
	do.call("data.frame", c(list(`_data`), args))
    } else `_data`
}

transform <- function(`_data`,...) UseMethod("transform")

## Actually, I have no idea what to transform(), except dataframes.
## The default converts its argument to a dataframe and transforms
## that. This is probably marginally useful at best. --pd
transform.default <- function(`_data`,...)
    transform.data.frame(as.data.frame(`_data`),...)

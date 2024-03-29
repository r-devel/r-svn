#  File src/library/stats/R/dist.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2024 The R Core Team
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

dist <- function(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
{
    ## account for possible spellings of euclid?an
    if(!is.na(pmatch(method, "euclidian")))
	method <- "euclidean"

    METHODS <- c("euclidean", "maximum",
		 "manhattan", "canberra", "binary", "minkowski")
    method <- pmatch(method, METHODS)
    if(is.na(method))
	stop("invalid distance method")
    if(method == -1)
	stop("ambiguous distance method")

    x <- as.matrix(x)
    N  <- nrow(x)
    attrs <- if(method == 6L)
        list(Size = N, Labels =  dimnames(x)[[1L]], Diag = diag,
             Upper = upper, method = METHODS[method],
             p = p, call = match.call(), class = "dist")
    else
        list(Size = N, Labels =  dimnames(x)[[1L]], Diag = diag,
             Upper = upper, method = METHODS[method],
             call = match.call(), class = "dist")
    .Call(C_Cdist, x, method, attrs, p)
}

format.dist <- function(x, ...) format(as.vector(x), ...)

as.matrix.dist <- function(x, ...)
{
    size <- attr(x, "Size")
    df <- matrix(0, size, size)
    ## instead of  lower <- (row(df) > col(df)) and  t(.), create {lo, up} directly:
    if (size > 1L) {
        n..1 <- (size - 1L):1L # n:1
        s.1 <- size + 1L
        up <- sequence.default(n..1, from = seq.int(s.1, length(df),      s.1), by = size)
        lo <- sequence.default(n..1, from = seq.int(2L , length(df) + 1L, s.1))
        df[up] <- df[lo] <- x ## preserving NAs in x
    }
    labels <- attr(x, "Labels") %||% seq_len(size)
    dimnames(df) <- list(labels,labels)
    df
}


as.dist <- function(m, diag = FALSE, upper = FALSE)
    UseMethod("as.dist")

as.dist.default <- function(m, diag = FALSE, upper = FALSE)
{
    if (inherits(m,"dist"))
	ans <- m
    else { ## matrix |-> dist
	m <- as.matrix(m)
        if(!is.numeric(m)) # coerce w/o losing attributes
            storage.mode(m) <- "numeric"
        p <- nrow(m)
        if(ncol(m) != p) warning("non-square matrix")
	ans <- m[row(m) > col(m)]
	attributes(ans) <- NULL
	if(!is.null(rownames(m)))
	    attr(ans,"Labels") <- rownames(m)
	else if(!is.null(colnames(m)))
	    attr(ans,"Labels") <- colnames(m)
	attr(ans,"Size") <- p
	attr(ans, "call") <- match.call()
	class(ans) <- "dist"
    }
    if(is.null(attr(ans,"Diag")) || !missing(diag))
	attr(ans,"Diag") <- diag
    if(is.null(attr(ans,"Upper")) || !missing(upper))
	attr(ans,"Upper") <- upper
    ans
}


print.dist <-
    function(x, diag = NULL, upper = NULL,
	     digits = getOption("digits"), justify = "none", right = TRUE, ...)
{
    if(length(x)) {
	if(is.null(diag))
	    diag <- attr(x, "Diag") %||% FALSE
	if(is.null(upper))
	    upper <- attr(x,"Upper") %||% FALSE

	m <- as.matrix(x)
	cf <- format(m, digits = digits, justify = justify)
	if(!upper)
	    cf[row(cf) < col(cf)] <- ""
	if(!diag)
	    cf[row(cf) == col(cf)] <- ""

	## Better: use an improved prettyNum() function -> ../../base/R/format.R
	##-	if(any((i <- m == floor(m))))
	##-	    cf[i] <- sub("0+$", "", cf[i])
	print(if(diag || upper) cf else cf[-1, -attr(x, "Size"), drop = FALSE],
	      quote = FALSE, right = right, ...)
    } else {
	cat(data.class(x),"(0)\n", sep = "")
    }
    invisible(x)
}

labels.dist <- function (object, ...) attr(object,"Labels")

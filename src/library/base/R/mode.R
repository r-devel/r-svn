#  File src/library/base/R/mode.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

mode <- function(x) {
    if(is.expression(x)) return("expression")
    if(is.call(x))
	return(switch(deparse(x[[1L]])[1L],
		      "(" = "(",
		      ## otherwise
		      "call"))
    if(is.name(x)) "name" else
    switch(tx <- typeof(x),
	   double =, integer =, xinteger = "numeric", # 'real=' dropped, 2000/Jan/14
	   closure =, builtin =, special = "function",
	   ## otherwise
	   tx)
}

`mode<-` <- function(x, value)
{
    if (storage.mode(x) == value) return(x)
    if (is.xinteger(x) && mode(x) == value) return(x)
    if(is.factor(x)) stop("invalid to change the storage mode of a factor")
    ## An 'xinteger' type is named by its width and kind (see
    ## R_xintTypeFromName in src/main/xints.c).  Plain "xinteger" is also
    ## handled here so that changing mode cannot silently invent a width.
    if(value == "xinteger" || .isXIntTypeName(value)) {
	storage.mode(x) <- value
	return(x)
    }
    atr <- attributes(x)
    isSingle <- !is.null(attr(x, "Csingle"))
    setSingle <- value == "single"
    mde <- get(paste0("as.",value), mode = "function", envir = parent.frame())
    x <- mde(x)
    attributes(x) <- atr
    ## this avoids one copy
    if(setSingle != isSingle)
        attr(x, "Csingle") <- if(setSingle) TRUE # else NULL
    x
}

storage.mode <- function(x) {
    if(is.xinteger(x)) {
	w <- xintegerWidth(x)
	return(switch(xintegerKind(x),
		      unsigned = paste0("uint", 8L * w),
		      signed = paste0("int", 8L * w)))
    }
    switch(tx <- typeof(x),
	   closure = , builtin = , special = "function",
	   ## otherwise
	   tx)
}

.storage_info <- function(x)
{
    tx <- typeof(x)
    if(!tx %in% c("logical", "integer", "double", "complex", "character",
		  "raw", "list", "expression", "xinteger"))
	stop("'x' must be a vector")

    if(tx == "xinteger") {
	element_size <- xintegerWidth(x)
	signed <- xintegerKind(x) == "signed"
	nullable <- xintegerHasNA(x)
    } else {
	element_size <- switch(tx,
		logical =, integer = 4L,
		double = 8L,
		complex = 16L,
		raw = 1L,
		character =, list =, expression =
		    as.integer(.Machine$sizeof.pointer))
	signed <- switch(tx, integer = TRUE, raw = FALSE, NA)
	nullable <- tx %in% c("logical", "integer", "double", "complex",
			    "character")
    }

    list(typeof = tx, storage_mode = storage.mode(x),
	 element_size = element_size, signed = signed, nullable = nullable)
}

### storage.mode<- is primitive as from R 2.6.0

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
	   double =, integer = "numeric", # 'real=' dropped, 2000/Jan/14
	   closure =, builtin =, special = "function",
	   ## These are their own mode, and naming them is not redundant
	   ## with the default below: a 'bytes' vector is the only type
	   ## whose typeof() is not a fixed string -- it names the width and
	   ## the kind -- so this is what keeps every other type off a test
	   ## that has to look at the object.  A name left out of the list
	   ## only pays for that test; it is not answered wrongly.
	   logical =, character =, complex =, raw =, list =, pairlist =,
	   environment =, externalptr =, promise =, weakref =, bytecode =,
	   S4 =, `NULL` = tx,
	   ## otherwise: the public mode follows the interpretation, not
	   ## BYTESXP's shared storage representation
	   if(is.numeric(x)) "numeric" else if(is.bytes(x)) "bytes" else tx)
}

`mode<-` <- function(x, value)
{
    if (storage.mode(x) == value) return(x)
    if (is.fixedwidth(x) && mode(x) == value) return(x)
    if(is.factor(x)) stop("invalid to change the storage mode of a factor")
    ## A 'bytes' type is named by its width and kind (see
    ## R_bytesTypeFromName in src/main/bytes.c).  Plain "bytes" is also
    ## handled here so that as.bytes()'s defaults cannot silently pick
    ## a width while changing mode.
    if(value == "bytes" || .isBytesTypeName(value)) {
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

storage.mode <- function(x)
    switch(tx <- typeof(x),
	   closure = , builtin = , special = "function",
	   ## otherwise
	   tx)

### storage.mode<- is primitive as from R 2.6.0

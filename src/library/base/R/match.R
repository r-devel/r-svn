#  File src/library/base/R/match.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2025 The R Core Team
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

match <- function(x, table, nomatch = NA_integer_, incomparables = NULL)
    .Internal(match(x, table, nomatch, incomparables))

match.call <-
    function(definition=sys.function(sys.parent()),
             call=sys.call(sys.parent()), expand.dots=TRUE,
             envir=parent.frame(2L))
{
    if (!missing(definition) && is.null(definition)) {
        definition <- sys.function(sys.parent())
    }
    .Internal(match.call(definition,call,expand.dots,envir))
}

pmatch <- function(x, table, nomatch = NA_integer_, duplicates.ok = FALSE)
    .Internal(pmatch(as.character(x), as.character(table), nomatch,
                     duplicates.ok))

# "utils::hasName(x, name)" is defined to be the same as "name %in% names(x)",
# so change it if this changes.
`%in%`  <- function(x, table) match(x, table, nomatch = 0L) > 0L

match.arg <- function (arg, choices, several.ok = FALSE)
{
    if (missing(choices)) {
	formal.args <- formals(sys.function(sysP <- sys.parent()))
	choices <- eval(formal.args[[as.character(substitute(arg))]],
			envir = sys.frame(sysP))
    }
    if (is.null(arg)) return(choices[1L])
    else if(!is.character(arg))
	stop("'arg' must be NULL or a character vector")
    if (!several.ok) { # most important (default) case:
        ## the arg can be the whole of choices as a default argument.
        if(identical(arg, choices)) return(arg[1L])
    if(length(arg) != 1L) stop(gettextf("'%s' must be of length 1", "arg"), domain=NA)
    } else if(length(arg) == 0L) stop("'arg' must be of length >= 1")

    ## handle each element of arg separately
    i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
    if (all(i == 0L))
        stop(sprintf(ngettext(length(chs <- unique(choices[nzchar(choices)])),
                              "'arg' should be %s",
                              "'arg' should be one of %s"),
                     paste(dQuote(chs), collapse=", ")),
             domain = NA)
    i <- i[i > 0L]
    if (!several.ok && length(i) > 1) ## can this happen ??
        stop("there is more than one match in 'match.arg'")
    choices[i]
}

charmatch <- function(x, table, nomatch = NA_integer_)
    .Internal(charmatch(as.character(x), as.character(table), nomatch))

char.expand <- function(input, target, nomatch = stop("no match"))
{
    if(length(input) != 1L)
	stop("'input' must have length 1")
    if(!(is.character(input) && is.character(target)))
	stop("'input' and 'target' must be character vectors")
    y <- .Internal(charmatch(input, target, NA_integer_))
    if(anyNA(y)) eval(nomatch)
    target[y]
}

mtfrm <- function(x)
    UseMethod("mtfrm")

mtfrm.default <- function(x) {
    if(length(y <- as.character(x)) != length(x))
        stop("cannot mtfrm")
    y
}

mtfrm.Date <- # <- for speed
mtfrm.POSIXct <-
mtfrm.POSIXlt <- function(x) as.vector(x, "any")

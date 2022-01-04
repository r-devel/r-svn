#  File src/library/base/R/all.equal.R
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

all.equal <- function(target, current, ...) UseMethod("all.equal")

## not really: do this inside all.equal.default() :
## all.equal.... <- function(target, current, ...) . . . .

all.equal.default <- function(target, current, ...)
{
    ## Really a dispatcher given mode() of args :
    ## use data.class as unlike class it does not give "integer"
    if(is.language(target))
	return(all.equal.language(target, current, ...))
    if(is.function(target)) {
        .Deprecated("all.equal(*)", old="all.equal.default(<function>)")
	return(all.equal.function(target, current, ...))
    }
    if(is.environment(target) || is.environment(current))# both: unclass() fails on env.
	return(all.equal.environment(target, current, ...))
    if(is.recursive(target)) {
        ## NB: "..." is recursive but not a list
	return(if(typeof(target) == "..." && typeof(current) == "...") {
		   ## <DOTSXP> ... may change .. *NOT* part of long-term API
		   if(identical(target, current))
		       TRUE
		   else if((lt <- length(target)) != (lc <- length(current)))
		       gettextf('"..."-typed": lengths (%d, %d) differ', lt, lc)
		   else if(xor(is.null(nt <- names(target)),
			       is.null(nc <- names(current)))) {
		       if(length(nt))
                           gettext('"..."-typed: names in target but not in current')
                       else ## length(nc)
                           gettext('"..."-typed: names in current but not in target')
		   } else if(!is.null(nt)) { # and !is.null(nc)
			   nt <- sort(nt)
			   nc <- sort(nc)
			   if(identical(nt, nc)) TRUE
			   else c(gettext('"..."-typed": sorted names differ'),
				  all.equal.character(nt, nc, ...))
		   }
		   else ## names both NULL
		       gettext('"..."-types of the same length, no names, but not identical')
	       }
	       else all.equal.list(target, current, ...))
    }
    msg <- switch (mode(target),
                   integer   = ,
                   complex   = ,
                   numeric   = all.equal.numeric  (target, current, ...),
                   character = all.equal.character(target, current, ...),
                   logical   = ,
                   raw       = all.equal.raw      (target, current, ...),
		   ## assumes that slots are implemented as attributes :
		   S4        = attr.all.equal(target, current, ...),
                   if(data.class(target) != data.class(current)) {
                       gettextf("target is %s, current is %s",
                                data.class(target), data.class(current))
                   } else NULL)
    if(is.null(msg)) TRUE else msg
}

all.equal.function <- function(target, current, check.environment = TRUE, ...)
{
    msg <- all.equal.language(target, current, ...)
    if(check.environment) {
        ## pre-check w/ identical(), for speed & against infinite recursion:
        ee <- identical(environment(target),
                        environment(current), ignore.environment=FALSE)
        if(!ee)
            ee <- all.equal.environment(environment(target),
                                        environment(current), ...)
        if(isTRUE(msg))
            ee
        else
            c(msg, if(!isTRUE(ee)) ee)
    } else
        msg
}


all.equal.numeric <-
    function(target, current, tolerance = sqrt(.Machine$double.eps),
             scale = NULL, countEQ = FALSE,
             formatFUN = function(err, what) format(err),
             ..., check.attributes = TRUE)
{
    if (!is.numeric(tolerance))
        stop("'tolerance' should be numeric")
    if (!is.numeric(scale) && !is.null(scale))
        stop("'scale' should be numeric or NULL")
    if (!is.logical(check.attributes))
        stop(gettextf("'%s' must be logical", "check.attributes"), domain = NA)
    msg <- if(check.attributes)
	attr.all.equal(target, current, tolerance = tolerance, scale = scale,
                       ...)
    if(data.class(target) != data.class(current)) {
	msg <- c(msg, gettextf("target is %s, current is %s", data.class(target), data.class(current)))
	return(msg)
    }

    lt <- length(target)
    lc <- length(current)
    cplx <- is.complex(target) # and so current must be too.
    if(lt != lc) {
	## *replace* the 'Lengths' msg[] from attr.all.equal():
        if(!is.null(msg)) msg <- msg[- grep(gettextf("Lengths: %d, %d", lt, lc), msg, fixed=TRUE)]
	msg <- c(msg, if (cplx) gettextf("Complex: lengths (%d, %d) differ" lt, lc)
                      else gettextf("Numeric: lengths (%d, %d) differ", lt, lc))
	return(msg)
    }
    ## remove atttributes (remember these are both numeric or complex vectors)
    ## one place this is needed is to unclass Surv objects in the rpart test suite.
    target <- as.vector(target)
    current <- as.vector(current)
    out <- is.na(target)
    if(any(out != is.na(current))) {
	msg <- c(msg, gettextf("'is.NA' value mismatch: %d in current %d in target",
                               sum(is.na(current)), sum(out)))
	return(msg)
    }
    out <- out | target == current # equal NAs _or_ numbers
    if(all(out)) return(if(is.null(msg)) TRUE else msg)
    if(countEQ) {
        N <- length(out)
        sabst0 <- sum(abs(target[out])/N)
    } else
        sabst0 <- 0
    target  <- target [!out]
    current <- current[!out]
    if(!countEQ) N <- length(target)
    if(is.integer(target) && is.integer(current)) target <- as.double(target)
    what <-
	if(is.null(scale)) {
	    scale <- (sabst0 + sum(abs(target)/N))
	    if(is.finite(scale) && scale > tolerance) {
		"relative"
	    } else {
		scale <- 1
                "absolute"
	    }
	} else {
	    stopifnot(all(scale > 0))
	    if(all(abs(scale - 1) < 1e-7)) "absolute" else "scaled"
	}
    xy <- sum(abs(target - current)/(N*scale)) ## abs(z) == Mod(z) for complex

    if (cplx) what <- paste0(what, "Mod") # PR#10575
    if(is.na(xy) || xy > tolerance) {
        xy_fmt <- formatFUN(xy, what)
        msg <- c(msg, switch(what,
                             absolute = gettextf("Mean absolute difference: %s", xy_fmt),
                             absoluteMod = gettextf("Mean absolute Mod difference: %s", xy_fmt),
                             relative = gettextf("Mean relative difference: %s", xy_fmt),
                             relativeMod = gettextf("Mean relative Mod difference: %s", xy_fmt),
                             scaled = gettextf("Mean scaled difference: %s", xy_fmt),
                             scaledMod = gettextf("Mean scaled Mod difference: %s", xy_fmt)))
    }

    if(is.null(msg)) TRUE else msg
}

all.equal.character <-
    function(target, current, ..., check.attributes = TRUE)
{
    if (!is.logical(check.attributes))
        stop(gettextf("'%s' must be logical", "check.attributes"), domain = NA)
    msg <-  if(check.attributes) attr.all.equal(target, current, ...)
    if(data.class(target) != data.class(current)) {
	msg <- c(msg, gettextf("target is %s, current is %s", data.class(target), data.class(current)))
	return(msg)
    }
    lt <- length(target)
    lc <- length(current)
    if(lt != lc) {
        if(!is.null(msg)) msg <- msg[- grep(gettextf("Lengths: %d, %d", lt, lc), msg, fixed=TRUE)]
	msg <- c(msg,
                 gettextf("Lengths (%d, %d) differ (string compare on first %d)",
                          lt, lc, ll <- min(lt, lc)))
	ll <- seq_len(ll)
	target <- target[ll]
	current <- current[ll]
    }
    nas <- is.na(target); nasc <- is.na(current)
    if (any(nas != nasc)) {
	msg <- c(msg, gettextf("'is.NA' value mismatch: %d in current %d in target",
                               sum(nasc), sum(nas)))
	return(msg)
    }
    ne <- !nas & (target != current)
    if(!any(ne) && is.null(msg)) TRUE
    else c(msg, sprintf(ngettext(count_ne <- sum(ne), "1 string mismatch", "%d string mismatches"),
                        count_ne))
    else msg
}

## In 'base' these are all visible, so need to test both args:

all.equal.envRefClass <- function (target, current, ...) {
    if(!methods::is(target,  "envRefClass")) return(gettext("'target' is not an envRefClass"))
    if(!methods::is(current, "envRefClass")) return(gettext("'current' is not an envRefClass"))
    if(!isTRUE(ae <- all.equal(class(target), class(current), ...)))
	return(gettextf("Classes differ: %s", paste(ae, collapse=" ")))
    getCl <- function(x) { cl <- tryCatch(x$getClass(), error=function(e) NULL)
			   if(is.null(cl)) class(x) else cl }
    if(!identical(cld <- getCl(target), c2 <- getCl(current))) {
	hasCA <- "check.attributes" %in% ...names()
	ae <-
	    if(hasCA) all.equal(cld, c2, ...)
	    else all.equal(cld, c2, check.attributes=FALSE, ...)
        if(isTRUE(ae) && !hasCA) ae <- all.equal(cld, c2, ...)
	return(gettextf("Class definitions are not identical%s",
		        if(isTRUE(ae)) "" else paste(":", ae, collapse=" ")))
    }
    if(!isS4(cld)) ## prototype / incomplete
	return(if(identical(target, current)) TRUE
	       else gettext("different prototypical 'envRefClass' objects"))
    flds <- names(cld@fieldClasses) ## else NULL
    asL <- function(O) sapply(flds, function(ch) O[[ch]], simplify = FALSE)
    ## ## ?setRefClass explicitly says users should not use ".<foo>" fields:
    ## if(is.na(all.names)) all.names <- FALSE
    ## ## try preventing infinite recursion by not looking at  .self :
    ## T <- function(ls) ls[is.na(match(names(ls), c(".self", methods:::envRefMethodNames)))]
    ## asL <- function(E) T(as.list(as.environment(E), all.names=all.names, sorted=TRUE))
    n <- all.equal.list(asL(target), asL(current), ...)
    ## Can have slots (apart from '.xData'), though not recommended; check these:
    sns <- names(cld@slots); sns <- sns[sns != ".xData"]
    msg <- if(length(sns)) {
	L <- lapply(sns, function(sn)
	    all.equal(methods::slot(target, sn),
                      methods::slot(current, sn), ...))
	unlist(L[vapply(L, is.character, NA)])
    }
    if(is.character(n)) msg <- c(msg, n)
    if(is.null(msg)) TRUE else msg
}

all.equal.environment <- function (target, current, all.names=TRUE, evaluate=TRUE, ...) {
    if(!is.environment (target)) return(gettext( "'target' is not an environment"))
    if(!is.environment(current)) return(gettext("'current' is not an environment"))
    if(identical(target, current)) # only true if *same* address
                 ## ignore.environment =
                 ##     if(!is.na(i <- match("check.environment", ...names())))
                 ##         ! ...elt(i) else FALSE))
        return(TRUE)
    ae.run <- dynGet("__all.eq.E__", NULL)
    if(is.null(ae.run))
	"__all.eq.E__" <- environment() # -> 5 visible + 6 ".<..>" objects
    else { ## ae.run contains previous target, current, ..

	## If we exactly match one of these, we return TRUE here,
	## otherwise, divert to all.equal(as.list(.), ...) below

	## needs recursive function -- a loop with  em <- em$mm	 destroys the env!
	do1 <- function(em) {
	    if(identical(target, em$target) && identical(current, em$current))
		TRUE
	    else if(!is.null(em$ mm)) ## recurse
		do1(em$ mm)
	    else {
		## add the new (target, current) pair, and return FALSE
		e <- new.env(parent = emptyenv())
		e$target  <- target
		e$current <- current
		em$ mm <- e
		FALSE
	    }
	}

	if(do1(ae.run)) return(TRUE)
	## else, continue:
    }
    if(evaluate) {
        Lt <- as.list.environment(target , all.names=all.names, sorted=TRUE)
        Lc <- as.list.environment(current, all.names=all.names, sorted=TRUE)
        ## identical(*,*) for the environment with `...` and general consistency:
        if(identical(Lt, Lc))
            TRUE
        else
            all.equal.list(Lt, Lc, ...)
    } else { ## do *not* force promises, i.e. *not* coerce to list
        if(!identical(nt <- sort(names(target )),
                      nc <- sort(names(current))))
            paste(gettextf("names of environments differ: %s", all.equal(nt, nc, ...)), collapse=" ")
        else
            gettext("environments contain objects of the same names, but are not identical")
    }
}

all.equal.factor <- function(target, current, ..., check.attributes = TRUE)
{
    if(!inherits(current, "factor"))
	return(gettext("'current' is not a factor"))
    msg <-  if(check.attributes) attr.all.equal(target, current, ...)
    class(target) <- class(current) <- NULL
    nax <- is.na(target)
    nay <- is.na(current)
    n <- sum(nax != nay)
    if(n == 0L) { # nax == nay (w/ recycling!)
	target  <- levels(target) [target [!nax]]
	current <- levels(current)[current[!nay]]
	n <- all.equal(target, current,
		       check.attributes = check.attributes, ...)
	if(is.character(n)) msg <- c(msg, n)
    } else
        msg <- c(msg, sprintf(ngettext(n, "1 NA mismatch", "%d NA mismatches"), n))
    if(is.null(msg)) TRUE else msg
}

all.equal.formula <- function(target, current, ...)
{
    ## NB: this assumes the default method for class formula, not
    ## the misquided one in package Formula
    if(length(target) != length(current))
	return(gettextf("target, current differ in having response: %s, %s",
                        length(target ) == 3L,
                        length(current) == 3L))
    ## <NOTE>
    ## This takes same-length formulas as all equal if they deparse
    ## identically.  As of 2010-02-24, deparsing strips attributes; if
    ## this is changed, the all equal behavior will change unless the
    ## test is changed.
    ## </NOTE>
    if(!identical(deparse(target), deparse(current)))
	gettext("formulas differ in contents")
    else TRUE
}

all.equal.language <- function(target, current, ...)
{
    mt <- mode(target)
    mc <- mode(current)
    if(mt == "expression" && mc == "expression")
	return(all.equal.list(target, current, ...))
    ttxt <- paste(deparse(target ), collapse = "\n")
    ## try: if 'current' is not "language" and deparse() bails out for DOTSXP, see PR#18029
    ctxt <- tryCatch(paste(deparse(current), collapse = "\n"), error=function(e) NULL)
    msg <- c(if(mt != mc)
	     gettextf("Modes of target, current: %s, %s", mt, mc),
	     if(is.null(ctxt))
		 gettext("current is not deparse()able")
	     else if(ttxt != ctxt) {
		 if(pmatch(ttxt, ctxt, 0L))
		     gettext("target is a subset of current")
		 else if(pmatch(ctxt, ttxt, 0L))
		     gettext("current is a subset of target")
		 else gettext("target, current do not match when deparsed")
	     })
    if(is.null(msg)) TRUE else msg
}

## use.names is new in 3.1.0: avoid partial/positional matching
all.equal.list <- function(target, current, ...,
                           check.attributes = TRUE, use.names = TRUE)
{
    if (!is.logical(check.attributes))
        stop(gettextf("'%s' must be logical", "check.attributes"),
             domain = NA)
    if (!is.logical(use.names))
        stop(gettextf("'%s' must be logical", "use.names"), domain = NA)
    msg <- if(check.attributes) attr.all.equal(target, current, ...)
    ## Unclass to ensure we get the low-level components
    target <- unclass(target) # "list"
    current <- unclass(current)# ??
    ## Comparing the data.class() is not ok, as a list matrix is 'matrix' not 'list'
    if(!is.list(target) && !is.vector(target))
	return(c(msg, gettext("%s is not list-like", "target")))
    if(!is.list(current) && !is.vector(current))
	return(c(msg, gettext("%s is not list-like", "current")))
    if((nt <- length(target)) != (nc <- length(current))) {
        if(!is.null(msg)) msg <- msg[- grep(gettextf("Lengths: %d, %d", nt, nc), msg, fixed=TRUE)]
	n <- min(nt, nc)
	msg <- c(msg, gettextf("Length mismatch: comparison on first %d components", n))
    }
    iseq <- seq_len(n)
    if(use.names)
	use.names <- (length(nt <- names(target )[iseq]) == n &&
		      length(nc <- names(current)[iseq]) == n)
    for(i in iseq) {
	mi <- all.equal(target[[i]], current[[i]],
			check.attributes=check.attributes, use.names=use.names, ...)
	if(is.character(mi))
	    msg <- c(msg, gettextf("Component %s: %s",
				   if(use.names && nt[i] == nc[i]) dQuote(nt[i]) else i,
				   mi))
    }
    if(is.null(msg)) TRUE else msg
}

## also used for logical
all.equal.raw <-
    function(target, current, ..., check.attributes = TRUE)
{
    if (!is.logical(check.attributes))
        stop(gettextf("'%s' must be logical", "check.attributes"), domain = NA)
    msg <-  if(check.attributes) attr.all.equal(target, current, ...)
    if(data.class(target) != data.class(current)) {
	msg <- c(msg, gettextf("target is %s, current is %s", data.class(target), data.class(current)))
	return(msg)
    }
    lt <- length(target)
    lc <- length(current)
    if(lt != lc) {
	if(!is.null(msg)) msg <- msg[- grep(gettextf("Lengths: %d, %d", lt, lc), msg, fixed=TRUE)]
	msg <- c(msg, gettextf("Lengths (%d, %d) differ (comparison on first %d components)",
                               lt, lc, ll <- min(lt, lc)))
	ll <- seq_len(ll)
	target <- target[ll]
	current <- current[ll]
    }
    # raws do not have NAs, but logicals do
    nas <- is.na(target); nasc <- is.na(current)
    if (any(nas != nasc)) {
	msg <- c(msg, gettextf("'is.NA' value mismatch: %d in current %d in target", sum(nasc), sum(nas)))
	return(msg)
    }
    ne <- !nas & (target != current)
    if(!any(ne) && is.null(msg)) TRUE
    else c(msg, sprintf(ngettext(count_ne <- sum(ne), "1 element mismatch", "%d element mismatches"),
                        count_ne))
    else msg
}


## attributes are a pairlist, so never 'long'
attr.all.equal <- function(target, current, ...,
                           check.attributes = TRUE, check.names = TRUE)
{
    ##--- "all.equal(.)" for attributes ---
    ##---  Auxiliary in all.equal(.) methods --- return NULL or character()
    if (!is.logical(check.attributes))
        stop(gettextf("'%s' must be logical", "check.attributes"), domain = NA)
    if (!is.logical(check.names))
        stop(gettextf("'%s' must be logical", "check.names"), domain = NA)
    msg <- NULL
    if(mode(target) != mode(current))
	msg <- gettextf("Modes: %d, %d", mode(target), mode(current))
    if(length(target) != length(current))
	msg <- c(msg,
                 gettextf("Lengths: %d, %d", length(target), length(current)))
    ax <- attributes(target)
    ay <- attributes(current)
    if(check.names) {
        nx <- names(target)
        ny <- names(current)
        if((lx <- length(nx)) | (ly <- length(ny))) {
            ## names() treated now; hence NOT with attributes()
            ax$names <- ay$names <- NULL
            if(lx && ly) {
                if(is.character(m <- all.equal.character(nx, ny, check.attributes = check.attributes)))
                    msg <- c(msg, paste("Names:", m))
            } else if(lx)
                msg <- c(msg, gettextf("names for %s but not for %s", "target", "current"))
            else msg <- c(msg, gettextf("names for %s but not for %s", "current", "target"))
        }
    } else {
	ax[["names"]] <- NULL
	ay[["names"]] <- NULL
    }

    if(check.attributes && (length(ax) || length(ay))) {# some (more) attributes
	## order by names before comparison:
	nx <- names(ax)
	ny <- names(ay)
	if(length(nx)) ax <- ax[order(nx)]
	if(length(ny)) ay <- ay[order(ny)]
	tt <- all.equal(ax, ay, ..., check.attributes = check.attributes)
	if(is.character(tt)) msg <- c(msg, gettextf("Attributes: < %s >", tt))
    }
    msg # NULL or character
}

## formerly in datetime.R
## force absolute comparisons
all.equal.POSIXt <- function(target, current, ..., tolerance = 1e-3, scale,
                             check.tzone = TRUE)
{
    if(!inherits(target, "POSIXt"))
        return(gettextf("'%s' is not a POSIXt", "target"))
    if(!inherits(current, "POSIXt"))
        return(gettextf("'%s' is not a POSIXt", "current"))
    target <- as.POSIXct(target)
    current <- as.POSIXct(current)
    msg <- NULL
    if(check.tzone) {
        ## See check_tzones():
        tz <- function(dt) {
            if(is.null(tz <- attr(dt, "tzone"))) "" else tz[1L]
        }
        ## FIXME: check_tzones() ignores differences with "" as time zone,
        ## regardless of whether that other time zone is the current one.
        ## However, this code does not handle "" at all, so that it is
        ## treated as "inconsistent" even with the current time zone,
        ## leading to surprising results, e.g.
        ##   x <- Sys.time() ; all.equal(x, as.POSIXlt(x))
        tzt <- tz(target)
        tzc <- tz(current)
        if(!isTRUE(tzt == tzc))
            msg <- gettextf("'tzone' attributes are inconsistent ('%s' and '%s')",
                            tzt, tzc)
    }
    attr(target, "tzone") <- attr(current, "tzone") <- NULL
    val <- all.equal.numeric(target, current, ...,
                             tolerance = tolerance, scale = 1)
    if(!isTRUE(val)) msg <- c(msg, val)
    if(is.null(msg)) TRUE else msg
}

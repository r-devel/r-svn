#  File src/library/utils/R/objects.R
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

## findGeneric(fname) :  is 'fname' the name of an S3 generic ?
##			[unexported function used only in this file]
findGeneric <- function(fname, envir, warnS4only = TRUE)
{
    if(!exists(fname, mode = "function", envir = envir)) return("")
    f <- get(fname, mode = "function", envir = envir)
    ## FIXME? In the first case, e.g. 'methods(qr)', we are very inefficient:
    ##  inside methods() we transform the 'qr' function object into a character,
    ##  whereas here, we revert this, searching around unnecessarily
    ##
    if(.isMethodsDispatchOn() && methods::is(f, "genericFunction")) {
	## maybe an S3 generic was turned into the S4 default
	## Try to find it, otherwise warn :
	fMethsEnv <- methods::getMethodsForDispatch(f)
        meths <- as.list(fMethsEnv, all.names=TRUE)
        r <- meths[grep("^ANY\\b", names(meths))]
	if(any(ddm <- vapply(r, methods::is, logical(1L), "derivedDefaultMethod")))
	    f <- r[ddm][[1]]@.Data
	else if(warnS4only)
	    warning(gettextf(
	"'%s' is a formal generic function; S3 methods will not likely be found",
			     fname), domain = NA)
    }
    isUMEbrace <- function(e) {
        for (ee in as.list(e[-1L]))
            if (nzchar(res <- isUME(ee))) return(res)
        ""
    }
    isUMEif <- function(e) {
        if (length(e) == 3L) isUME(e[[3L]])
        else {
            if (nzchar(res <- isUME(e[[3L]]))) res
            else if (nzchar(res <- isUME(e[[4L]]))) res
            else ""
        }
    }
    isUME <- function(e) { ## is it an "UseMethod() calling function" ?
        if (is.call(e) && (is.name(e[[1L]]) || is.character(e[[1L]]))) {
            switch(as.character(e[[1L]]),
                   UseMethod = as.character(e[[2L]]),
                   "{" = isUMEbrace(e),
                   "if" = isUMEif(e),
                   "")
        } else ""
    }
    isUME(body(f))
}

getKnownS3generics <-
function()
    c(names(.knownS3Generics), tools:::.get_internal_S3_generics())

.S3methods <-
function(generic.function, class, envir=parent.frame(), all.names = FALSE, dropPath = FALSE, useEnv = FALSE)
{
    rbindSome <- function(df, nms, msg) {
        ## rbind.data.frame() -- dropping rows with duplicated names
        nms <- unique(nms)
        n2 <- length(nms)
        dnew <- data.frame(visible = rep.int(FALSE, n2),
                           from    = rep.int(msg,   n2),
                           row.names = nms)
        n <- nrow(df)
        if(n == 0L) return(dnew)
        ## else
        keep <- !duplicated(c(rownames(df), rownames(dnew)))
        rbind(df  [keep[1L:n] , ],
              dnew[keep[(n+1L):(n+n2)] , ])
    }

    S3MethodsStopList <- tools::nonS3methods(NULL)
    knownGenerics <- getKnownS3generics()
    methods.called <- identical(sys.call(-1)[[1]], as.symbol("methods"))
    if(useEnv) {
        attach(envir, pos = 2L, warn.conflicts = FALSE)
        if(methods.called) message("some methods may be unavailable outside of their namespace")
        on.exit(detach(2L))
    }
    sp <- search()
    if(dropPath) sp <- sp[c(if(useEnv) 1:2 else 1L, length(sp))]
    an <- lapply(sp, ls, all.names = all.names)
    lens <- lengths(an)
    an <- unlist(an, use.names=FALSE)
    names(an) <- rep.int(sp, lens)
    an <- an[!duplicated(an)] # removed masked objects, *keep* names
    info <- data.frame(visible = rep.int(TRUE, length(an)),
		       from = .rmpkg(names(an)),
                       row.names = an)
    if (!missing(generic.function)) {
	if (!is.character(generic.function))
	    generic.function <- deparse1(substitute(generic.function))
        ## else
        if(!exists(generic.function, mode = "function", envir = envir) &&
           !any(generic.function == c("Math", "Ops", "Complex", "Summary", "matrixOps")))
            stop(gettextf("no function '%s' is visible", generic.function),
                 domain = NA)
        warn.not.generic <- FALSE
        if(!any(generic.function == knownGenerics)) {
	    truegf <- findGeneric(generic.function, envir, warnS4only = !methods.called)
            if(truegf == "")
                warn.not.generic <- TRUE
            else if(truegf != generic.function) {
                warning(gettextf("generic function '%s' dispatches methods for generic '%s'",
                        generic.function, truegf), domain = NA)
                generic.function <- truegf
            }
        }
	info <- info[startsWith(row.names(info), paste0(generic.function,".")), ]
        info <- info[! row.names(info) %in% S3MethodsStopList, ]
        ## check that these are all functions
        ## might be none at this point
	if(nrow(info)) {
	    keep <- vapply(row.names(info), exists, logical(1), mode="function")
	    info <- info[keep, ]
	}
	if(warn.not.generic && nrow(info))
	    warning(gettextf(
	"function '%s' appears not to be S3 generic; found functions that look like S3 methods",
			     generic.function), domain = NA)

        ## also look for registered methods from namespaces
        ## we assume that only functions get registered.
        defenv <- if(!is.na(w <- .knownS3Generics[generic.function]))
            asNamespace(w)
        else {
            genfun <- get(generic.function, mode = "function", envir = envir)
            if(.isMethodsDispatchOn() && methods::is(genfun, "genericFunction"))
                genfun <- methods::finalDefaultMethod(genfun@default)
            .defenv_for_S3_registry(genfun)
        }
	S3reg <- names(get(".__S3MethodsTable__.", envir = defenv)) # may climb up search()
	S3reg <- S3reg[startsWith(S3reg, paste0(generic.function,"."))]
        if(length(S3reg))
            info <- rbindSome(info, S3reg, msg =
                              paste("registered S3method for",
                                    generic.function))
        ## both all() and all.equal() are generic, so
        if(generic.function == "all")
            info <- info[-grep("^all\\.equal", row.names(info)), ]
    }
    else if (!missing(class)) {
	if (!is.character(class))
	    class <- deparse1(substitute(class))
	if(length(class) > 1L) {
	    warning("'class' is of length > 1; only the first element will be used")
	    class <- class[1L]
	}
	name <- paste0(".", class, "$")
        name <- gsub("([.[])", "\\\\\\1", name)
        info <- info[grep(name, row.names(info)), ]
        info <- info[! row.names(info) %in% S3MethodsStopList, ]

        if(nrow(info)) {
            ## check if we can find a generic matching the name
            possible.generics <- gsub(name, "", row.names(info))
            keep <- vapply(possible.generics, function(nm) {
                if(nm %in% knownGenerics) return(TRUE)
                where <- find(nm, mode = "function")
		if(length(where))
		    any(vapply(where, function(w)
			nzchar(findGeneric(nm, envir=as.environment(w))), NA))
		else FALSE
	    }, logical(1))
            info <- info[keep, ]
        }

        ## also look for registered methods in loaded namespaces.
        ## These should only be registered in environments containing
        ## the corresponding generic, so we don't check again.
        ## Note that the generic will not necessarily be visible,
        ## as the package may not be loaded.
        S3reg <- unlist(lapply(loadedNamespaces(), function(i)
	    ls(getNamespace(i)[[".__S3MethodsTable__."]],
               pattern = name, all.names = all.names)))
        ## now methods like print.summary.aov will be picked up,
        ## so we do look for such mismatches.
        if(length(S3reg))
            S3reg <- S3reg[vapply(gsub(name, "", S3reg), exists, NA)]
        if(length(S3reg))
            info <- rbindSome(info, S3reg, msg = "registered S3method")
    }
    else stop("must supply 'generic.function' or 'class'")

    info$generic <- if (!missing(generic.function))
        rep.int(generic.function, nrow(info))
    else sub(paste0("\\.", class, "$"), "", row.names(info))
    info$isS4 <- rep.int(FALSE, nrow(info))

    info <- info[sort.list(row.names(info)), , drop=FALSE]
    res <- row.names(info)
    class(res) <- "MethodsFunction"
    attr(res, "info") <- info
    attr(res, "byclass") <- missing(generic.function)
    res
}

methods <-
function(generic.function, class, all.names = FALSE, dropPath = FALSE)
{
    envir <- parent.frame()
    useNS <- FALSE
    if(!missing(generic.function) && !is.character(generic.function)) {
        what <- substitute(generic.function)
        generic.function <-
            if(is.function(generic.function) &&
               is.call(what) &&
               (deparse(what[[1L]], nlines=1L) %in% c("::", ":::"))) {
                what <- as.character(what[2:3])
                envir <- asNamespace(what[[1L]])
                useNS <- TRUE
                what[[2L]]
            } else
                deparse(what)
    }

    if (!missing(class) && !is.character(class))
        class <- deparse1(substitute(class))

    s3 <- .S3methods(generic.function, class, envir, all.names=all.names, dropPath=dropPath,
                     useEnv = useNS)
    s4 <- if(.isMethodsDispatchOn()) methods::.S4methods(generic.function, class)

    .MethodsFunction(s3, s4, missing(generic.function))
}

.MethodsFunction <-
function(s3, s4, byclass)
{
    info3 <- attr(s3, "info")
    info4 <- attr(s4, "info")
    info <- rbind(info3, info4)
    dups <- duplicated(c(rownames(info3), rownames(info4)))
    info <- info[!dups, , drop=FALSE]
    info <- info[order(rownames(info)), , drop=FALSE]
    structure(rownames(info), info=info, byclass=byclass,
              class="MethodsFunction")
}

format.MethodsFunction <- function(x, byclass = attr(x, "byclass"), ...)
{
    info <- attr(x, "info")
	if (byclass)
	    unique(info$generic)
	else
	    paste0(rownames(info), visible = ifelse(info$visible, "", "*"))
}

print.MethodsFunction <- function(x, byclass = attr(x, "byclass"), ...)
{
    if (length(values <- format(x, byclass=byclass, ...))) {
        print(noquote(values))
        cat("see '?methods' for accessing help and source code\n")
    } else
        cat("no methods found\n")

    invisible(x)
}

getS3method <- function(f, class, optional = FALSE, envir = parent.frame())
{
    stopifnot(is.character(f), length(f) == 1L)
    stopifnot(is.character(class), length(class) == 1L)
    if(!any(f == getKnownS3generics())) {
        truegf <- findGeneric(f, envir)
        if(nzchar(truegf)) f <- truegf
        else {
            if(optional) return(NULL)
            else stop(gettextf("no function '%s' could be found", f), domain = NA)
        }
    }
    method <- paste(f, class, sep=".")
    if(!is.null(m <- get0(method, envir = envir, mode = "function"))) {
	## know: f is a knownS3generic, and method m is a visible function
	pkg <- if(isNamespace(em <- environment(m))) environmentName(em)
	       else if(is.primitive(m)) "base" ## else NULL
	if(is.na(match(method, tools::nonS3methods(pkg))))
	    return(m)
    }
    ## also look for registered method in namespaces
    defenv <-
	if(!is.na(w <- .knownS3Generics[f]))
	    asNamespace(w)
	else if(f %in% tools:::.get_internal_S3_generics())
	    .BaseNamespaceEnv
	else {
	    genfun <- get(f, mode="function", envir = envir)
	    if(.isMethodsDispatchOn() && methods::is(genfun, "genericFunction"))
		## assumes the default method is the S3 generic function
		genfun <- methods::selectMethod(genfun, "ANY")
            .defenv_for_S3_registry(genfun)
	}
    S3Table <- get(".__S3MethodsTable__.", envir = defenv)# climb search()
    if(!is.null(m <- get0(method, envir = S3Table, inherits = FALSE)))
	m
    else if(optional)
	NULL
    else stop(gettextf("S3 method '%s' not found", method), domain = NA)
}

##' Much in parallel to getS3method(), isS3method() gives TRUE/FALSE, but not an error
isS3method <- function(method, f, class, envir = parent.frame())
{
    if(missing(method)) {
        method <- paste(f, class, sep=".")
    } else { # determine (f, class) from 'method'
	f.c <- strsplit(method, ".", fixed=TRUE)[[1]]
	nfc <- length(f.c)
	if(nfc < 2 || !is.character(f.c) || f.c[[1L]] == "")
	    return(FALSE) ## stop("Invalid 'method' specification; must be  \"<fun>.<class>\"")
	if(nfc == 2) {
	    f     <- f.c[[1L]]
	    class <- f.c[[2L]]
	} else { ## nfc > 2 : e.g., t.data.frame, is.na.data.frame
	    for(j in 2:nfc)
		if(isS3method(f     = paste(f.c[1:(j-1)], collapse="."),
			      class = paste(f.c[j: nfc ], collapse="."),
			      envir = envir))
		    return(TRUE)
	    return(FALSE)
	}
    }
    if(!any(f == getKnownS3generics())) { ## either a known generic or found in 'envir'
	if(!nzchar(f <- findGeneric(f, envir)))
            return(FALSE)
    }
    if(!is.null(m <- get0(method, envir = envir, mode = "function"))) {
	## know: f is a knownS3generic, and method m is a visible function
	pkg <- if(isNamespace(em <- environment(m))) environmentName(em)
	       else if(is.primitive(m)) "base" ## else NULL
	return(is.na(match(method, tools::nonS3methods(pkg)))) ## TRUE unless an exception
    }
    ## also look for registered method in namespaces
    defenv <-
	if(!is.na(w <- .knownS3Generics[f]))
	    asNamespace(w)
	else if(f %in% tools:::.get_internal_S3_generics())
	    .BaseNamespaceEnv
	else {
	    genfun <- get(f, mode="function", envir = envir)
	    if(.isMethodsDispatchOn() && methods::is(genfun, "genericFunction"))
		## assumes the default method is the S3 generic function
		genfun <- methods::selectMethod(genfun, "ANY")
            .defenv_for_S3_registry(genfun)
	}
    S3Table <- defenv[[".__S3MethodsTable__."]]
    ## return
    exists(method, envir = S3Table, inherits = FALSE)
}

isS3stdGeneric <- function(f) {
    bdexpr <- body(if(methods::is(f, "traceable")) f@original else f)
    ## protect against technically valid but bizarre
    ## function(x) { { { UseMethod("gen")}}} by
    ## repeatedly consuming the { until we get to the first non { expr
    while(is.call(bdexpr) && bdexpr[[1L]] == quote(`{`))
        bdexpr <- bdexpr[[2L]]

    ## We only check if it is a "standard" s3 generic. i.e. the first non-{
    ## expression is a call to UseMethod. This will return FALSE if any
    ## work occurs before the UseMethod call ("non-standard" S3 generic)
    ret <- is.call(bdexpr) && bdexpr[[1L]] == quote(UseMethod)
    if(ret)
        names(ret) <- bdexpr[[2L]] ## arg passed to UseMethod naming generic
    ret
}

getFromNamespace <-
function(x, ns, pos = -1, envir = as.environment(pos))
{
    if(missing(ns)) {
        nm <- attr(envir, "name", exact = TRUE)
        if(is.null(nm) || !startsWith(nm, "package:"))
            stop("environment specified is not a package")
        ns <- asNamespace(substring(nm, 9L))
    } else ns <- asNamespace(ns)
    get(x, envir = ns, inherits = FALSE)
}

assignInMyNamespace <-
function(x, value)
{
    f <- sys.function(-1)
    ns <- environment(f)
    ## deal with subclasses of "function"
    ## that may insert an environment in front of the namespace
    if(isS4(f))
        while(!isNamespace(ns))
            ns <- parent.env(ns)
    if(bindingIsLocked(x, ns)) {
        unlockBinding(x, ns)
        assign(x, value, envir = ns, inherits = FALSE)
        w <- options("warn")
        on.exit(options(w))
        options(warn = -1)
        lockBinding(x, ns)
    } else assign(x, value, envir = ns, inherits = FALSE)
    if(!isBaseNamespace(ns)) {
        ## now look for possible copy as a registered S3 method
        S3 <- getNamespaceInfo(ns, "S3methods")
        if(!length(S3)) return(invisible(NULL))
        S3names <- S3[, 3L]
        if(x %in% S3names) {
            i <- match(x, S3names)
            genfun <- get(S3[i, 1L], mode = "function", envir = ns)
            if(.isMethodsDispatchOn() && methods::is(genfun, "genericFunction"))
                genfun <- methods::slot(genfun, "default")@methods$ANY
            defenv <- .defenv_for_S3_registry(genfun)
            S3Table <- defenv[[".__S3MethodsTable__."]]
            remappedName <- paste(S3[i, 1L], S3[i, 2L], sep = ".")
            if(exists(remappedName, envir = S3Table, inherits = FALSE))
                assign(remappedName, value, S3Table)
        }
    }
    invisible(NULL)
}

assignInNamespace <-
function(x, value, ns, pos = -1, envir = as.environment(pos))
{
    nf <- sys.nframe()
    if(missing(ns)) {
        nm <- attr(envir, "name", exact = TRUE)
        if(is.null(nm) || !startsWith(nm, "package:"))
            stop("environment specified is not a package")
        ns <- asNamespace(substring(nm, 9L))
    } else ns <- asNamespace(ns)
    ns_name <- getNamespaceName(ns)
    if (nf > 1L && !identical(sys.function(1), fixInNamespace)) {
        if(ns_name %in% tools:::.get_standard_package_names()$base)
            stop("locked binding of ", sQuote(x), " cannot be changed",
                 domain = NA)
    }
    if(bindingIsLocked(x, ns)) {
        in_load <- Sys.getenv("_R_NS_LOAD_")
        if (nzchar(in_load)) {
            if(in_load != ns_name) {
                msg <-
                    gettextf("changing locked binding for %s in %s whilst loading %s",
                             sQuote(x), sQuote(ns_name), sQuote(in_load))
                if (! in_load %in% c("Matrix", "SparseM"))
                    warning(msg, call. = FALSE, domain = NA, immediate. = TRUE)
            }
        } else if (nzchar(Sys.getenv("_R_WARN_ON_LOCKED_BINDINGS_"))) {
            warning(gettextf("changing locked binding for %s in %s",
                             sQuote(x), sQuote(ns_name)),
                    call. = FALSE, domain = NA, immediate. = TRUE)
        }
        unlockBinding(x, ns)
        assign(x, value, envir = ns, inherits = FALSE)
        w <- options("warn")
        on.exit(options(w))
        options(warn = -1)
        lockBinding(x, ns)
    } else {
        assign(x, value, envir = ns, inherits = FALSE)
    }
    if(!isBaseNamespace(ns)) {
        ## now look for possible copy as a registered S3 method
	S3 <- .getNamespaceInfo(ns, "S3methods")
        if(!length(S3)) return(invisible(NULL))
        S3names <- S3[, 3L]
        if(x %in% S3names) {
            i <- match(x, S3names)
            genfun <- get(S3[i, 1L], mode = "function", envir = ns)
            if(.isMethodsDispatchOn() && methods::is(genfun, "genericFunction"))
                genfun <- methods::slot(genfun, "default")@methods$ANY
            defenv <- .defenv_for_S3_registry(genfun)
            S3Table <- defenv[[".__S3MethodsTable__."]]
            remappedName <- paste(S3[i, 1L], S3[i, 2L], sep = ".")
            if(exists(remappedName, envir = S3Table, inherits = FALSE))
                assign(remappedName, value, S3Table)
        }
    }
    invisible(NULL)
}

fixInNamespace <-
function(x, ns, pos = -1, envir = as.environment(pos), ...)
{
    subx <- substitute(x)
    if (is.name(subx))
        subx <- deparse(subx)
    if (!is.character(subx) || length(subx) != 1L)
        stop("'fixInNamespace' requires a name")
    if(missing(ns)) {
        nm <- attr(envir, "name", exact = TRUE)
        if(is.null(nm) || !startsWith(nm, "package:"))
            stop("environment specified is not a package")
        ns <- asNamespace(substring(nm, 9L))
    } else ns <- asNamespace(ns)
    x <- edit(get(subx, envir = ns, inherits = FALSE), ...)
    assignInNamespace(subx, x, ns)
}

getAnywhere <-
function(x)
{
    if(tryCatch(!is.character(x), error = function(e) TRUE))
        x <- as.character(substitute(x))
    objs <- list(); where <- character(); visible <- logical()
    ## first look on search path
    if(length(pos <- find(x, numeric = TRUE))) {
        objs <- lapply(pos, function(pos, x) get(x, pos=pos), x=x)
        where <- names(pos)
        visible <- rep.int(TRUE, length(pos))
    }
    ## next look for methods: a.b.c.d could be a method for a or a.b or a.b.c
    if(length(grep(".", x, fixed=TRUE))) {
        np <- length(parts <- strsplit(x, ".", fixed=TRUE)[[1L]])
        for(i in 2:np) {
            gen <- paste(parts[1L:(i-1)], collapse=".")
            cl <- paste(parts[i:np], collapse=".")
            if (gen == "" || cl == "") next
            ## want to evaluate this in the parent, or the utils namespace
            ## gets priority.
            Call <- substitute(getS3method(gen, cl, TRUE), list(gen = gen, cl = cl))
            f <- eval.parent(Call)
            ## Now try to fathom out where it is from.
            ## f might be a special, not a closure, and not have an environment,
            if(!is.null(f) && !is.null(environment(f))) {
                ev <- topenv(environment(f), baseenv())
                nmev <- if(isNamespace(ev)) getNamespaceName(ev) else NULL
		objs <- c(objs, list(f))
                msg <- paste("registered S3 method for", gen)
                if(!is.null(nmev))
                    msg <- paste(msg, "from namespace", nmev)
                where <- c(where, msg)
                visible <- c(visible, FALSE)
            }
        }
    }
    ## now look in loaded namespaces
    for(i in loadedNamespaces()) {
        ns <- asNamespace(i)
        if(exists(x, envir = ns, inherits = FALSE)) {
            f <- get(x, envir = ns, inherits = FALSE)
	    objs <- c(objs, list(f))
            where <- c(where, paste0("namespace:", i))
            visible <- c(visible, FALSE)
        }
    }
    # now check for duplicates
    ln <- length(objs)
    dups <- rep.int(FALSE, ln)
    if(ln > 1L)
        for(i in 2L:ln)
            for(j in 1L:(i-1L))
                if(identical(objs[[i]], objs[[j]],
                             ignore.environment = TRUE)) {
                    dups[i] <- TRUE
                    break
                }
    structure(list(name=x, objs=objs, where=where, visible=visible, dups=dups),
              class = "getAnywhere")
}

print.getAnywhere <-
function(x, ...)
{
    n <- sum(!x$dups)
    if(n == 0L) {
        cat("no object named", sQuote(x$name), "was found\n")
    } else if (n == 1L) {
        cat("A single object matching", sQuote(x$name), "was found\n")
        cat("It was found in the following places\n")
	cat(paste0("  ", x$where), sep="\n")
        cat("with value\n\n")
        print(x$objs[[1L]])
    } else {
        cat(n, "differing objects matching", sQuote(x$name),
            "were found\n")
        cat("in the following places\n")
        cat(paste0("  ", x$where), sep="\n")
        cat("Use [] to view one of them\n")
    }
    invisible(x)
}

`[.getAnywhere` <-
function(x, i)
{
    if(!is.numeric(i)) stop("only numeric indices can be used")
    if(length(i) == 1L) x$objs[[i]]
    else x$objs[i]
}

argsAnywhere <-
function(x)
{
    if(tryCatch(!is.character(x), error = function(e) TRUE))
        x <- as.character(substitute(x))
    fs <- getAnywhere(x)
    if (sum(!fs$dups) == 0L)
        return(NULL)
    if (sum(!fs$dups) > 1L)
        sapply(fs$objs[!fs$dups],
               function(f) if (is.function(f)) args(f))
    else args(fs$objs[[1L]])
}

.defenv_for_S3_registry <-
function(genfun)
{
    if (typeof(genfun) == "closure") {
        topenv(environment(genfun))
    }
    else .BaseNamespaceEnv
}
